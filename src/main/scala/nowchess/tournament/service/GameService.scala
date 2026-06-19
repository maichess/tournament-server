package nowchess.tournament.service

import zio.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.game.*
import nowchess.tournament.domain.round.*
import nowchess.tournament.domain.tournament.*
import nowchess.tournament.domain.event.{TournamentEvent, GameEvent}
import nowchess.tournament.domain.error.DomainError
import nowchess.tournament.domain.lifecycle.TournamentLifecycle
import nowchess.tournament.domain.standing.ScoringRules
import nowchess.tournament.domain.pairing.*
import nowchess.tournament.persistence.{TournamentRepository, GameRepository}

trait GameService:
  def getGame(gameId: GameId): Task[Game]
  def makeMove(gameId: GameId, uci: String, botId: BotId): Task[Game]

object GameService:
  def getGame(gameId: GameId): ZIO[GameService, Throwable, Game] =
    ZIO.serviceWithZIO(_.getGame(gameId))
  def makeMove(gameId: GameId, uci: String, botId: BotId): ZIO[GameService, Throwable, Game] =
    ZIO.serviceWithZIO(_.makeMove(gameId, uci, botId))

final class GameServiceLive(
  gameRepo: GameRepository,
  tournamentRepo: TournamentRepository,
  streamService: StreamService,
) extends GameService:

  override def getGame(gameId: GameId): Task[Game] =
    gameRepo.get(gameId).flatMap:
      case Some(g) => ZIO.succeed(g)
      case None    => ZIO.fail(DomainError.NotFound("game not found"))

  override def makeMove(gameId: GameId, uci: String, botId: BotId): Task[Game] =
    for
      game <- getGame(gameId)
      _    <- ZIO.when(game.status.isTerminal)(ZIO.fail(DomainError.Conflict("game already finished")))
      _    <- ZIO.when(game.status == GameStatus.Pending)(ZIO.fail(DomainError.Conflict("game not active")))
      _    <- ZIO.when(game.currentPlayer.id != botId)(ZIO.fail(DomainError.Forbidden("not your turn")))
      now  <- zio.Clock.instant
      move <- ZIO.fromEither(ChessRules.parseUci(uci)).mapError(e => DomainError.BadRequest(e))
      board <- ZIO.fromEither(ChessRules.parseFen(game.fen)).mapError(e => DomainError.BadRequest(e))
      newBoard <- ZIO.fromEither(ChessRules.applyMove(board, move)).mapError(e => DomainError.BadRequest(e))
      newFen = ChessRules.boardToFen(newBoard)
      (newStatus, winner) = determineStatus(newBoard)
      newClock = updateClock(game, now)
      (finalStatus, finalWinner) = if newClock.timeForTurn(game.turn) <= 0 then
        (GameStatus.Timeout, Some(game.turn.opposite))
      else (newStatus, winner)
      updatedGame = game.copy(
        moves = game.moves :+ uci,
        fen = newFen,
        turn = game.turn.opposite,
        status = finalStatus,
        winner = finalWinner,
        clock = newClock,
        lastMoveAt = now,
      )
      _ <- gameRepo.save(updatedGame)
      _ <- publishMoveEvent(gameId, uci, newFen, updatedGame)
      _ <- ZIO.when(finalStatus.isTerminal)(handleGameEnd(updatedGame))
    yield updatedGame

  private def updateClock(game: Game, now: java.time.Instant): GameClock =
    val elapsed = if game.lastMoveAt == java.time.Instant.EPOCH then 0.0
      else java.time.Duration.between(game.lastMoveAt, now).toMillis / 1000.0
    val inc = game.clock.increment.toDouble
    game.turn match
      case Color.White =>
        game.clock.copy(whiteTime = math.max(0, game.clock.whiteTime - elapsed + inc))
      case Color.Black =>
        game.clock.copy(blackTime = math.max(0, game.clock.blackTime - elapsed + inc))

  def startTimeoutDaemon: UIO[Fiber.Runtime[Throwable, Nothing]] =
    val loop = for
      tournamentsMap <- tournamentRepo.listByStatus.orDie
      startedTournaments = tournamentsMap.getOrElse(TournamentStatus.Started, Vector.empty)
      _ <- ZIO.foreachDiscard(startedTournaments): tournament =>
        for
          games <- gameRepo.findByTournament(tournament.id).orDie
          ongoingGames = games.filter(_.status == GameStatus.Ongoing)
          now <- zio.Clock.instant
          _ <- ZIO.foreachDiscard(ongoingGames): game =>
            val elapsed = if game.lastMoveAt == java.time.Instant.EPOCH then 0.0
              else java.time.Duration.between(game.lastMoveAt, now).toMillis / 1000.0
            val remaining = game.clock.timeForTurn(game.turn) - elapsed
            ZIO.when(remaining <= 0)(timeoutGame(game).catchAll(e => ZIO.logError(s"Failed to timeout game ${game.id}: $e")))
        yield ()
    yield ()
    (loop *> ZIO.sleep(1.second)).forever.forkDaemon

  private def timeoutGame(game: Game): Task[Unit] =
    val loser = game.turn
    val winner = loser.opposite
    val updatedClock = loser match
      case Color.White => game.clock.copy(whiteTime = 0)
      case Color.Black => game.clock.copy(blackTime = 0)
    val updatedGame = game.copy(
      status = GameStatus.Timeout,
      winner = Some(winner),
      clock = updatedClock,
    )
    gameRepo.save(updatedGame) *> handleGameEnd(updatedGame)

  private def determineStatus(board: ChessRules.Board): (GameStatus, Option[Color]) =
    if ChessRules.isCheckmate(board) then
      (GameStatus.Checkmate, Some(board.turn.opposite))
    else if ChessRules.isStalemate(board) then
      (GameStatus.Stalemate, None)
    else if ChessRules.isDraw(board) then
      (GameStatus.Draw, None)
    else
      (GameStatus.Ongoing, None)

  private def publishMoveEvent(gameId: GameId, uci: String, fen: String, game: Game): UIO[Unit] =
    streamService.publishGame(gameId, GameEvent.MovePlayed(uci, fen, game.turn, game.clock))

  private def handleGameEnd(game: Game): Task[Unit] =
    streamService.publishGame(game.id, GameEvent.GameEnd(game.winner, game.status)) *>
      game.toOutcome.fold(ZIO.unit)(o => updateTournamentAfterGame(game.tournamentId, game.id, o, game.movesUci))

  private def updateTournamentAfterGame(
    tournamentId: TournamentId,
    gameId: GameId,
    outcome: GameOutcome,
    moves: String,
  ): Task[Unit] =
    tournamentRepo.get(tournamentId).flatMap:
      case None => ZIO.fail(DomainError.NotFound("tournament not found"))
      case Some(tournament) =>
        tournament.rounds.find(_.number == tournament.currentRound) match
          case None => ZIO.fail(DomainError.NotFound("current round not found"))
          case Some(currentRound) =>
            val updatedPairings = currentRound.pairings.map: p =>
              if p.matches.exists(_.gameId == gameId) then
                p.recordResult(gameId, outcome, moves, tournament.config.matchesPerPairing)
              else p
            val updatedRound = currentRound.copy(pairings = updatedPairings)
            ZIO.fromEither(TournamentLifecycle.updateRound(tournament, tournament.currentRound, updatedRound)).flatMap: updated =>
              tournamentRepo.save(updated) *>
                (if updatedRound.isComplete(tournament.config.matchesPerPairing) then handleRoundComplete(updated)
                 else GameActivation.activate(updated, updatedRound, gameRepo, streamService))

  private def handleRoundComplete(tournament: Tournament): Task[Unit] =
    streamService.publishTournament(tournament.id, TournamentEvent.RoundFinished(tournament.currentRound)) *>
      (if tournament.currentRound >= tournament.config.nbRounds then finishTournament(tournament)
       else startNextRound(tournament))

  private def finishTournament(tournament: Tournament): Task[Unit] =
    val standings = ScoringRules.computeStandings(tournament)
    val winner = standings.head.bot
    ZIO.fromEither(TournamentLifecycle.finish(tournament, winner)).flatMap: finished =>
      tournamentRepo.save(finished) *>
        streamService.publishTournament(tournament.id, TournamentEvent.TournamentFinished(winner))

  private def startNextRound(tournament: Tournament): Task[Unit] =
    ZIO.fromEither(TournamentLifecycle.advanceRound(tournament)).flatMap: advanced =>
      val standings = ScoringRules.computeStandings(advanced)
      val algorithm = selectAlgorithm(advanced)
      val roundNum = advanced.currentRound
      val pairings = algorithm.pair(advanced.participants, standings, advanced.rounds, roundNum)
      ZIO.foreach(pairings)(createGamesForPairing(advanced, roundNum, _)).flatMap: games =>
        val roundPairings = games.map((pair, matches) =>
          Pairing(pair._1, pair._2, matches, None))
        val round = Round(roundNum, roundPairings)
        ZIO.fromEither(TournamentLifecycle.addRound(advanced, round)).flatMap: withRound =>
          tournamentRepo.save(withRound) *>
            streamService.publishTournament(tournament.id, TournamentEvent.RoundStarted(roundNum)) *>
            GameActivation.activate(withRound, round, gameRepo, streamService)

  private def selectAlgorithm(tournament: Tournament): PairingAlgorithm =
    tournament.config.format match
      case TournamentFormat.Swiss             => SwissPairing
      case TournamentFormat.SingleElimination => EliminationBracket
      case TournamentFormat.DoubleElimination => EliminationBracket
      case TournamentFormat.GroupStage(_)     => GroupStagePairing
      case TournamentFormat.League            => RoundRobinPairing
      case TournamentFormat.RandomKnockout    => RandomKnockoutPairing(tournament.seed)

  private def createGamesForPairing(
    tournament: Tournament,
    roundNum: Int,
    pair: (BotRef, BotRef),
  ): Task[((BotRef, BotRef), Vector[Match])] =
    GameFactory.create(tournament, roundNum, pair, gameRepo).map(matches => (pair, matches))

object GameServiceLive:
  val layer: URLayer[GameRepository & TournamentRepository & StreamService, GameService] =
    ZLayer:
      for
        gameRepo <- ZIO.service[GameRepository]
        tournamentRepo <- ZIO.service[TournamentRepository]
        stream <- ZIO.service[StreamService]
        service = GameServiceLive(gameRepo, tournamentRepo, stream)
        _ <- service.startTimeoutDaemon
      yield service
