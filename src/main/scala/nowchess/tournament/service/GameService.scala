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
import java.time.Instant

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
      move <- ZIO.fromEither(ChessRules.parseUci(uci)).mapError(e => DomainError.BadRequest(e))
      board <- ZIO.fromEither(ChessRules.parseFen(game.fen)).mapError(e => DomainError.BadRequest(e))
      newBoard <- ZIO.fromEither(ChessRules.applyMove(board, move)).mapError(e => DomainError.BadRequest(e))
      newFen = ChessRules.boardToFen(newBoard)
      (newStatus, winner) = determineStatus(newBoard)
      now = Instant.now()
      updatedGame = game.copy(
        moves = game.moves :+ uci,
        fen = newFen,
        turn = game.turn.opposite,
        status = newStatus,
        winner = winner,
        endedAt = if newStatus.isTerminal then Some(now) else None,
      )
      _ <- gameRepo.save(updatedGame)
      _ <- publishMoveEvent(gameId, uci, newFen, updatedGame)
      _ <- ZIO.when(newStatus.isTerminal)(handleGameEnd(updatedGame))
    yield updatedGame

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
      yield GameServiceLive(gameRepo, tournamentRepo, stream)
