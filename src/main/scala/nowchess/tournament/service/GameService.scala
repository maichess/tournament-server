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
      now  <- zio.Clock.instant
      move <- ZIO.fromEither(ChessRules.parseUci(uci)).mapError(e => DomainError.BadRequest(e))
      board <- ZIO.fromEither(ChessRules.parseFen(game.fen)).mapError(e => DomainError.BadRequest(e))
      newBoard <- ZIO.fromEither(ChessRules.applyMove(board, move)).mapError(e => DomainError.BadRequest(e))
      newFen = ChessRules.boardToFen(newBoard)
      (newStatus, winner) = determineStatus(newBoard)
      (newClock, flagged) = ClockRules.applyMove(game.clock, game.turn, game.lastMoveAt, now)
      (finalStatus, finalWinner) = if flagged then (GameStatus.Timeout, Some(game.turn.opposite))
        else (newStatus, winner)
      updatedGame = game.copy(
        moves = game.moves :+ uci,
        fen = newFen,
        turn = game.turn.opposite,
        status = finalStatus,
        winner = finalWinner,
        clock = newClock,
        lastMoveAt = now,
        endedAt = if finalStatus.isTerminal then Some(now) else None,
      )
      _ <- gameRepo.save(updatedGame)
      _ <- publishMoveEvent(gameId, uci, newFen, updatedGame)
      _ <- ZIO.when(finalStatus.isTerminal)(handleGameEnd(updatedGame))
    yield updatedGame

  /** Single pass of the timeout daemon: for every started tournament, end any
    * ongoing game whose clock has run out. The decision is re-evaluated inside
    * the atomic `modifyIf`, so a game a concurrent move has just finished (or
    * whose clock a move just refreshed) is left untouched and `handleGameEnd`
    * fires exactly once. */
  def timeoutSweep: Task[Unit] =
    zio.Clock.instant.flatMap: now =>
      tournamentRepo.listByStatus.flatMap: byStatus =>
        ZIO.foreachDiscard(byStatus.getOrElse(TournamentStatus.Started, Vector.empty))(t => sweepTournament(t.id, now))

  private def sweepTournament(id: TournamentId, now: Instant): Task[Unit] =
    gameRepo.findByTournament(id).flatMap: games =>
      ZIO.foreachDiscard(games.filter(_.status == GameStatus.Ongoing))(g => timeoutGame(g.id, now))

  private def timeoutGame(gameId: GameId, now: Instant): Task[Unit] =
    val timeout = (g: Game) =>
      if g.status == GameStatus.Ongoing && ClockRules.hasFlagged(g.clock, g.turn, g.lastMoveAt, now) then
        Some(g.copy(
          status = GameStatus.Timeout,
          winner = Some(g.turn.opposite),
          clock = g.clock.withTimeForTurn(g.turn, 0.0),
          endedAt = Some(now),
        ))
      else None
    gameRepo.modifyIf(gameId)(timeout).flatMap:
      case Some(timedOut) => handleGameEnd(timedOut)
      case None           => ZIO.unit

  /** One daemon iteration with failures swallowed (and logged) so a transient
    * repository error can never permanently kill the loop. */
  private[service] def timeoutTick: UIO[Unit] =
    timeoutSweep.catchAllCause(c => ZIO.logErrorCause("Timeout sweep failed", c))

  def startTimeoutDaemon: UIO[Fiber.Runtime[Nothing, Nothing]] =
    (timeoutTick *> ZIO.sleep(1.second)).forever.forkDaemon

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
            val alreadyComplete = currentRound.pairings
              .find(_.matches.exists(_.gameId == gameId))
              .exists(_.aggregateOutcome.isDefined)
            if alreadyComplete then ZIO.unit
            else
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
    // finishTournament only runs once a round has completed, so the tournament
    // has participants and computeStandings is never empty; the empty case is a
    // defensive guard excluded from coverage as it is unreachable in practice.
    ScoringRules.computeStandings(tournament).headOption match
      case Some(leader) =>
        zio.Clock.instant.flatMap: now =>
          ZIO.fromEither(TournamentLifecycle.finish(tournament, leader.bot, now)).flatMap: finished =>
            tournamentRepo.save(finished) *>
              streamService.publishTournament(tournament.id, TournamentEvent.TournamentFinished(leader.bot))
      // $COVERAGE-OFF$
      case None =>
        ZIO.fail(DomainError.Conflict("no participants to determine winner"))
      // $COVERAGE-ON$

  private def startNextRound(tournament: Tournament): Task[Unit] =
    ZIO.fromEither(TournamentLifecycle.advanceRound(tournament)).flatMap: advanced =>
      val standings = ScoringRules.computeStandings(advanced)
      val algorithm = GameFactory.selectAlgorithm(advanced)
      val roundNum = advanced.currentRound
      val pairings = algorithm.pair(advanced.participants, standings, advanced.rounds, roundNum)
      ZIO.foreach(pairings)(GameFactory.createForPairing(advanced, roundNum, _, gameRepo)).flatMap: games =>
        val roundPairings = games.map((pair, matches) =>
          Pairing(pair._1, pair._2, matches, None))
        val round = Round(roundNum, roundPairings)
        ZIO.fromEither(TournamentLifecycle.addRound(advanced, round)).flatMap: withRound =>
          tournamentRepo.save(withRound) *>
            streamService.publishTournament(tournament.id, TournamentEvent.RoundStarted(roundNum)) *>
            GameActivation.activate(withRound, round, gameRepo, streamService)

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
