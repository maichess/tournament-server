package nowchess.tournament.service

import zio.*
import nowchess.tournament.domain.model.{TournamentId, GameId, Color}
import nowchess.tournament.domain.game.{Game, GameStatus, GameScheduler}
import java.time.Instant
import nowchess.tournament.domain.round.Round
import nowchess.tournament.domain.tournament.Tournament
import nowchess.tournament.domain.event.TournamentEvent
import nowchess.tournament.persistence.GameRepository

/** Promotes `Pending` games of a round to `Ongoing`, respecting the tournament's
  * `maxConcurrentGames` cap, and announces each newly started game. Shared by
  * round start (TournamentService) and round progression / game completion
  * (GameService) so the concurrency policy is enforced in one place.
  */
object GameActivation:

  def activate(
    tournament: Tournament,
    round: Round,
    gameRepo: GameRepository,
    stream: StreamService,
  ): Task[Unit] =
    for
      now         <- zio.Clock.instant
      games       <- gameRepo.findByTournament(tournament.id)
      byId         = games.map(g => g.id -> g).toMap
      orderedIds   = round.pairings.flatMap(_.matches.map(_.gameId))
      ongoingCount = orderedIds.count(id => byId.get(id).exists(_.status == GameStatus.Ongoing))
      pending      = orderedIds.filter(id => byId.get(id).exists(_.status == GameStatus.Pending))
      toActivate   = GameScheduler.toActivate(ongoingCount, pending, tournament.config.maxConcurrentGames)
      _           <- ZIO.foreach(toActivate)(gid => activateOne(byId(gid), tournament.id, round.number, now, gameRepo, stream))
    yield ()

  private def activateOne(
    game: Game,
    tournamentId: TournamentId,
    round: Int,
    now: Instant,
    gameRepo: GameRepository,
    stream: StreamService,
  ): Task[Unit] =
    val activated = game.copy(status = GameStatus.Ongoing, startedAt = Some(now), lastMoveAt = now)
    gameRepo.save(activated) *>
      ZIO.foreachDiscard(gameStartEvents(round, game.id))(stream.publishTournament(tournamentId, _))

  /** The `gameStart` announcements for one game — one per colour, so the bot of
    * either side learns its game has begun. */
  def gameStartEvents(round: Int, gameId: GameId): Vector[TournamentEvent] =
    Vector(
      TournamentEvent.GameStart(round, gameId, Color.White),
      TournamentEvent.GameStart(round, gameId, Color.Black),
    )

  /** Catch-up announcements for a subscriber that joins the tournament stream
    * mid-round: the same `gameStart` events the live path emitted, for exactly
    * the games currently `Ongoing` in the current round (never the pending ones
    * still held back by the concurrency cap). */
  def activeGameStarts(tournament: Tournament, games: Vector[Game]): Vector[TournamentEvent] =
    tournament.rounds.find(_.number == tournament.currentRound) match
      case None => Vector.empty
      case Some(round) =>
        val ongoingIds = games.iterator.filter(_.status == GameStatus.Ongoing).map(_.id).toSet
        round.pairings.iterator
          .flatMap(_.matches.iterator.map(_.gameId))
          .filter(ongoingIds.contains)
          .flatMap(gid => gameStartEvents(tournament.currentRound, gid))
          .toVector
