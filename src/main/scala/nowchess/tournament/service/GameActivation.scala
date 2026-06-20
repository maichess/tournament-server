package nowchess.tournament.service

import zio.*
import nowchess.tournament.domain.model.{TournamentId, Color}
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
      games       <- gameRepo.findByTournament(tournament.id)
      byId         = games.map(g => g.id -> g).toMap
      orderedIds   = round.pairings.flatMap(_.matches.map(_.gameId))
      ongoingCount = orderedIds.count(id => byId.get(id).exists(_.status == GameStatus.Ongoing))
      pending      = orderedIds.filter(id => byId.get(id).exists(_.status == GameStatus.Pending))
      toActivate   = GameScheduler.toActivate(ongoingCount, pending, tournament.config.maxConcurrentGames)
      _           <- ZIO.foreach(toActivate)(gid => activateOne(byId(gid), tournament.id, round.number, gameRepo, stream))
    yield ()

  private def activateOne(
    game: Game,
    tournamentId: TournamentId,
    round: Int,
    gameRepo: GameRepository,
    stream: StreamService,
  ): Task[Unit] =
    val activated = game.copy(status = GameStatus.Ongoing, startedAt = Some(Instant.now()))
    gameRepo.save(activated) *>
      stream.publishTournament(tournamentId, TournamentEvent.GameStart(round, game.id, Color.White)) *>
      stream.publishTournament(tournamentId, TournamentEvent.GameStart(round, game.id, Color.Black))
