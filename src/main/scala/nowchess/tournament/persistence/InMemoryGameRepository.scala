package nowchess.tournament.persistence

import zio.*
import nowchess.tournament.domain.model.{GameId, TournamentId}
import nowchess.tournament.domain.game.Game

final class InMemoryGameRepository(ref: Ref[Map[GameId, Game]]) extends GameRepository:

  override def get(id: GameId): Task[Option[Game]] =
    ref.get.map(_.get(id))

  override def save(game: Game): Task[Unit] =
    ref.update(_.updated(game.id, game))

  override def findByTournament(tournamentId: TournamentId): Task[Vector[Game]] =
    ref.get.map(_.values.filter(_.tournamentId == tournamentId).toVector)

object InMemoryGameRepository:
  val layer: ULayer[GameRepository] =
    ZLayer:
      Ref.make(Map.empty[GameId, Game]).map(new InMemoryGameRepository(_))
