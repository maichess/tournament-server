package nowchess.tournament.persistence

import zio.*
import nowchess.tournament.domain.model.TournamentId
import nowchess.tournament.domain.tournament.{Tournament, TournamentStatus}

final class InMemoryTournamentRepository(ref: Ref[Map[TournamentId, Tournament]]) extends TournamentRepository:

  override def get(id: TournamentId): Task[Option[Tournament]] =
    ref.get.map(_.get(id))

  override def save(tournament: Tournament): Task[Unit] =
    ref.update(_.updated(tournament.id, tournament))

  override def delete(id: TournamentId): Task[Unit] =
    ref.update(_ - id)

  override def listByStatus: Task[Map[TournamentStatus, Vector[Tournament]]] =
    ref.get.map: m =>
      m.values.toVector.groupBy(_.status).withDefaultValue(Vector.empty)

object InMemoryTournamentRepository:
  val layer: ULayer[TournamentRepository] =
    ZLayer:
      Ref.make(Map.empty[TournamentId, Tournament]).map(new InMemoryTournamentRepository(_))
