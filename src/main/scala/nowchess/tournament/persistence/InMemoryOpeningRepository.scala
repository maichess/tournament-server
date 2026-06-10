package nowchess.tournament.persistence

import zio.*
import nowchess.tournament.domain.opening.Opening

final class InMemoryOpeningRepository(ref: Ref[Map[String, Opening]]) extends OpeningRepository:

  override def save(opening: Opening): Task[Unit] =
    ref.update(_.updated(opening.key, opening))

  override def get(key: String): Task[Option[Opening]] =
    ref.get.map(_.get(key))

  override def list: Task[Vector[Opening]] =
    ref.get.map(_.values.toVector)

object InMemoryOpeningRepository:
  val layer: ULayer[OpeningRepository] =
    ZLayer:
      Ref.make(Map.empty[String, Opening]).map(new InMemoryOpeningRepository(_))
