package tournament.persistence

import zio.*

final class InMemoryIdentityRepository(ref: Ref[Map[String, Identity]]) extends IdentityRepository:

  override def save(identity: Identity): Task[Unit] =
    ref.update(_.updated(identity.id, identity))

  override def get(id: String): Task[Option[Identity]] =
    ref.get.map(_.get(id))

  override def findByName(name: String, isBot: Boolean): Task[Option[Identity]] =
    ref.get.map(_.values.find(i => i.name == name && i.isBot == isBot))

object InMemoryIdentityRepository:
  val layer: ULayer[IdentityRepository] =
    ZLayer:
      Ref.make(Map.empty[String, Identity]).map(new InMemoryIdentityRepository(_))
