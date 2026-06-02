package nowchess.tournament.persistence

import zio.*

final case class Identity(id: String, name: String, isBot: Boolean)

trait IdentityRepository:
  def save(identity: Identity): Task[Unit]
  def get(id: String): Task[Option[Identity]]
  def findByName(name: String, isBot: Boolean): Task[Option[Identity]]

object IdentityRepository:
  def save(identity: Identity): ZIO[IdentityRepository, Throwable, Unit] =
    ZIO.serviceWithZIO(_.save(identity))

  def get(id: String): ZIO[IdentityRepository, Throwable, Option[Identity]] =
    ZIO.serviceWithZIO(_.get(id))

  def findByName(name: String, isBot: Boolean): ZIO[IdentityRepository, Throwable, Option[Identity]] =
    ZIO.serviceWithZIO(_.findByName(name, isBot))
