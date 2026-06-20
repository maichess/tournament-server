package tournament.persistence

import zio.*
import tournament.domain.model.{BotId, RegisteredBot}

trait BotRegistryRepository:
  def save(bot: RegisteredBot): Task[Unit]
  def get(id: BotId): Task[Option[RegisteredBot]]
  def list: Task[Vector[RegisteredBot]]
  def delete(id: BotId): Task[Unit]

object BotRegistryRepository:
  def save(bot: RegisteredBot): ZIO[BotRegistryRepository, Throwable, Unit] =
    ZIO.serviceWithZIO(_.save(bot))
  def get(id: BotId): ZIO[BotRegistryRepository, Throwable, Option[RegisteredBot]] =
    ZIO.serviceWithZIO(_.get(id))
  def list: ZIO[BotRegistryRepository, Throwable, Vector[RegisteredBot]] =
    ZIO.serviceWithZIO(_.list)
  def delete(id: BotId): ZIO[BotRegistryRepository, Throwable, Unit] =
    ZIO.serviceWithZIO(_.delete(id))
