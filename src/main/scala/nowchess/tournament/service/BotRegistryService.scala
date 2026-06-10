package nowchess.tournament.service

import zio.*
import nowchess.tournament.domain.model.{BotId, RegisteredBot}
import nowchess.tournament.domain.error.DomainError
import nowchess.tournament.persistence.BotRegistryRepository

trait BotRegistryService:
  def register(name: String, endpoint: Option[String]): Task[RegisteredBot]
  def list: Task[Vector[RegisteredBot]]
  def get(id: BotId): Task[Option[RegisteredBot]]
  def delete(id: BotId): Task[Unit]

object BotRegistryService:
  def register(name: String, endpoint: Option[String]): ZIO[BotRegistryService, Throwable, RegisteredBot] =
    ZIO.serviceWithZIO(_.register(name, endpoint))
  def list: ZIO[BotRegistryService, Throwable, Vector[RegisteredBot]] =
    ZIO.serviceWithZIO(_.list)
  def get(id: BotId): ZIO[BotRegistryService, Throwable, Option[RegisteredBot]] =
    ZIO.serviceWithZIO(_.get(id))
  def delete(id: BotId): ZIO[BotRegistryService, Throwable, Unit] =
    ZIO.serviceWithZIO(_.delete(id))

final class BotRegistryServiceLive(repo: BotRegistryRepository) extends BotRegistryService:

  override def register(name: String, endpoint: Option[String]): Task[RegisteredBot] =
    for
      _  <- ZIO.when(name.isBlank)(ZIO.fail(DomainError.BadRequest("name must not be blank")))
      id  = BotId(java.util.UUID.randomUUID().toString.take(8))
      bot = RegisteredBot(id, name.trim, endpoint.map(_.trim).filter(_.nonEmpty))
      _  <- repo.save(bot)
    yield bot

  override def list: Task[Vector[RegisteredBot]] =
    repo.list

  override def get(id: BotId): Task[Option[RegisteredBot]] =
    repo.get(id)

  override def delete(id: BotId): Task[Unit] =
    repo.delete(id)

object BotRegistryServiceLive:
  val layer: URLayer[BotRegistryRepository, BotRegistryService] =
    ZLayer:
      ZIO.service[BotRegistryRepository].map(new BotRegistryServiceLive(_))
