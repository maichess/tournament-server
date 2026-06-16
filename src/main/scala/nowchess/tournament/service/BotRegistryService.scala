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

final class BotRegistryServiceLive(repo: BotRegistryRepository, authService: AuthService) extends BotRegistryService:

  override def register(name: String, endpoint: Option[String]): Task[RegisteredBot] =
    for
      _  <- ZIO.when(name.isBlank)(ZIO.fail(DomainError.BadRequest("name must not be blank")))
      // Back the registry entry with a bot auth identity so its id lives in the
      // same space as JWT subjects. A client that can mint a token for this name
      // (idempotent /api/auth/register) can then drive the bot's moves after it
      // is added to a tournament via /participants. Registering the same name
      // again resolves to the same identity, so registration is idempotent.
      identity <- authService.register(name.trim, isBot = true)
      bot = RegisteredBot(BotId(identity.id), name.trim, endpoint.map(_.trim).filter(_.nonEmpty))
      _  <- repo.save(bot)
    yield bot

  override def list: Task[Vector[RegisteredBot]] =
    repo.list

  override def get(id: BotId): Task[Option[RegisteredBot]] =
    repo.get(id)

  override def delete(id: BotId): Task[Unit] =
    repo.delete(id)

object BotRegistryServiceLive:
  val layer: URLayer[BotRegistryRepository & AuthService, BotRegistryService] =
    ZLayer:
      for
        repo <- ZIO.service[BotRegistryRepository]
        auth <- ZIO.service[AuthService]
      yield BotRegistryServiceLive(repo, auth)
