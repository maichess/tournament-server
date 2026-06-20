package nowchess.tournament.service

import zio.*
import nowchess.tournament.domain.model.{BotId, RegisteredBot}
import nowchess.tournament.domain.error.DomainError
import nowchess.tournament.persistence.BotRegistryRepository

trait BotRegistryService:
  def register(
    name: String,
    endpoint: Option[String],
    family: Option[String] = None,
    strategyType: Option[String] = None,
    engineType: Option[String] = None,
    modelVersion: Option[String] = None,
  ): Task[RegisteredBot]
  def list: Task[Vector[RegisteredBot]]
  def get(id: BotId): Task[Option[RegisteredBot]]
  def delete(id: BotId): Task[Unit]

object BotRegistryService:
  def register(
    name: String,
    endpoint: Option[String],
    family: Option[String] = None,
    strategyType: Option[String] = None,
    engineType: Option[String] = None,
    modelVersion: Option[String] = None,
  ): ZIO[BotRegistryService, Throwable, RegisteredBot] =
    ZIO.serviceWithZIO(_.register(name, endpoint, family, strategyType, engineType, modelVersion))
  def list: ZIO[BotRegistryService, Throwable, Vector[RegisteredBot]] =
    ZIO.serviceWithZIO(_.list)
  def get(id: BotId): ZIO[BotRegistryService, Throwable, Option[RegisteredBot]] =
    ZIO.serviceWithZIO(_.get(id))
  def delete(id: BotId): ZIO[BotRegistryService, Throwable, Unit] =
    ZIO.serviceWithZIO(_.delete(id))

final class BotRegistryServiceLive(repo: BotRegistryRepository, authService: AuthService) extends BotRegistryService:

  override def register(
    name: String,
    endpoint: Option[String],
    family: Option[String],
    strategyType: Option[String],
    engineType: Option[String],
    modelVersion: Option[String],
  ): Task[RegisteredBot] =
    for
      _        <- ZIO.when(name.isBlank)(ZIO.fail(DomainError.BadRequest("name must not be blank")))
      // Back the registry entry with an auth identity so its id lives in the same
      // space as JWT subjects. authService.register is idempotent: same name →
      // same id. We honour that contract by also making the registry write a
      // no-op for an already-registered bot, so metadata set at first registration
      // is never silently overwritten by a repeat call with fewer fields.
      identity <- authService.register(name.trim, isBot = true)
      existing <- repo.get(BotId(identity.id))
      bot      <- existing match
        case Some(b) => ZIO.succeed(b)
        case None    =>
          val newBot = RegisteredBot(
            BotId(identity.id),
            name.trim,
            endpoint.map(_.trim).filter(_.nonEmpty),
            family.map(_.trim).filter(_.nonEmpty),
            strategyType.map(_.trim).filter(_.nonEmpty),
            engineType.map(_.trim).filter(_.nonEmpty),
            modelVersion.map(_.trim).filter(_.nonEmpty),
          )
          repo.save(newBot).as(newBot)
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
