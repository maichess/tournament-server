package tournament.service

import zio.*
import tournament.domain.opening.{Opening, OpeningCatalog}
import tournament.domain.error.DomainError
import tournament.persistence.OpeningRepository

trait OpeningService:
  /** Built-in catalog plus any custom registered openings. */
  def list: Task[Vector[Opening]]
  /** Register a custom named position; `keyOpt` defaults to a slug of `name`. */
  def register(name: String, fen: String, keyOpt: Option[String]): Task[Opening]
  /** Resolve an opening key to its FEN, catalog first then custom. */
  def resolve(key: String): Task[Option[String]]

object OpeningService:
  def list: ZIO[OpeningService, Throwable, Vector[Opening]] =
    ZIO.serviceWithZIO(_.list)
  def register(name: String, fen: String, keyOpt: Option[String]): ZIO[OpeningService, Throwable, Opening] =
    ZIO.serviceWithZIO(_.register(name, fen, keyOpt))
  def resolve(key: String): ZIO[OpeningService, Throwable, Option[String]] =
    ZIO.serviceWithZIO(_.resolve(key))

  /** lowerCamelCase slug derived from a display name. */
  def slug(name: String): String =
    val words = name.trim.split("[^A-Za-z0-9]+").filter(_.nonEmpty)
    words.zipWithIndex.map: (w, i) =>
      val lower = w.toLowerCase
      if i == 0 then lower else lower.capitalize
    .mkString

final class OpeningServiceLive(repo: OpeningRepository) extends OpeningService:

  override def list: Task[Vector[Opening]] =
    repo.list.map: custom =>
      OpeningCatalog.all ++ custom.filterNot(c => OpeningCatalog.byKey(c.key).isDefined)

  override def register(name: String, fen: String, keyOpt: Option[String]): Task[Opening] =
    for
      _   <- ZIO.when(name.isBlank)(ZIO.fail(DomainError.BadRequest("name must not be blank")))
      _   <- ZIO.when(fen.isBlank)(ZIO.fail(DomainError.BadRequest("fen must not be blank")))
      key  = keyOpt.map(_.trim).filter(_.nonEmpty).getOrElse(OpeningService.slug(name))
      _   <- ZIO.when(key.isEmpty)(ZIO.fail(DomainError.BadRequest("could not derive a key from name")))
      _   <- ZIO.when(OpeningCatalog.byKey(key).isDefined)(ZIO.fail(DomainError.Conflict(s"opening key '$key' is reserved")))
      existing <- repo.get(key)
      _   <- ZIO.when(existing.isDefined)(ZIO.fail(DomainError.Conflict(s"opening key '$key' already registered")))
      opening = Opening(key, name.trim, fen.trim)
      _   <- repo.save(opening)
    yield opening

  override def resolve(key: String): Task[Option[String]] =
    OpeningCatalog.byKey(key) match
      case Some(o) => ZIO.succeed(Some(o.fen))
      case None    => repo.get(key).map(_.map(_.fen))

object OpeningServiceLive:
  val layer: URLayer[OpeningRepository, OpeningService] =
    ZLayer:
      ZIO.service[OpeningRepository].map(new OpeningServiceLive(_))
