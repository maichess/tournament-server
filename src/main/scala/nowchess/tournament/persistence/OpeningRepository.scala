package nowchess.tournament.persistence

import zio.*
import nowchess.tournament.domain.opening.Opening

trait OpeningRepository:
  def save(opening: Opening): Task[Unit]
  def get(key: String): Task[Option[Opening]]
  def list: Task[Vector[Opening]]

object OpeningRepository:
  def save(opening: Opening): ZIO[OpeningRepository, Throwable, Unit] =
    ZIO.serviceWithZIO(_.save(opening))
  def get(key: String): ZIO[OpeningRepository, Throwable, Option[Opening]] =
    ZIO.serviceWithZIO(_.get(key))
  def list: ZIO[OpeningRepository, Throwable, Vector[Opening]] =
    ZIO.serviceWithZIO(_.list)
