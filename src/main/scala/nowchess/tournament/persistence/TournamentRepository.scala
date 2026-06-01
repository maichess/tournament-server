package nowchess.tournament.persistence

import zio.*
import nowchess.tournament.domain.model.TournamentId
import nowchess.tournament.domain.tournament.{Tournament, TournamentStatus}

trait TournamentRepository:
  def get(id: TournamentId): Task[Option[Tournament]]
  def save(tournament: Tournament): Task[Unit]
  def delete(id: TournamentId): Task[Unit]
  def listByStatus: Task[Map[TournamentStatus, Vector[Tournament]]]

object TournamentRepository:
  def get(id: TournamentId): ZIO[TournamentRepository, Throwable, Option[Tournament]] =
    ZIO.serviceWithZIO(_.get(id))
  def save(tournament: Tournament): ZIO[TournamentRepository, Throwable, Unit] =
    ZIO.serviceWithZIO(_.save(tournament))
  def delete(id: TournamentId): ZIO[TournamentRepository, Throwable, Unit] =
    ZIO.serviceWithZIO(_.delete(id))
  def listByStatus: ZIO[TournamentRepository, Throwable, Map[TournamentStatus, Vector[Tournament]]] =
    ZIO.serviceWithZIO(_.listByStatus)
