package nowchess.tournament.persistence

import zio.*
import nowchess.tournament.domain.model.{GameId, TournamentId}
import nowchess.tournament.domain.game.Game

trait GameRepository:
  def get(id: GameId): Task[Option[Game]]
  def save(game: Game): Task[Unit]
  def findByTournament(tournamentId: TournamentId): Task[Vector[Game]]

object GameRepository:
  def get(id: GameId): ZIO[GameRepository, Throwable, Option[Game]] =
    ZIO.serviceWithZIO(_.get(id))
  def save(game: Game): ZIO[GameRepository, Throwable, Unit] =
    ZIO.serviceWithZIO(_.save(game))
  def findByTournament(tournamentId: TournamentId): ZIO[GameRepository, Throwable, Vector[Game]] =
    ZIO.serviceWithZIO(_.findByTournament(tournamentId))
