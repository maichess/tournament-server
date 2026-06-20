package nowchess.tournament.persistence

import zio.*
import nowchess.tournament.domain.model.{GameId, TournamentId}
import nowchess.tournament.domain.game.Game

trait GameRepository:
  def get(id: GameId): Task[Option[Game]]
  def save(game: Game): Task[Unit]
  def findByTournament(tournamentId: TournamentId): Task[Vector[Game]]

  /** Atomically reads the game, applies `f`, and persists the result iff `f`
    * returns `Some`. Returns the stored game when applied, otherwise `None`
    * (game missing, or `f` declined). Lets callers do compare-and-set without a
    * read/write race. */
  def modifyIf(id: GameId)(f: Game => Option[Game]): Task[Option[Game]]

object GameRepository:
  def get(id: GameId): ZIO[GameRepository, Throwable, Option[Game]] =
    ZIO.serviceWithZIO(_.get(id))
  def save(game: Game): ZIO[GameRepository, Throwable, Unit] =
    ZIO.serviceWithZIO(_.save(game))
  def findByTournament(tournamentId: TournamentId): ZIO[GameRepository, Throwable, Vector[Game]] =
    ZIO.serviceWithZIO(_.findByTournament(tournamentId))
  def modifyIf(id: GameId)(f: Game => Option[Game]): ZIO[GameRepository, Throwable, Option[Game]] =
    ZIO.serviceWithZIO(_.modifyIf(id)(f))
