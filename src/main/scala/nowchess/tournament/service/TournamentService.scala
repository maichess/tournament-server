package nowchess.tournament.service

import zio.*
import nowchess.tournament.domain.model.{TournamentId, BotId, UserId, BotRef, GameId, Color, GameOutcome, StartPosition, Clock as DomainClock}
import nowchess.tournament.domain.tournament.*
import nowchess.tournament.domain.round.*
import nowchess.tournament.domain.game.*
import nowchess.tournament.domain.error.DomainError
import nowchess.tournament.domain.event.TournamentEvent
import nowchess.tournament.domain.lifecycle.TournamentLifecycle
import nowchess.tournament.domain.standing.ScoringRules
import nowchess.tournament.domain.pairing.*
import nowchess.tournament.persistence.{TournamentRepository, GameRepository}
import java.time.Instant

final case class CreateTournamentForm(
  name: String,
  nbRounds: Int,
  clockLimit: Int,
  clockIncrement: Int,
  rated: Boolean,
  format: TournamentFormat,
  startPosition: StartPosition,
  matchesPerPairing: Int,
  groupSize: Option[Int],
)

trait TournamentService:
  def create(form: CreateTournamentForm, director: UserId): Task[Tournament]
  def get(id: TournamentId): Task[Tournament]
  def list: Task[Map[TournamentStatus, Vector[Tournament]]]
  def delete(id: TournamentId, director: UserId): Task[Unit]
  def start(id: TournamentId, director: UserId): Task[Tournament]
  def join(id: TournamentId, bot: BotRef): Task[Unit]
  def withdraw(id: TournamentId, botId: BotId): Task[Unit]

object TournamentService:
  def create(form: CreateTournamentForm, director: UserId): ZIO[TournamentService, Throwable, Tournament] =
    ZIO.serviceWithZIO(_.create(form, director))
  def get(id: TournamentId): ZIO[TournamentService, Throwable, Tournament] =
    ZIO.serviceWithZIO(_.get(id))
  def list: ZIO[TournamentService, Throwable, Map[TournamentStatus, Vector[Tournament]]] =
    ZIO.serviceWithZIO(_.list)
  def delete(id: TournamentId, director: UserId): ZIO[TournamentService, Throwable, Unit] =
    ZIO.serviceWithZIO(_.delete(id, director))
  def start(id: TournamentId, director: UserId): ZIO[TournamentService, Throwable, Tournament] =
    ZIO.serviceWithZIO(_.start(id, director))
  def join(id: TournamentId, bot: BotRef): ZIO[TournamentService, Throwable, Unit] =
    ZIO.serviceWithZIO(_.join(id, bot))
  def withdraw(id: TournamentId, botId: BotId): ZIO[TournamentService, Throwable, Unit] =
    ZIO.serviceWithZIO(_.withdraw(id, botId))

final class TournamentServiceLive(
  repo: TournamentRepository,
  gameRepo: GameRepository,
  streamService: StreamService,
) extends TournamentService:

  override def create(form: CreateTournamentForm, director: UserId): Task[Tournament] =
    for
      id <- ZIO.succeed(TournamentId(java.util.UUID.randomUUID().toString.take(8)))
      now <- zio.Clock.instant
      tournament = Tournament(
        id = id,
        config = TournamentConfig(
          name = form.name,
          nbRounds = form.nbRounds,
          clock = DomainClock(form.clockLimit, form.clockIncrement),
          rated = form.rated,
          format = form.format,
          startPosition = form.startPosition,
          matchesPerPairing = form.matchesPerPairing,
        ),
        status = TournamentStatus.Created,
        participants = Vector.empty,
        rounds = Vector.empty,
        currentRound = 0,
        director = director,
        createdAt = now,
        startedAt = None,
        winner = None,
      )
      _ <- repo.save(tournament)
    yield tournament

  override def get(id: TournamentId): Task[Tournament] =
    repo.get(id).flatMap:
      case Some(t) => ZIO.succeed(t)
      case None    => ZIO.fail(DomainError.NotFound("tournament not found"))

  override def list: Task[Map[TournamentStatus, Vector[Tournament]]] =
    repo.listByStatus

  override def delete(id: TournamentId, director: UserId): Task[Unit] =
    get(id).flatMap: t =>
      ZIO.when(t.director != director)(ZIO.fail(DomainError.Forbidden("not the director"))) *>
        ZIO.fromEither(TournamentLifecycle.terminate(t)).flatMap(repo.save)

  override def start(id: TournamentId, director: UserId): Task[Tournament] =
    for
      t <- get(id)
      _ <- ZIO.when(t.director != director)(ZIO.fail(DomainError.Forbidden("not the director")))
      now <- zio.Clock.instant
      started <- ZIO.fromEither(TournamentLifecycle.start(t, now))
      algorithm = selectAlgorithm(started.config.format)
      pairings = algorithm.pair(started.participants, Vector.empty, Vector.empty, 1)
      games <- ZIO.foreach(pairings)(createGamesForPairing(started, 1, _))
      roundPairings = games.map((pair, gameIds) =>
        Pairing(pair._1, pair._2,
          gameIds.map(gid => Match(gid, None, None)),
          None))
      round = Round(1, roundPairings)
      withRound <- ZIO.fromEither(TournamentLifecycle.addRound(started, round))
      _ <- repo.save(withRound)
      _ <- streamService.publishTournament(id, TournamentEvent.TournamentStarted)
      _ <- streamService.publishTournament(id, TournamentEvent.RoundStarted(1))
      _ <- ZIO.foreach(roundPairings)(publishGameStartEvents(id, 1, _))
    yield withRound

  override def join(id: TournamentId, bot: BotRef): Task[Unit] =
    get(id).flatMap: t =>
      ZIO.fromEither(TournamentLifecycle.join(t, bot)).flatMap(repo.save)

  override def withdraw(id: TournamentId, botId: BotId): Task[Unit] =
    get(id).flatMap: t =>
      ZIO.fromEither(TournamentLifecycle.withdraw(t, botId)).flatMap(repo.save)

  private def selectAlgorithm(format: TournamentFormat): PairingAlgorithm =
    format match
      case TournamentFormat.Swiss            => SwissPairing
      case TournamentFormat.SingleElimination => EliminationBracket
      case TournamentFormat.DoubleElimination => EliminationBracket
      case TournamentFormat.GroupStage(_)     => GroupStagePairing
      case TournamentFormat.League           => RoundRobinPairing

  private def createGamesForPairing(
    tournament: Tournament,
    roundNum: Int,
    pair: (BotRef, BotRef),
  ): Task[((BotRef, BotRef), Vector[GameId])] =
    val (white, black) = pair
    ZIO.foreach((1 to tournament.config.matchesPerPairing).toVector): matchNum =>
      val gameId = GameId(java.util.UUID.randomUUID().toString.take(8))
      val fen = tournament.config.startPosition.toFen
      val game = Game(
        id = gameId,
        tournamentId = tournament.id,
        round = roundNum,
        white = white,
        black = black,
        moves = Vector.empty,
        status = GameStatus.Ongoing,
        turn = Color.White,
        winner = None,
        clock = GameClock(tournament.config.clock.limit.toDouble, tournament.config.clock.limit.toDouble),
        startPosition = tournament.config.startPosition,
        fen = fen,
      )
      gameRepo.save(game).as(gameId)
    .map(ids => (pair, ids))

  private def publishGameStartEvents(
    tournamentId: TournamentId,
    round: Int,
    pairing: Pairing,
  ): UIO[Unit] =
    val firstGameId = pairing.matches.head.gameId
    streamService.publishTournament(tournamentId,
      TournamentEvent.GameStart(round, firstGameId, Color.White)) *>
    streamService.publishTournament(tournamentId,
      TournamentEvent.GameStart(round, firstGameId, Color.Black))

object TournamentServiceLive:
  val layer: URLayer[TournamentRepository & GameRepository & StreamService, TournamentService] =
    ZLayer:
      for
        repo <- ZIO.service[TournamentRepository]
        gameRepo <- ZIO.service[GameRepository]
        stream <- ZIO.service[StreamService]
      yield TournamentServiceLive(repo, gameRepo, stream)
