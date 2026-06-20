package nowchess.tournament.service

import zio.*
import nowchess.tournament.domain.model.{TournamentId, BotId, UserId, BotRef, StartPosition, Clock as DomainClock}
import nowchess.tournament.domain.tournament.*
import nowchess.tournament.domain.round.*
import nowchess.tournament.domain.error.DomainError
import nowchess.tournament.domain.event.TournamentEvent
import nowchess.tournament.domain.lifecycle.TournamentLifecycle
import nowchess.tournament.persistence.{TournamentRepository, GameRepository}

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
  opening: Option[String] = None,
  bots: Option[String] = None,
  maxConcurrentGames: Option[Int] = None,
  openings: Option[String] = None,
)

trait TournamentService:
  def create(form: CreateTournamentForm, director: UserId): Task[Tournament]
  def get(id: TournamentId): Task[Tournament]
  def list: Task[Map[TournamentStatus, Vector[Tournament]]]
  def delete(id: TournamentId, director: UserId): Task[Unit]
  def start(id: TournamentId, director: UserId): Task[Tournament]
  def join(id: TournamentId, bot: BotRef): Task[Unit]
  def addRegisteredBot(id: TournamentId, botId: BotId, director: UserId): Task[Unit]
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
  def addRegisteredBot(id: TournamentId, botId: BotId, director: UserId): ZIO[TournamentService, Throwable, Unit] =
    ZIO.serviceWithZIO(_.addRegisteredBot(id, botId, director))
  def withdraw(id: TournamentId, botId: BotId): ZIO[TournamentService, Throwable, Unit] =
    ZIO.serviceWithZIO(_.withdraw(id, botId))

final class TournamentServiceLive(
  repo: TournamentRepository,
  gameRepo: GameRepository,
  streamService: StreamService,
  openingService: OpeningService,
  botRegistry: BotRegistryService,
) extends TournamentService:

  override def create(form: CreateTournamentForm, director: UserId): Task[Tournament] =
    for
      id <- ZIO.succeed(TournamentId(java.util.UUID.randomUUID().toString.take(8)))
      now <- zio.Clock.instant
      seed <- zio.Random.nextLong
      startPosition <- resolveStartPosition(form)
      openingBook <- resolveOpeningBook(form)
      participants <- resolveParticipants(form)
      // A multi-position opening book plays every position twice (colours
      // reversed) per pairing, so the pairing total is 2 * book size; keep
      // matchesPerPairing in step so completion/early-winner counts match.
      matchesPerPairing = if openingBook.nonEmpty then 2 * openingBook.size else form.matchesPerPairing
      tournament = Tournament(
        id = id,
        config = TournamentConfig(
          name = form.name,
          nbRounds = form.nbRounds,
          clock = DomainClock(form.clockLimit, form.clockIncrement),
          rated = form.rated,
          format = form.format,
          startPosition = startPosition,
          matchesPerPairing = matchesPerPairing,
          maxConcurrentGames = form.maxConcurrentGames,
          startPositions = openingBook,
        ),
        status = TournamentStatus.Created,
        participants = participants,
        rounds = Vector.empty,
        currentRound = 0,
        director = director,
        createdAt = now,
        startedAt = None,
        winner = None,
        seed = seed,
      )
      _ <- repo.save(tournament)
    yield tournament

  private def resolveStartPosition(form: CreateTournamentForm): Task[StartPosition] =
    form.opening.map(_.trim).filter(_.nonEmpty) match
      case None => ZIO.succeed(form.startPosition)
      case Some(key) =>
        openingService.resolve(key).flatMap:
          case Some(fen) => ZIO.succeed(StartPosition.FromFen(fen))
          case None      => ZIO.fail(DomainError.BadRequest(s"unknown opening: $key"))

  private def resolveOpeningBook(form: CreateTournamentForm): Task[Vector[StartPosition]] =
    val keys = form.openings.map(_.split(',').map(_.trim).filter(_.nonEmpty).toVector).getOrElse(Vector.empty)
    ZIO.foreach(keys): key =>
      openingService.resolve(key).flatMap:
        case Some(fen) => ZIO.succeed(StartPosition.FromFen(fen))
        case None      => ZIO.fail(DomainError.BadRequest(s"unknown opening: $key"))

  private def resolveParticipants(form: CreateTournamentForm): Task[Vector[BotRef]] =
    val ids = form.bots.map(_.split(',').map(_.trim).filter(_.nonEmpty).toVector.distinct).getOrElse(Vector.empty)
    ZIO.foreach(ids): raw =>
      botRegistry.get(BotId(raw)).flatMap:
        case Some(rb) => ZIO.succeed(rb.toRef)
        case None     => ZIO.fail(DomainError.BadRequest(s"unknown registered bot: $raw"))

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
      algorithm = GameFactory.selectAlgorithm(started)
      pairings = algorithm.pair(started.participants, Vector.empty, Vector.empty, 1)
      games <- ZIO.foreach(pairings)(GameFactory.createForPairing(started, 1, _, gameRepo))
      roundPairings = games.map((pair, matches) =>
        Pairing(pair._1, pair._2, matches, None))
      round = Round(1, roundPairings)
      withRound <- ZIO.fromEither(TournamentLifecycle.addRound(started, round))
      _ <- repo.save(withRound)
      _ <- streamService.publishTournament(id, TournamentEvent.TournamentStarted)
      _ <- streamService.publishTournament(id, TournamentEvent.RoundStarted(1))
      _ <- GameActivation.activate(withRound, round, gameRepo, streamService)
    yield withRound

  override def join(id: TournamentId, bot: BotRef): Task[Unit] =
    get(id).flatMap: t =>
      ZIO.fromEither(TournamentLifecycle.join(t, bot)).flatMap(repo.save)

  override def addRegisteredBot(id: TournamentId, botId: BotId, director: UserId): Task[Unit] =
    get(id).flatMap: t =>
      ZIO.when(t.director != director)(ZIO.fail(DomainError.Forbidden("not the director"))) *>
        botRegistry.get(botId).flatMap:
          case Some(b) => ZIO.fromEither(TournamentLifecycle.join(t, b.toRef)).flatMap(repo.save)
          case None    => ZIO.fail(DomainError.NotFound("registered bot not found"))

  override def withdraw(id: TournamentId, botId: BotId): Task[Unit] =
    get(id).flatMap: t =>
      ZIO.fromEither(TournamentLifecycle.withdraw(t, botId)).flatMap(repo.save)


object TournamentServiceLive:
  val layer: URLayer[TournamentRepository & GameRepository & StreamService & OpeningService & BotRegistryService, TournamentService] =
    ZLayer:
      for
        repo <- ZIO.service[TournamentRepository]
        gameRepo <- ZIO.service[GameRepository]
        stream <- ZIO.service[StreamService]
        openings <- ZIO.service[OpeningService]
        bots <- ZIO.service[BotRegistryService]
      yield TournamentServiceLive(repo, gameRepo, stream, openings, bots)
