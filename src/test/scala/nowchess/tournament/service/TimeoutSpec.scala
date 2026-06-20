package nowchess.tournament.service

import zio.*
import zio.test.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.tournament.*
import nowchess.tournament.domain.game.GameStatus
import nowchess.tournament.persistence.*

object TimeoutSpec extends ZIOSpecDefault:

  private val bot1 = BotRef(BotId("b1"), "Bot1")
  private val bot2 = BotRef(BotId("b2"), "Bot2")
  private val director = UserId("director")

  // 60s sudden-death clock so the daemon/move paths flag deterministically.
  private val form = CreateTournamentForm(
    name = "TO", nbRounds = 1, clockLimit = 60, clockIncrement = 0,
    rated = true, format = TournamentFormat.Swiss,
    startPosition = StartPosition.Standard, matchesPerPairing = 1, groupSize = None,
  )

  // TournamentService plus the raw collaborators, but NOT GameServiceLive.layer:
  // we build GameServiceLive by hand so its wall-clock daemon never runs and the
  // sweep can be driven explicitly under the TestClock.
  private val layer =
    InMemoryTournamentRepository.layer ++
    InMemoryGameRepository.layer ++
    StreamServiceLive.layer ++
    (InMemoryOpeningRepository.layer >>> OpeningServiceLive.layer) ++
    ServiceTestLayers.botRegistry >>>
    (TournamentServiceLive.layer ++
      ZLayer.service[GameRepository] ++
      ZLayer.service[TournamentRepository] ++
      ZLayer.service[StreamService])

  private def setup =
    for
      gameRepo       <- ZIO.service[GameRepository]
      tournamentRepo <- ZIO.service[TournamentRepository]
      stream         <- ZIO.service[StreamService]
      tsvc           <- ZIO.service[TournamentService]
      svc             = GameServiceLive(gameRepo, tournamentRepo, stream)
      t              <- tsvc.create(form, director)
      _              <- tsvc.join(t.id, bot1)
      _              <- tsvc.join(t.id, bot2)
      started        <- tsvc.start(t.id, director)
      gameId          = started.rounds.head.pairings.head.matches.head.gameId
    yield (svc, gameRepo, gameId)

  def spec = suite("Timeout")(
    test("timeoutSweep ends an ongoing game whose clock has run out"):
      (for
        (svc, gameRepo, gameId) <- setup
        _    <- TestClock.adjust(120.seconds)
        _    <- svc.timeoutSweep
        game <- gameRepo.get(gameId).map(_.get)
      yield assertTrue(
        game.status == GameStatus.Timeout,
        game.winner.contains(Color.Black),
        game.clock.whiteTime == 0.0,
      )).provide(layer)
    ,
    test("timeoutSweep leaves a game that still has time on the clock"):
      (for
        (svc, gameRepo, gameId) <- setup
        _    <- svc.timeoutSweep // TestClock at start of game; no time elapsed
        game <- gameRepo.get(gameId).map(_.get)
      yield assertTrue(game.status == GameStatus.Ongoing)).provide(layer)
    ,
    test("makeMove flags a mover who has already run out of time"):
      (for
        (svc, _, gameId) <- setup
        _    <- TestClock.adjust(120.seconds)
        game <- svc.makeMove(gameId, "e2e4", bot1.id)
      yield assertTrue(
        game.status == GameStatus.Timeout,
        game.winner.contains(Color.Black),
      )).provide(layer)
    ,
    test("timeoutSweep does not clobber a game a concurrent move just finished"):
      (for
        (svc, gameRepo, gameId) <- setup
        // Finish the game by checkmate first, then let the sweep run "late".
        _    <- svc.makeMove(gameId, "f2f3", bot1.id)
        _    <- svc.makeMove(gameId, "e7e5", bot2.id)
        _    <- svc.makeMove(gameId, "g2g4", bot1.id)
        _    <- svc.makeMove(gameId, "d8h4", bot2.id)
        _    <- TestClock.adjust(120.seconds)
        _    <- svc.timeoutSweep
        game <- gameRepo.get(gameId).map(_.get)
      yield assertTrue(game.status == GameStatus.Checkmate, game.winner.contains(Color.Black)))
        .provide(layer)
    ,
    test("timeoutTick swallows a repository failure instead of dying"):
      val failingTournaments = new TournamentRepository:
        def get(id: TournamentId)   = ZIO.fail(new RuntimeException("boom"))
        def save(t: Tournament)     = ZIO.unit
        def delete(id: TournamentId) = ZIO.unit
        def listByStatus            = ZIO.fail(new RuntimeException("boom"))
      (for
        gameRepo <- ZIO.service[GameRepository]
        stream   <- ZIO.service[StreamService]
        svc       = GameServiceLive(gameRepo, failingTournaments, stream)
        _        <- svc.timeoutTick // must succeed despite the failing repo
      yield assertCompletes).provide(InMemoryGameRepository.layer ++ StreamServiceLive.layer)
  ) @@ TestAspect.sequential
