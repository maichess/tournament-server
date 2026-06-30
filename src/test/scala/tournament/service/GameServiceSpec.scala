package tournament.service

import zio.*
import zio.test.*
import tournament.domain.model.*
import tournament.domain.tournament.*
import tournament.domain.game.GameStatus
import tournament.domain.error.DomainError
import tournament.persistence.{InMemoryTournamentRepository, InMemoryGameRepository, InMemoryOpeningRepository, InMemoryBotRegistryRepository}

object GameServiceSpec extends ZIOSpecDefault:

  private val bot1 = BotRef(BotId("b1"), "Bot1")
  private val bot2 = BotRef(BotId("b2"), "Bot2")
  private val director = UserId("director")

  private val form = CreateTournamentForm(
    name = "Test", nbRounds = 1, clockLimit = 300, clockIncrement = 3,
    rated = true, format = TournamentFormat.Swiss,
    startPosition = StartPosition.Standard, matchesPerPairing = 1, groupSize = None,
  )

  private val testLayer =
    InMemoryTournamentRepository.layer ++
    InMemoryGameRepository.layer ++
    StreamServiceLive.layer ++
    (InMemoryOpeningRepository.layer >>> OpeningServiceLive.layer) ++
    ServiceTestLayers.botRegistry >>>
    (TournamentServiceLive.layer ++ GameServiceLive.layer)

  private def setupStartedTournament =
    for
      tsvc <- ZIO.service[TournamentService]
      t <- tsvc.create(form, director)
      _ <- tsvc.join(t.id, bot1)
      _ <- tsvc.join(t.id, bot2)
      started <- tsvc.start(t.id, director)
      gameId = started.rounds.head.pairings.head.matches.head.gameId
    yield (started, gameId)

  def spec = suite("GameService")(
    test("get game") {
      (for
        gsvc <- ZIO.service[GameService]
        (_, gameId) <- setupStartedTournament
        game <- gsvc.getGame(gameId)
      yield
        assertTrue(game.status == GameStatus.Ongoing) &&
        assertTrue(game.turn == Color.White)
      ).provide(testLayer)
    },
    test("get non-existent game fails") {
      (for
        gsvc <- ZIO.service[GameService]
        result <- gsvc.getGame(GameId("nonexistent")).exit
      yield assertTrue(result.isFailure)
      ).provide(testLayer)
    },
    test("make valid move") {
      (for
        gsvc <- ZIO.service[GameService]
        (_, gameId) <- setupStartedTournament
        game <- gsvc.makeMove(gameId, "e2e4", bot1.id)
      yield
        assertTrue(game.moves == Vector("e2e4")) &&
        assertTrue(game.turn == Color.Black)
      ).provide(testLayer)
    },
    test("wrong bot cannot move") {
      (for
        gsvc <- ZIO.service[GameService]
        (_, gameId) <- setupStartedTournament
        result <- gsvc.makeMove(gameId, "e7e5", bot2.id).exit
      yield assertTrue(result match
        case Exit.Failure(cause) => cause.failureOption.exists(_.isInstanceOf[DomainError.Forbidden])
        case _ => false
      )
      ).provide(testLayer)
    },
    test("illegal move rejected") {
      (for
        gsvc <- ZIO.service[GameService]
        (_, gameId) <- setupStartedTournament
        result <- gsvc.makeMove(gameId, "e2e5", bot1.id).exit
      yield assertTrue(result.isFailure)
      ).provide(testLayer)
    },
    test("invalid UCI rejected") {
      (for
        gsvc <- ZIO.service[GameService]
        (_, gameId) <- setupStartedTournament
        result <- gsvc.makeMove(gameId, "xx", bot1.id).exit
      yield assertTrue(result.isFailure)
      ).provide(testLayer)
    },
    test("fool's mate ends game") {
      (for
        gsvc <- ZIO.service[GameService]
        (_, gameId) <- setupStartedTournament
        _ <- gsvc.makeMove(gameId, "f2f3", bot1.id)
        _ <- gsvc.makeMove(gameId, "e7e5", bot2.id)
        _ <- gsvc.makeMove(gameId, "g2g4", bot1.id)
        game <- gsvc.makeMove(gameId, "d8h4", bot2.id)
      yield
        assertTrue(game.status == GameStatus.Checkmate) &&
        assertTrue(game.winner.contains(Color.Black))
      ).provide(testLayer)
    },
    test("threefold repetition draws the game") {
      (for
        gsvc <- ZIO.service[GameService]
        (_, gameId) <- setupStartedTournament
        _ <- gsvc.makeMove(gameId, "g1f3", bot1.id)
        _ <- gsvc.makeMove(gameId, "g8f6", bot2.id)
        _ <- gsvc.makeMove(gameId, "f3g1", bot1.id)
        mid <- gsvc.makeMove(gameId, "f6g8", bot2.id)
        _ <- gsvc.makeMove(gameId, "g1f3", bot1.id)
        _ <- gsvc.makeMove(gameId, "g8f6", bot2.id)
        _ <- gsvc.makeMove(gameId, "f3g1", bot1.id)
        game <- gsvc.makeMove(gameId, "f6g8", bot2.id)
      yield
        // start position reached twice so far → still ongoing
        assertTrue(mid.status == GameStatus.Ongoing) &&
        // start position reached a third time → draw
        assertTrue(game.status == GameStatus.Draw) &&
        assertTrue(game.winner.isEmpty)
      ).provide(testLayer)
    },
    test("cannot move after game ends") {
      (for
        gsvc <- ZIO.service[GameService]
        (_, gameId) <- setupStartedTournament
        _ <- gsvc.makeMove(gameId, "f2f3", bot1.id)
        _ <- gsvc.makeMove(gameId, "e7e5", bot2.id)
        _ <- gsvc.makeMove(gameId, "g2g4", bot1.id)
        _ <- gsvc.makeMove(gameId, "d8h4", bot2.id)
        result <- gsvc.makeMove(gameId, "e2e4", bot1.id).exit
      yield assertTrue(result.isFailure)
      ).provide(testLayer)
    },
    test("game completion updates tournament round") {
      (for
        gsvc <- ZIO.service[GameService]
        tsvc <- ZIO.service[TournamentService]
        (started, gameId) <- setupStartedTournament
        _ <- gsvc.makeMove(gameId, "f2f3", bot1.id)
        _ <- gsvc.makeMove(gameId, "e7e5", bot2.id)
        _ <- gsvc.makeMove(gameId, "g2g4", bot1.id)
        _ <- gsvc.makeMove(gameId, "d8h4", bot2.id)
        t <- tsvc.get(started.id)
      yield
        // 1-round tournament, game finished → tournament should be finished
        assertTrue(t.status == TournamentStatus.Finished) &&
        assertTrue(t.winner.isDefined)
      ).provide(testLayer)
    },
  )
