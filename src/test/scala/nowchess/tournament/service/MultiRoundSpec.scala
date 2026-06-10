package nowchess.tournament.service

import zio.*
import zio.test.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.tournament.*
import nowchess.tournament.domain.game.GameStatus
import nowchess.tournament.persistence.{InMemoryTournamentRepository, InMemoryGameRepository, GameRepository, InMemoryOpeningRepository, InMemoryBotRegistryRepository}

object MultiRoundSpec extends ZIOSpecDefault:

  private val bot1 = BotRef(BotId("b1"), "Bot1")
  private val bot2 = BotRef(BotId("b2"), "Bot2")
  private val director = UserId("director")

  private val testLayer =
    InMemoryTournamentRepository.layer ++
    InMemoryGameRepository.layer ++
    StreamServiceLive.layer ++
    (InMemoryOpeningRepository.layer >>> OpeningServiceLive.layer) ++
    (InMemoryBotRegistryRepository.layer >>> BotRegistryServiceLive.layer) >>>
    (TournamentServiceLive.layer ++ GameServiceLive.layer ++ ZLayer.service[GameRepository])

  private def foolsMate(gsvc: GameService, gameRepo: GameRepository, gameId: GameId) =
    for
      game <- gameRepo.get(gameId).map(_.get)
      whiteId = game.white.id
      blackId = game.black.id
      _ <- gsvc.makeMove(gameId, "f2f3", whiteId)
      _ <- gsvc.makeMove(gameId, "e7e5", blackId)
      _ <- gsvc.makeMove(gameId, "g2g4", whiteId)
      _ <- gsvc.makeMove(gameId, "d8h4", blackId)
    yield ()

  def spec = suite("MultiRound")(
    test("2-round tournament advances to round 2 after round 1 completes") {
      val form = CreateTournamentForm(
        name = "Multi", nbRounds = 2, clockLimit = 300, clockIncrement = 3,
        rated = true, format = TournamentFormat.Swiss,
        startPosition = StartPosition.Standard, matchesPerPairing = 1, groupSize = None,
      )
      (for
        tsvc <- ZIO.service[TournamentService]
        gsvc <- ZIO.service[GameService]
        t <- tsvc.create(form, director)
        _ <- tsvc.join(t.id, bot1)
        _ <- tsvc.join(t.id, bot2)
        started <- tsvc.start(t.id, director)
        gameId1 = started.rounds.head.pairings.head.matches.head.gameId
        gameRepo <- ZIO.service[GameRepository]
        _ <- foolsMate(gsvc, gameRepo, gameId1)
        afterR1 <- tsvc.get(t.id)
        _ <- assertTrue(afterR1.status == TournamentStatus.Started)
        _ <- assertTrue(afterR1.currentRound == 2)
        _ <- assertTrue(afterR1.rounds.length == 2)
        gameId2 = afterR1.rounds.last.pairings.head.matches.head.gameId
        _ <- foolsMate(gsvc, gameRepo, gameId2)
        afterR2 <- tsvc.get(t.id)
      yield
        assertTrue(afterR2.status == TournamentStatus.Finished) &&
        assertTrue(afterR2.winner.isDefined)
      ).provide(testLayer)
    },
    test("round-robin format tournament") {
      val form = CreateTournamentForm(
        name = "RR", nbRounds = 1, clockLimit = 300, clockIncrement = 3,
        rated = true, format = TournamentFormat.League,
        startPosition = StartPosition.Standard, matchesPerPairing = 1, groupSize = None,
      )
      (for
        tsvc <- ZIO.service[TournamentService]
        gsvc <- ZIO.service[GameService]
        gameRepo <- ZIO.service[GameRepository]
        t <- tsvc.create(form, director)
        _ <- tsvc.join(t.id, bot1)
        _ <- tsvc.join(t.id, bot2)
        started <- tsvc.start(t.id, director)
        gameId = started.rounds.head.pairings.head.matches.head.gameId
        _ <- foolsMate(gsvc, gameRepo, gameId)
        result <- tsvc.get(t.id)
      yield assertTrue(result.status == TournamentStatus.Finished)
      ).provide(testLayer)
    },
    test("elimination format tournament") {
      val form = CreateTournamentForm(
        name = "Elim", nbRounds = 1, clockLimit = 300, clockIncrement = 3,
        rated = true, format = TournamentFormat.SingleElimination,
        startPosition = StartPosition.Standard, matchesPerPairing = 1, groupSize = None,
      )
      (for
        tsvc <- ZIO.service[TournamentService]
        gsvc <- ZIO.service[GameService]
        gameRepo <- ZIO.service[GameRepository]
        t <- tsvc.create(form, director)
        _ <- tsvc.join(t.id, bot1)
        _ <- tsvc.join(t.id, bot2)
        started <- tsvc.start(t.id, director)
        gameId = started.rounds.head.pairings.head.matches.head.gameId
        _ <- foolsMate(gsvc, gameRepo, gameId)
        result <- tsvc.get(t.id)
      yield assertTrue(result.status == TournamentStatus.Finished)
      ).provide(testLayer)
    },
    test("double elimination format tournament") {
      val form = CreateTournamentForm(
        name = "DblElim", nbRounds = 1, clockLimit = 300, clockIncrement = 3,
        rated = true, format = TournamentFormat.DoubleElimination,
        startPosition = StartPosition.Standard, matchesPerPairing = 1, groupSize = None,
      )
      (for
        tsvc <- ZIO.service[TournamentService]
        gsvc <- ZIO.service[GameService]
        gameRepo <- ZIO.service[GameRepository]
        t <- tsvc.create(form, director)
        _ <- tsvc.join(t.id, bot1)
        _ <- tsvc.join(t.id, bot2)
        started <- tsvc.start(t.id, director)
        gameId = started.rounds.head.pairings.head.matches.head.gameId
        _ <- foolsMate(gsvc, gameRepo, gameId)
        result <- tsvc.get(t.id)
      yield assertTrue(result.status == TournamentStatus.Finished)
      ).provide(testLayer)
    },
    test("custom FEN start position") {
      val form = CreateTournamentForm(
        name = "Custom", nbRounds = 1, clockLimit = 300, clockIncrement = 3,
        rated = true, format = TournamentFormat.Swiss,
        startPosition = StartPosition.FromFen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"),
        matchesPerPairing = 1, groupSize = None,
      )
      (for
        tsvc <- ZIO.service[TournamentService]
        gsvc <- ZIO.service[GameService]
        gameRepo <- ZIO.service[GameRepository]
        t <- tsvc.create(form, director)
        _ <- tsvc.join(t.id, bot1)
        _ <- tsvc.join(t.id, bot2)
        started <- tsvc.start(t.id, director)
        gameId = started.rounds.head.pairings.head.matches.head.gameId
        game <- gsvc.getGame(gameId)
      yield assertTrue(game.fen.contains("4P3"))
      ).provide(testLayer)
    },
    test("stream events published during tournament") {
      (for
        tsvc <- ZIO.service[TournamentService]
        gsvc <- ZIO.service[GameService]
        ssvc <- ZIO.service[StreamService]
        gameRepo <- ZIO.service[GameRepository]
        form = CreateTournamentForm(
          name = "StreamTest", nbRounds = 1, clockLimit = 300, clockIncrement = 3,
          rated = true, format = TournamentFormat.Swiss,
          startPosition = StartPosition.Standard, matchesPerPairing = 1, groupSize = None,
        )
        t <- tsvc.create(form, director)
        tStream <- ssvc.subscribeTournament(t.id)
        _ <- tsvc.join(t.id, bot1)
        _ <- tsvc.join(t.id, bot2)
        started <- tsvc.start(t.id, director)
        gameId = started.rounds.head.pairings.head.matches.head.gameId
        gStream <- ssvc.subscribeGame(gameId)
        _ <- foolsMate(gsvc, gameRepo, gameId)
      yield assertTrue(true)
      ).provide(
        InMemoryTournamentRepository.layer ++
        InMemoryGameRepository.layer ++
        StreamServiceLive.layer ++
        (InMemoryOpeningRepository.layer >>> OpeningServiceLive.layer) ++
        (InMemoryBotRegistryRepository.layer >>> BotRegistryServiceLive.layer) >>>
        (TournamentServiceLive.layer ++ GameServiceLive.layer ++ ZLayer.service[StreamService] ++ ZLayer.service[GameRepository])
      )
    },
  )
