package tournament.service

import zio.*
import zio.test.*
import tournament.domain.model.*
import tournament.domain.tournament.*
import tournament.domain.game.{GameStatus, Game, GameClock}
import tournament.domain.error.DomainError
import tournament.persistence.{InMemoryTournamentRepository, InMemoryGameRepository, GameRepository, TournamentRepository, InMemoryOpeningRepository, InMemoryBotRegistryRepository}

object GameServiceCoverageSpec extends ZIOSpecDefault:

  private val bot1 = BotRef(BotId("b1"), "Bot1")
  private val bot2 = BotRef(BotId("b2"), "Bot2")
  private val bot3 = BotRef(BotId("b3"), "Bot3")
  private val bot4 = BotRef(BotId("b4"), "Bot4")
  private val director = UserId("director")

  private val testLayer =
    InMemoryTournamentRepository.layer ++
    InMemoryGameRepository.layer ++
    StreamServiceLive.layer ++
    (InMemoryOpeningRepository.layer >>> OpeningServiceLive.layer) ++
    ServiceTestLayers.botRegistry >>>
    (TournamentServiceLive.layer ++ GameServiceLive.layer ++ ZLayer.service[GameRepository] ++ ZLayer.service[TournamentRepository])

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

  def spec = suite("GameService coverage")(
    test("makeMove fails on invalid FEN") {
      (for
        gameRepo <- ZIO.service[GameRepository]
        // Save a game with invalid FEN directly
        badGame = Game(
          id = GameId("bad"), tournamentId = TournamentId("t1"), round = 1,
          white = bot1, black = bot2, moves = Vector.empty,
          status = GameStatus.Ongoing, turn = Color.White, winner = None,
          clock = GameClock(300, 300), startPosition = StartPosition.Standard,
          fen = "invalid-fen",
        )
        _ <- gameRepo.save(badGame)
        gsvc <- ZIO.service[GameService]
        result <- gsvc.makeMove(GameId("bad"), "e2e4", BotId("b1")).exit
      yield assertTrue(result.isFailure)
      ).provide(testLayer)
    },
    test("2-round league tournament covers selectAlgorithm League branch in round 2") {
      val form = CreateTournamentForm(
        name = "League2R", nbRounds = 2, clockLimit = 300, clockIncrement = 3,
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
        gameId1 = started.rounds.head.pairings.head.matches.head.gameId
        _ <- foolsMate(gsvc, gameRepo, gameId1)
        afterR1 <- tsvc.get(t.id)
        _ <- assertTrue(afterR1.currentRound == 2)
        gameId2 = afterR1.rounds.last.pairings.head.matches.head.gameId
        _ <- foolsMate(gsvc, gameRepo, gameId2)
        result <- tsvc.get(t.id)
      yield assertTrue(result.status == TournamentStatus.Finished)
      ).provide(testLayer)
    },
    test("2-round elimination tournament with 4 bots covers selectAlgorithm Elimination branch") {
      val form = CreateTournamentForm(
        name = "Elim2R", nbRounds = 2, clockLimit = 300, clockIncrement = 3,
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
        _ <- tsvc.join(t.id, bot3)
        _ <- tsvc.join(t.id, bot4)
        started <- tsvc.start(t.id, director)
        // Round 1: 2 games (4 bots, seeded bracket)
        _ <- ZIO.foreach(started.rounds.head.pairings) { p =>
          foolsMate(gsvc, gameRepo, p.matches.head.gameId)
        }
        afterR1 <- tsvc.get(t.id)
        _ <- assertTrue(afterR1.currentRound == 2)
        // Round 2: 1 game (2 winners)
        _ <- ZIO.foreach(afterR1.rounds.last.pairings) { p =>
          foolsMate(gsvc, gameRepo, p.matches.head.gameId)
        }
        result <- tsvc.get(t.id)
      yield assertTrue(result.status == TournamentStatus.Finished)
      ).provide(testLayer)
    },
    test("2-round double elimination tournament with 4 bots") {
      val form = CreateTournamentForm(
        name = "DblElim2R", nbRounds = 2, clockLimit = 300, clockIncrement = 3,
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
        _ <- tsvc.join(t.id, bot3)
        _ <- tsvc.join(t.id, bot4)
        started <- tsvc.start(t.id, director)
        _ <- ZIO.foreach(started.rounds.head.pairings) { p =>
          foolsMate(gsvc, gameRepo, p.matches.head.gameId)
        }
        afterR1 <- tsvc.get(t.id)
        _ <- assertTrue(afterR1.currentRound == 2)
        _ <- ZIO.foreach(afterR1.rounds.last.pairings) { p =>
          foolsMate(gsvc, gameRepo, p.matches.head.gameId)
        }
        result <- tsvc.get(t.id)
      yield assertTrue(result.status == TournamentStatus.Finished)
      ).provide(testLayer)
    },
    test("start GroupStage format tournament") {
      val form = CreateTournamentForm(
        name = "Group", nbRounds = 1, clockLimit = 300, clockIncrement = 3,
        rated = true, format = TournamentFormat.GroupStage(2),
        startPosition = StartPosition.Standard, matchesPerPairing = 1, groupSize = Some(2),
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
    test("2-round GroupStage tournament covers selectAlgorithm GroupStage in startNextRound") {
      val form = CreateTournamentForm(
        name = "GroupStage2R", nbRounds = 2, clockLimit = 300, clockIncrement = 3,
        rated = true, format = TournamentFormat.GroupStage(4),
        startPosition = StartPosition.Standard, matchesPerPairing = 1, groupSize = Some(4),
      )
      (for
        tsvc <- ZIO.service[TournamentService]
        gsvc <- ZIO.service[GameService]
        gameRepo <- ZIO.service[GameRepository]
        t <- tsvc.create(form, director)
        _ <- tsvc.join(t.id, bot1)
        _ <- tsvc.join(t.id, bot2)
        _ <- tsvc.join(t.id, bot3)
        _ <- tsvc.join(t.id, bot4)
        started <- tsvc.start(t.id, director)
        _ <- ZIO.foreach(started.rounds.head.pairings) { p =>
          foolsMate(gsvc, gameRepo, p.matches.head.gameId)
        }
        afterR1 <- tsvc.get(t.id)
        _ <- assertTrue(afterR1.currentRound == 2)
        _ <- ZIO.foreach(afterR1.rounds.last.pairings) { p =>
          foolsMate(gsvc, gameRepo, p.matches.head.gameId)
        }
        result <- tsvc.get(t.id)
      yield assertTrue(result.status == TournamentStatus.Finished)
      ).provide(testLayer)
    },
    test("game end with non-existent tournament fails") {
      (for
        gameRepo <- ZIO.service[GameRepository]
        gsvc <- ZIO.service[GameService]
        badGame = Game(
          id = GameId("orphan"), tournamentId = TournamentId("nonexistent"), round = 1,
          white = bot1, black = bot2, moves = Vector.empty,
          status = GameStatus.Ongoing, turn = Color.White, winner = None,
          clock = GameClock(300, 300), startPosition = StartPosition.Standard,
          fen = StartPosition.Standard.toFen,
        )
        _ <- gameRepo.save(badGame)
        _ <- gsvc.makeMove(GameId("orphan"), "f2f3", BotId("b1"))
        _ <- gsvc.makeMove(GameId("orphan"), "e7e5", BotId("b2"))
        _ <- gsvc.makeMove(GameId("orphan"), "g2g4", BotId("b1"))
        result <- gsvc.makeMove(GameId("orphan"), "d8h4", BotId("b2")).exit
      yield assertTrue(result.isFailure)
      ).provide(testLayer)
    },
    test("game end with corrupted tournament round fails") {
      val form = CreateTournamentForm(
        name = "Corrupt", nbRounds = 1, clockLimit = 300, clockIncrement = 3,
        rated = true, format = TournamentFormat.Swiss,
        startPosition = StartPosition.Standard, matchesPerPairing = 1, groupSize = None,
      )
      (for
        tsvc <- ZIO.service[TournamentService]
        gsvc <- ZIO.service[GameService]
        gameRepo <- ZIO.service[GameRepository]
        tournamentRepo <- ZIO.service[TournamentRepository]
        t <- tsvc.create(form, director)
        _ <- tsvc.join(t.id, bot1)
        _ <- tsvc.join(t.id, bot2)
        started <- tsvc.start(t.id, director)
        gameId = started.rounds.head.pairings.head.matches.head.gameId
        // Corrupt: set currentRound to 99, a round that doesn't exist
        _ <- tournamentRepo.save(started.copy(currentRound = 99))
        game <- gameRepo.get(gameId).map(_.get)
        _ <- gsvc.makeMove(gameId, "f2f3", game.white.id)
        _ <- gsvc.makeMove(gameId, "e7e5", game.black.id)
        _ <- gsvc.makeMove(gameId, "g2g4", game.white.id)
        result <- gsvc.makeMove(gameId, "d8h4", game.black.id).exit
      yield assertTrue(result.isFailure)
      ).provide(testLayer)
    },
    test("finishing the second game of an already-decided pairing is a no-op") {
      val form = CreateTournamentForm(
        name = "BestOf2", nbRounds = 1, clockLimit = 300, clockIncrement = 3,
        rated = true, format = TournamentFormat.Swiss,
        startPosition = StartPosition.Standard, matchesPerPairing = 2, groupSize = None,
      )
      (for
        tsvc <- ZIO.service[TournamentService]
        gsvc <- ZIO.service[GameService]
        gameRepo <- ZIO.service[GameRepository]
        t <- tsvc.create(form, director)
        _ <- tsvc.join(t.id, bot1)
        _ <- tsvc.join(t.id, bot2)
        started <- tsvc.start(t.id, director)
        pairing = started.rounds.head.pairings.head
        g1 = pairing.matches(0).gameId
        g2 = pairing.matches(1).gameId
        // g1 alone decides the best-of-2 (one win is a majority), completing the pairing
        _ <- foolsMate(gsvc, gameRepo, g1)
        // g2 is still ongoing; finishing it now hits the alreadyComplete short-circuit
        _ <- foolsMate(gsvc, gameRepo, g2)
        g2state <- gameRepo.get(g2).map(_.get)
      yield assertTrue(g2state.status == GameStatus.Checkmate)
      ).provide(testLayer)
    },
    test("makeMove on finished game fails") {
      val form = CreateTournamentForm(
        name = "Fin", nbRounds = 1, clockLimit = 300, clockIncrement = 3,
        rated = true, format = TournamentFormat.Swiss,
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
        game <- gameRepo.get(gameId).map(_.get)
        result <- gsvc.makeMove(gameId, "e2e4", game.white.id).exit
      yield assertTrue(result.isFailure)
      ).provide(testLayer)
    },
  )
