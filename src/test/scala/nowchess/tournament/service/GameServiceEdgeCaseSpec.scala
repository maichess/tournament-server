package nowchess.tournament.service

import zio.*
import zio.test.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.tournament.*
import nowchess.tournament.domain.game.{GameStatus, Game, GameClock}
import nowchess.tournament.domain.error.DomainError
import nowchess.tournament.persistence.{InMemoryTournamentRepository, InMemoryGameRepository, GameRepository, InMemoryOpeningRepository, InMemoryBotRegistryRepository}

object GameServiceEdgeCaseSpec extends ZIOSpecDefault:

  private val bot1 = BotRef(BotId("b1"), "Bot1")
  private val bot2 = BotRef(BotId("b2"), "Bot2")
  private val director = UserId("director")

  private val testLayer =
    InMemoryTournamentRepository.layer ++
    InMemoryGameRepository.layer ++
    StreamServiceLive.layer ++
    (InMemoryOpeningRepository.layer >>> OpeningServiceLive.layer) ++
    ServiceTestLayers.botRegistry >>>
    (TournamentServiceLive.layer ++ GameServiceLive.layer ++ ZLayer.service[GameRepository])

  def spec = suite("GameService edge cases")(
    test("stalemate ends game as draw") {
      // Use a custom FEN where stalemate is one move away
      // Black king on a8, white king on c7, white queen on b5
      // After Qb6, it's stalemate
      val form = CreateTournamentForm(
        name = "Stalemate", nbRounds = 1, clockLimit = 300, clockIncrement = 3,
        rated = true, format = TournamentFormat.Swiss,
        startPosition = StartPosition.FromFen("k7/2K5/8/1Q6/8/8/8/8 w - - 0 1"),
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
        game <- gameRepo.get(gameId).map(_.get)
        whiteId = game.white.id
        result <- gsvc.makeMove(gameId, "b5b6", whiteId)
      yield
        assertTrue(result.status == GameStatus.Stalemate) &&
        assertTrue(result.winner.isEmpty)
      ).provide(testLayer)
    },
    test("draw by 50 move rule") {
      // Position with halfmove clock at 99, next quiet move triggers draw
      val form = CreateTournamentForm(
        name = "DrawTest", nbRounds = 1, clockLimit = 300, clockIncrement = 3,
        rated = true, format = TournamentFormat.Swiss,
        startPosition = StartPosition.FromFen("4k3/8/8/8/8/8/8/4K3 w - - 99 50"),
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
        game <- gameRepo.get(gameId).map(_.get)
        whiteId = game.white.id
        result <- gsvc.makeMove(gameId, "e1d1", whiteId)
      yield assertTrue(result.status == GameStatus.Draw)
      ).provide(testLayer)
    },
    test("GameService companion methods work") {
      (for
        tsvc <- ZIO.service[TournamentService]
        form = CreateTournamentForm(
          name = "Companion", nbRounds = 1, clockLimit = 300, clockIncrement = 3,
          rated = true, format = TournamentFormat.Swiss,
          startPosition = StartPosition.Standard, matchesPerPairing = 1, groupSize = None,
        )
        t <- tsvc.create(form, director)
        _ <- tsvc.join(t.id, bot1)
        _ <- tsvc.join(t.id, bot2)
        started <- tsvc.start(t.id, director)
        gameId = started.rounds.head.pairings.head.matches.head.gameId
        game <- GameService.getGame(gameId)
        gameRepo <- ZIO.service[GameRepository]
        whiteId = game.white.id
        afterMove <- GameService.makeMove(gameId, "e2e4", whiteId)
      yield assertTrue(afterMove.moves.length == 1)
      ).provide(testLayer)
    },
  )
