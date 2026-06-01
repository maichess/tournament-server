package nowchess.tournament.persistence

import zio.*
import zio.test.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.tournament.*
import nowchess.tournament.domain.game.*

object RepositorySpec extends ZIOSpecDefault:

  private val testTournament = Tournament(
    id = TournamentId("t1"),
    config = TournamentConfig("Test", 2, nowchess.tournament.domain.model.Clock(300, 5), true, TournamentFormat.Swiss, StartPosition.Standard, 1),
    status = TournamentStatus.Created,
    participants = Vector.empty,
    rounds = Vector.empty,
    currentRound = 0,
    director = UserId("d1"),
    createdAt = java.time.Instant.now,
    startedAt = None,
    winner = None,
  )

  private val testGame = Game(
    id = GameId("g1"),
    tournamentId = TournamentId("t1"),
    round = 1,
    white = BotRef(BotId("w"), "White"),
    black = BotRef(BotId("b"), "Black"),
    moves = Vector.empty,
    status = GameStatus.Ongoing,
    clock = GameClock(300, 300),
    startPosition = StartPosition.Standard,
    winner = None,
    turn = Color.White,
    fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
  )

  def spec = suite("Repository")(
    suite("TournamentRepository companion")(
      test("save and get via companion") {
        for
          _ <- TournamentRepository.save(testTournament)
          result <- TournamentRepository.get(TournamentId("t1"))
        yield assertTrue(result.isDefined)
      },
      test("delete via companion") {
        for
          _ <- TournamentRepository.save(testTournament)
          _ <- TournamentRepository.delete(TournamentId("t1"))
          result <- TournamentRepository.get(TournamentId("t1"))
        yield assertTrue(result.isEmpty)
      },
      test("listByStatus via companion") {
        for
          _ <- TournamentRepository.save(testTournament)
          result <- TournamentRepository.listByStatus
        yield assertTrue(result(TournamentStatus.Created).nonEmpty)
      },
    ),
    suite("GameRepository companion")(
      test("save and get via companion") {
        for
          _ <- GameRepository.save(testGame)
          result <- GameRepository.get(GameId("g1"))
        yield assertTrue(result.isDefined)
      },
      test("findByTournament via companion") {
        for
          _ <- GameRepository.save(testGame)
          result <- GameRepository.findByTournament(TournamentId("t1"))
        yield assertTrue(result.nonEmpty)
      },
    ),
  ).provide(InMemoryTournamentRepository.layer ++ InMemoryGameRepository.layer) @@ TestAspect.sequential
