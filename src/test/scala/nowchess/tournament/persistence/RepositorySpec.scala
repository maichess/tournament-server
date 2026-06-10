package nowchess.tournament.persistence

import zio.*
import zio.test.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.tournament.*
import nowchess.tournament.domain.game.*
import nowchess.tournament.domain.opening.Opening

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
    suite("IdentityRepository")(
      test("save and get"):
        val identity = Identity("id1", "Bot1", isBot = true)
        for
          _ <- IdentityRepository.save(identity)
          result <- IdentityRepository.get("id1")
        yield assertTrue(result.contains(identity))
      ,
      test("get returns None for missing"):
        for result <- IdentityRepository.get("missing")
        yield assertTrue(result.isEmpty)
      ,
      test("findByName finds matching"):
        val identity = Identity("id2", "UniqueBot", isBot = true)
        for
          _ <- IdentityRepository.save(identity)
          result <- IdentityRepository.findByName("UniqueBot", isBot = true)
        yield assertTrue(result.contains(identity))
      ,
      test("findByName returns None for wrong isBot"):
        val identity = Identity("id3", "TypeTest", isBot = true)
        for
          _ <- IdentityRepository.save(identity)
          result <- IdentityRepository.findByName("TypeTest", isBot = false)
        yield assertTrue(result.isEmpty)
      ,
      test("findByName returns None for wrong name"):
        val identity = Identity("id4", "NameTest", isBot = false)
        for
          _ <- IdentityRepository.save(identity)
          result <- IdentityRepository.findByName("Other", isBot = false)
        yield assertTrue(result.isEmpty)
      ,
    ),
    suite("OpeningRepository companion")(
      test("save, get and list via companion") {
        val opening = Opening("custom", "Custom", "fen")
        for
          _ <- OpeningRepository.save(opening)
          got <- OpeningRepository.get("custom")
          all <- OpeningRepository.list
        yield assertTrue(got.contains(opening), all.contains(opening))
      },
      test("get returns None for a missing key") {
        for got <- OpeningRepository.get("missing")
        yield assertTrue(got.isEmpty)
      },
    ),
    suite("BotRegistryRepository companion")(
      test("save, get, list and delete via companion") {
        val bot = RegisteredBot(BotId("rb1"), "RegBot", Some("http://x"))
        for
          _ <- BotRegistryRepository.save(bot)
          got <- BotRegistryRepository.get(BotId("rb1"))
          all <- BotRegistryRepository.list
          _ <- BotRegistryRepository.delete(BotId("rb1"))
          gone <- BotRegistryRepository.get(BotId("rb1"))
        yield assertTrue(got.contains(bot), all.contains(bot), gone.isEmpty)
      },
    ),
  ).provide(
    InMemoryTournamentRepository.layer ++ InMemoryGameRepository.layer ++ InMemoryIdentityRepository.layer ++
    InMemoryOpeningRepository.layer ++ InMemoryBotRegistryRepository.layer
  ) @@ TestAspect.sequential
