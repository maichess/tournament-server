package nowchess.tournament.http

import zio.test.*
import zio.json.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.tournament.*
import nowchess.tournament.domain.game.{GameStatus, GameClock}
import nowchess.tournament.domain.event.GameEvent
import nowchess.tournament.http.codec.JsonCodecs.{*, given}

object JsonCodecsDecoderSpec extends ZIOSpecDefault:

  def spec = suite("JsonCodecs decoders")(
    test("GameOutcome all variants decode") {
      assertTrue(
        "\"white\"".fromJson[GameOutcome] == Right(GameOutcome.White),
        "\"black\"".fromJson[GameOutcome] == Right(GameOutcome.Black),
        "\"draw\"".fromJson[GameOutcome] == Right(GameOutcome.Draw),
      )
    },
    test("GameStatus all variants decode") {
      assertTrue(
        "\"ongoing\"".fromJson[GameStatus] == Right(GameStatus.Ongoing),
        "\"checkmate\"".fromJson[GameStatus] == Right(GameStatus.Checkmate),
        "\"stalemate\"".fromJson[GameStatus] == Right(GameStatus.Stalemate),
        "\"draw\"".fromJson[GameStatus] == Right(GameStatus.Draw),
        "\"resigned\"".fromJson[GameStatus] == Right(GameStatus.Resigned),
        "\"timeout\"".fromJson[GameStatus] == Right(GameStatus.Timeout),
      )
    },
    test("TournamentFormat all variants decode") {
      assertTrue(
        "\"swiss\"".fromJson[TournamentFormat] == Right(TournamentFormat.Swiss),
        "\"singleElimination\"".fromJson[TournamentFormat] == Right(TournamentFormat.SingleElimination),
        "\"doubleElimination\"".fromJson[TournamentFormat] == Right(TournamentFormat.DoubleElimination),
        "\"groupStage\"".fromJson[TournamentFormat] == Right(TournamentFormat.GroupStage(4)),
        "\"league\"".fromJson[TournamentFormat] == Right(TournamentFormat.League),
      )
    },
    test("StartPosition FromFen encodes and decodes") {
      val sp = StartPosition.FromFen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1")
      val json = sp.toJson
      assertTrue(json.fromJson[StartPosition] == Right(sp))
    },
    test("Color black encoder") {
      assertTrue(Color.Black.toJson == "\"black\"")
    },
    test("GameEvent GameState with black turn and black winner") {
      val clock = GameClock(100, 200)
      val e = GameEvent.GameState("fen", "e2e4", Color.Black, clock, GameStatus.Checkmate, Some(Color.Black))
      val json = e.toJson
      assertTrue(
        json.contains("\"turn\":\"black\""),
        json.contains("\"winner\":\"black\""),
      )
    },
    test("GameEvent GameState with white turn and no winner") {
      val clock = GameClock(100, 200)
      val e = GameEvent.GameState("fen", "", Color.White, clock, GameStatus.Ongoing, None)
      val json = e.toJson
      assertTrue(
        json.contains("\"turn\":\"white\""),
        json.contains("\"winner\":null"),
      )
    },
    test("GameEvent MovePlayed with white turn") {
      val clock = GameClock(100, 200)
      val e = GameEvent.MovePlayed("e2e4", "fen", Color.White, clock)
      val json = e.toJson
      assertTrue(json.contains("\"turn\":\"white\""))
    },
    test("GameEvent GameEnd with black winner") {
      val e = GameEvent.GameEnd(Some(Color.Black), GameStatus.Checkmate)
      val json = e.toJson
      assertTrue(json.contains("\"winner\":\"black\""))
    },
    test("GameEvent GameEnd with no winner") {
      val e = GameEvent.GameEnd(None, GameStatus.Stalemate)
      val json = e.toJson
      assertTrue(json.contains("\"winner\":null"))
    },
    test("Tournament JSON with winner") {
      val t = nowchess.tournament.domain.tournament.Tournament(
        id = TournamentId("t1"),
        config = TournamentConfig("W", 1, nowchess.tournament.domain.model.Clock(60, 0), true, TournamentFormat.Swiss, StartPosition.Standard, 1),
        status = TournamentStatus.Finished,
        participants = Vector(BotRef(BotId("b1"), "B1")),
        rounds = Vector.empty,
        currentRound = 0,
        director = UserId("d1"),
        createdAt = java.time.Instant.now,
        startedAt = None,
        winner = Some(BotRef(BotId("b1"), "B1")),
      )
      val json = t.toJson
      assertTrue(json.contains("\"winner\":{"))
    },
    test("Tournament JSON with FromFen start position") {
      val t = nowchess.tournament.domain.tournament.Tournament(
        id = TournamentId("t1"),
        config = TournamentConfig("F", 1, nowchess.tournament.domain.model.Clock(60, 0), true, TournamentFormat.Swiss, StartPosition.FromFen("custom fen"), 1),
        status = TournamentStatus.Created,
        participants = Vector.empty,
        rounds = Vector.empty,
        currentRound = 0,
        director = UserId("d1"),
        createdAt = java.time.Instant.now,
        startedAt = None,
        winner = None,
      )
      val json = t.toJson
      assertTrue(json.contains("custom fen"))
    },
    test("Game JSON with black winner and FromFen") {
      import nowchess.tournament.domain.game.Game
      val g = Game(
        id = GameId("g1"), tournamentId = TournamentId("t1"), round = 1,
        white = BotRef(BotId("w"), "W"), black = BotRef(BotId("b"), "B"),
        moves = Vector("e2e4"), status = GameStatus.Checkmate,
        clock = GameClock(100, 200), startPosition = StartPosition.FromFen("fen"),
        winner = Some(Color.Black), turn = Color.White, fen = "fen",
      )
      val json = g.toJson
      assertTrue(
        json.contains("\"winner\":\"black\""),
        json.contains("\"turn\":\"white\""),
      )
    },
  )
