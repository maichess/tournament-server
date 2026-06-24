package tournament.http

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.json.*
import tournament.domain.model.*
import tournament.domain.tournament.*
import tournament.domain.round.{Round, Pairing, Match as GameMatch}
import tournament.domain.standing.{Standing, Result}
import tournament.domain.game.{Game, GameStatus, GameClock}
import tournament.domain.event.{TournamentEvent, GameEvent}
import tournament.domain.error.DomainError
import tournament.http.codec.JsonCodecs
import tournament.http.codec.JsonCodecs.{*, given}

object JsonCodecsSpec extends ZIOSpecDefault:

  def spec = suite("JsonCodecs")(
    test("TournamentId round-trips"):
      val id = TournamentId("t1")
      assertTrue(id.toJson.fromJson[TournamentId] == Right(id))
    ,
    test("BotId round-trips"):
      val id = BotId("b1")
      assertTrue(id.toJson.fromJson[BotId] == Right(id))
    ,
    test("UserId round-trips"):
      val id = UserId("u1")
      assertTrue(id.toJson.fromJson[UserId] == Right(id))
    ,
    test("GameId round-trips"):
      val id = GameId("g1")
      assertTrue(id.toJson.fromJson[GameId] == Right(id))
    ,
    test("Color round-trips"):
      assertTrue(
        Color.White.toJson == "\"white\"",
        Color.Black.toJson == "\"black\"",
        "\"white\"".fromJson[Color] == Right(Color.White),
        "\"black\"".fromJson[Color] == Right(Color.Black),
        "\"red\"".fromJson[Color].isLeft,
      )
    ,
    test("GameOutcome round-trips"):
      assertTrue(
        GameOutcome.White.toJson == "\"white\"",
        GameOutcome.Draw.toJson == "\"draw\"",
        "\"black\"".fromJson[GameOutcome] == Right(GameOutcome.Black),
        "\"invalid\"".fromJson[GameOutcome].isLeft,
      )
    ,
    test("GameStatus encodes and decodes"):
      assertTrue(
        GameStatus.Ongoing.toJson == "\"ongoing\"",
        GameStatus.Checkmate.toJson == "\"checkmate\"",
        GameStatus.Stalemate.toJson == "\"stalemate\"",
        GameStatus.Draw.toJson == "\"draw\"",
        GameStatus.Resigned.toJson == "\"resigned\"",
        GameStatus.Timeout.toJson == "\"timeout\"",
        "\"ongoing\"".fromJson[GameStatus] == Right(GameStatus.Ongoing),
        "\"bad\"".fromJson[GameStatus].isLeft,
      )
    ,
    test("TournamentStatus encodes"):
      assertTrue(
        TournamentStatus.Created.toJson == "\"created\"",
        TournamentStatus.Started.toJson == "\"started\"",
        TournamentStatus.Finished.toJson == "\"finished\"",
      )
    ,
    test("TournamentFormat encodes and decodes"):
      assertTrue(
        TournamentFormat.Swiss.toJson == "\"swiss\"",
        TournamentFormat.SingleElimination.toJson == "\"singleElimination\"",
        TournamentFormat.DoubleElimination.toJson == "\"doubleElimination\"",
        (TournamentFormat.GroupStage(4): TournamentFormat).toJson == "\"groupStage\"",
        TournamentFormat.League.toJson == "\"league\"",
        "\"swiss\"".fromJson[TournamentFormat] == Right(TournamentFormat.Swiss),
        "\"bad\"".fromJson[TournamentFormat].isLeft,
      )
    ,
    test("StartPosition encodes and decodes"):
      assertTrue(
        StartPosition.Standard.toJson == "\"standard\"",
        "\"standard\"".fromJson[StartPosition] == Right(StartPosition.Standard),
      )
    ,
    test("Clock round-trips"):
      val c = tournament.domain.model.Clock(300, 5)
      val json = c.toJson
      assertTrue(json.fromJson[tournament.domain.model.Clock] == Right(c))
    ,
    test("BotRef round-trips"):
      val b = BotRef(BotId("b1"), "Bot1")
      val json = b.toJson
      assertTrue(json.fromJson[BotRef] == Right(b))
    ,
    test("DomainError encodes"):
      val e: DomainError = DomainError.NotFound("not found")
      assertTrue(e.toJson.contains("not found"))
    ,
    test("OkResponse encodes"):
      val ok = JsonCodecs.OkResponse(true)
      assertTrue(ok.toJson == """{"ok":true}""")
    ,
    test("TournamentEvent encodes all variants"):
      val started = TournamentEvent.TournamentStarted
      val roundStarted = TournamentEvent.RoundStarted(1)
      val gameStart = TournamentEvent.GameStart(1, GameId("g1"), Color.White, BotId("b1"))
      val roundFinished = TournamentEvent.RoundFinished(1)
      val finished = TournamentEvent.TournamentFinished(BotRef(BotId("b1"), "Winner"))
      assertTrue(
        started.toJson.contains("tournamentStarted"),
        roundStarted.toJson.contains("roundStarted"),
        gameStart.toJson.contains("gameStart"),
        roundFinished.toJson.contains("roundFinished"),
        finished.toJson.contains("tournamentFinished"),
      )
    ,
    test("GameEvent encodes all variants"):
      val clock = GameClock(300, 300)
      val state = GameEvent.GameState("fen", "e2e4", Color.White, clock, GameStatus.Ongoing, None)
      val move = GameEvent.MovePlayed("e2e4", "fen", Color.Black, clock)
      val end = GameEvent.GameEnd(Some(Color.White), GameStatus.Checkmate)
      val endNone = GameEvent.GameEnd(None, GameStatus.Stalemate)
      assertTrue(
        state.toJson.contains("gameState"),
        move.toJson.contains("move"),
        end.toJson.contains("gameEnd"),
        end.toJson.contains("white"),
        endNone.toJson.contains("null"),
      )
    ,
    test("Tournament encodes"):
      val t = Tournament(
        id = TournamentId("t1"),
        config = TournamentConfig("Test", 2, tournament.domain.model.Clock(300, 5), rated = true, TournamentFormat.Swiss, StartPosition.Standard, 1),
        status = TournamentStatus.Created,
        participants = Vector(BotRef(BotId("b1"), "B1")),
        rounds = Vector.empty,
        currentRound = 0,
        director = UserId("d1"),
        createdAt = java.time.Instant.now,
        startedAt = None,
        winner = None,
      )
      val json = t.toJson
      assertTrue(
        json.contains("\"id\":\"t1\""),
        json.contains("\"fullName\":\"Test\""),
        json.contains("\"nbPlayers\":1"),
        json.contains("\"format\":\"swiss\""),
      )
    ,
    test("Game encodes"):
      val g = Game(
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
      val json = g.toJson
      assertTrue(
        json.contains("\"id\":\"g1\""),
        json.contains("\"status\":\"ongoing\""),
        json.contains("\"turn\":\"white\""),
      )
    ,
    test("Game encodes with winner"):
      val g = Game(
        id = GameId("g2"),
        tournamentId = TournamentId("t1"),
        round = 1,
        white = BotRef(BotId("w"), "White"),
        black = BotRef(BotId("b"), "Black"),
        moves = Vector.empty,
        status = GameStatus.Checkmate,
        clock = GameClock(300, 300),
        startPosition = StartPosition.FromFen("custom"),
        winner = Some(Color.White),
        turn = Color.Black,
        fen = "some fen",
      )
      val json = g.toJson
      assertTrue(
        json.contains("\"winner\":\"white\""),
        json.contains("\"startPosition\":\"custom\""),
      )
    ,
    test("GameExportJson encodes"):
      val e = JsonCodecs.GameExportJson("g1", 1, BotRef(BotId("w"), "W"), BotRef(BotId("b"), "B"), Some("white"), "e2e4")
      assertTrue(e.toJson.contains("\"id\":\"g1\""))
    ,
    test("RoundResponse encodes"):
      val r = JsonCodecs.RoundResponse(1, Vector.empty)
      assertTrue(r.toJson.contains("\"round\":1"))
    ,
    test("TournamentListResponse encodes"):
      val r = JsonCodecs.TournamentListResponse(Vector.empty, Vector.empty, Vector.empty)
      assertTrue(r.toJson.contains("\"created\":[]"))
    ,
    test("Match encodes"):
      val m = GameMatch(GameId("g1"), BotId("w"), None, None)
      assertTrue(m.toJson.contains("g1"))
    ,
    test("Pairing encodes"):
      val p = Pairing(
        BotRef(BotId("w"), "W"),
        BotRef(BotId("b"), "B"),
        Vector.empty,
        None,
      )
      assertTrue(p.toJson.contains("\"W\""))
    ,
    test("Round encodes"):
      val r = Round(1, Vector.empty)
      assertTrue(r.toJson.contains("\"number\":1"))
    ,
    test("Result encodes"):
      val r = Result(1, 3.0, 0.0, BotRef(BotId("b"), "B"), 1, 1, 0, 0)
      assertTrue(r.toJson.contains("\"rank\":1"))
    ,
    test("Standing encodes"):
      val s = Standing(1, Vector.empty)
      assertTrue(s.toJson.contains("\"page\":1"))
    ,
    test("TournamentConfig encodes"):
      val c = TournamentConfig("N", 2, tournament.domain.model.Clock(300, 5), true, TournamentFormat.Swiss, StartPosition.Standard, 1)
      assertTrue(c.toJson.contains("\"name\":\"N\""))
    ,
    test("Variant encodes"):
      val v = Variant("standard", "Standard")
      val json = v.toJson
      assertTrue(json.fromJson[Variant] == Right(v))
    ,
    test("GameClock round-trips"):
      val gc = GameClock(300, 300)
      val json = gc.toJson
      assertTrue(json.fromJson[GameClock] == Right(gc))
    ,
    test("Instant encodes as an ISO-8601 string"):
      assertTrue(java.time.Instant.EPOCH.toJson == "\"1970-01-01T00:00:00Z\"")
    ,
    test("Game encodes startedAt, endedAt and the clock increment"):
      val g = Game(
        id = GameId("g3"),
        tournamentId = TournamentId("t1"),
        round = 1,
        white = BotRef(BotId("w"), "White"),
        black = BotRef(BotId("b"), "Black"),
        moves = Vector.empty,
        status = GameStatus.Timeout,
        clock = GameClock(300, 300, 5),
        startPosition = StartPosition.Standard,
        winner = Some(Color.White),
        turn = Color.Black,
        fen = "some fen",
        startedAt = Some(java.time.Instant.EPOCH),
        endedAt = Some(java.time.Instant.EPOCH),
      )
      val json = g.toJson
      assertTrue(
        json.contains("\"increment\":5"),
        json.contains("\"startedAt\":\"1970-01-01T00:00:00Z\""),
        json.contains("\"endedAt\":\"1970-01-01T00:00:00Z\""),
      )
    ,
    test("TournamentFinished with winner"):
      val e = TournamentEvent.TournamentFinished(BotRef(BotId("w1"), "Winner"))
      assertTrue(e.toJson.contains("Winner"))
    ,
    test("GameState with winner"):
      val clock = GameClock(300, 300)
      val e = GameEvent.GameState("fen", "e2e4", Color.Black, clock, GameStatus.Checkmate, Some(Color.Black))
      assertTrue(e.toJson.contains("\"black\""))
    ,
  )
