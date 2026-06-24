package tournament.domain.model

import zio.test.*

object ModelSpec extends ZIOSpecDefault:

  def spec = suite("Model types")(
    suite("Color")(
      test("White opposite is Black") {
        assertTrue(Color.White.opposite == Color.Black)
      },
      test("Black opposite is White") {
        assertTrue(Color.Black.opposite == Color.White)
      },
    ),
    suite("StartPosition")(
      test("Standard toFen returns standard FEN") {
        assertTrue(StartPosition.Standard.toFen == StartPosition.standardFen)
      },
      test("FromFen toFen returns the FEN") {
        val fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
        assertTrue(StartPosition.FromFen(fen).toFen == fen)
      },
      test("fromString parses 'standard'") {
        assertTrue(StartPosition.fromString("standard") == StartPosition.Standard)
      },
      test("fromString parses empty string") {
        assertTrue(StartPosition.fromString("") == StartPosition.Standard)
      },
      test("fromString parses FEN") {
        val fen = "8/8/8/8/8/8/8/8 w - - 0 1"
        assertTrue(StartPosition.fromString(fen) == StartPosition.FromFen(fen))
      },
    ),
    suite("opaque IDs")(
      test("TournamentId roundtrip") {
        val id = TournamentId("abc")
        assertTrue(id.value == "abc")
      },
      test("BotId roundtrip") {
        val id = BotId("bot1")
        assertTrue(id.value == "bot1")
      },
      test("UserId roundtrip") {
        val id = UserId("user1")
        assertTrue(id.value == "user1")
      },
      test("GameId roundtrip") {
        val id = GameId("game1")
        assertTrue(id.value == "game1")
      },
    ),
    suite("Variant")(
      test("standard variant") {
        assertTrue(Variant.standard.key == "standard") &&
        assertTrue(Variant.standard.name == "Standard")
      },
    ),
    suite("DomainError")(
      test("all error variants carry message") {
        import tournament.domain.error.DomainError
        assertTrue(DomainError.NotFound("x").message == "x") &&
        assertTrue(DomainError.Conflict("y").message == "y") &&
        assertTrue(DomainError.Forbidden("z").message == "z") &&
        assertTrue(DomainError.BadRequest("a").message == "a") &&
        assertTrue(DomainError.Unauthorized("b").message == "b")
      },
    ),
    suite("TournamentEvent")(
      test("all event variants construct correctly") {
        import tournament.domain.event.TournamentEvent
        val e1 = TournamentEvent.TournamentStarted
        val e2 = TournamentEvent.RoundStarted(1)
        val e3 = TournamentEvent.GameStart(1, GameId("g1"), Color.White, BotId("b1"))
        val e4 = TournamentEvent.RoundFinished(1)
        val bot = BotRef(BotId("b"), "B")
        val e5 = TournamentEvent.TournamentFinished(bot)
        assertTrue(e2 == TournamentEvent.RoundStarted(1)) &&
        assertTrue(e3 == TournamentEvent.GameStart(1, GameId("g1"), Color.White, BotId("b1"))) &&
        assertTrue(e4 == TournamentEvent.RoundFinished(1)) &&
        assertTrue(e5 == TournamentEvent.TournamentFinished(bot))
      },
    ),
    suite("GameEvent")(
      test("all game event variants construct correctly") {
        import tournament.domain.event.GameEvent
        import tournament.domain.game.{GameStatus, GameClock}
        val gs = GameEvent.GameState("fen", "e2e4", Color.White, GameClock(300, 300), GameStatus.Ongoing, None)
        val mv = GameEvent.MovePlayed("e2e4", "fen", Color.Black, GameClock(299, 300))
        val ge = GameEvent.GameEnd(Some(Color.White), GameStatus.Checkmate)
        assertTrue(gs == GameEvent.GameState("fen", "e2e4", Color.White, GameClock(300, 300), GameStatus.Ongoing, None)) &&
        assertTrue(mv == GameEvent.MovePlayed("e2e4", "fen", Color.Black, GameClock(299, 300))) &&
        assertTrue(ge == GameEvent.GameEnd(Some(Color.White), GameStatus.Checkmate))
      },
    ),
    suite("TournamentFormat")(
      test("all format variants") {
        import tournament.domain.tournament.TournamentFormat
        val s = TournamentFormat.Swiss
        val se = TournamentFormat.SingleElimination
        val de = TournamentFormat.DoubleElimination
        val gs = TournamentFormat.GroupStage(4)
        val l = TournamentFormat.League
        assertTrue(gs == TournamentFormat.GroupStage(4)) &&
        assertTrue(s != se) &&
        assertTrue(de != l)
      },
    ),
    suite("TournamentStatus")(
      test("all status variants") {
        import tournament.domain.tournament.TournamentStatus
        assertTrue(TournamentStatus.Created != TournamentStatus.Started) &&
        assertTrue(TournamentStatus.Started != TournamentStatus.Finished)
      },
    ),
    suite("Standing")(
      test("constructs correctly") {
        import tournament.domain.standing.{Standing, Result}
        val r = Result(1, 3.5, 9.0, BotRef(BotId("b"), "B"), 4, 3, 1, 0)
        val s = Standing(1, Vector(r))
        assertTrue(s.page == 1) &&
        assertTrue(s.players.size == 1) &&
        assertTrue(r.rank == 1) &&
        assertTrue(r.tieBreak == 9.0)
      },
    ),
    suite("Clock")(
      test("constructs correctly") {
        val c = Clock(300, 3)
        assertTrue(c.limit == 300) &&
        assertTrue(c.increment == 3)
      },
    ),
    suite("BotRef")(
      test("constructs correctly") {
        val b = BotRef(BotId("b1"), "Bot1")
        assertTrue(b.id == BotId("b1")) &&
        assertTrue(b.name == "Bot1")
      },
    ),
    suite("GameOutcome")(
      test("all variants exist") {
        assertTrue(GameOutcome.White != GameOutcome.Black) &&
        assertTrue(GameOutcome.Draw != GameOutcome.White)
      },
    ),
    suite("Match")(
      test("constructs correctly") {
        import tournament.domain.round.Match
        val m = Match(GameId("g1"), BotId("w"), Some(GameOutcome.White), Some("e2e4"))
        assertTrue(m.gameId == GameId("g1")) &&
        assertTrue(m.whiteId == BotId("w")) &&
        assertTrue(m.outcome.contains(GameOutcome.White)) &&
        assertTrue(m.moves.contains("e2e4"))
      },
    ),
    suite("TournamentConfig")(
      test("constructs correctly") {
        import tournament.domain.tournament.*
        val c = TournamentConfig("Test", 5, Clock(300, 3), true,
          TournamentFormat.Swiss, StartPosition.Standard, 1)
        assertTrue(c.name == "Test") &&
        assertTrue(c.matchesPerPairing == 1)
      },
    ),
    suite("GameClock")(
      test("constructs correctly") {
        import tournament.domain.game.GameClock
        val gc = GameClock(300.0, 300.0)
        assertTrue(gc.whiteTime == 300.0) &&
        assertTrue(gc.blackTime == 300.0)
      },
    ),
  )
