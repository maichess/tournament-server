package nowchess.tournament.http

import zio.test.*
import zio.json.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.game.{GameStatus, GameClock}
import nowchess.tournament.domain.event.{TournamentEvent, GameEvent}
import nowchess.tournament.http.codec.JsonCodecs.given

object JsonCodecsCoverageSpec extends ZIOSpecDefault:

  def spec = suite("JsonCodecs coverage")(
    test("GameOutcome.Black encoder") {
      assertTrue(GameOutcome.Black.toJson == "\"black\"")
    },
    test("GameStart with Color.Black encoder") {
      val event: TournamentEvent = TournamentEvent.GameStart(1, GameId("g1"), Color.Black)
      val json = event.toJson
      assertTrue(json.contains("\"black\""))
    },
    test("GameState with winner None encodes null") {
      val event: GameEvent = GameEvent.GameState(
        "fen", "e2e4", Color.White, GameClock(300, 300), GameStatus.Ongoing, None
      )
      val json = event.toJson
      assertTrue(json.contains("\"winner\":null"))
    },
    test("GameState with black turn") {
      val event: GameEvent = GameEvent.GameState(
        "fen", "e2e4", Color.Black, GameClock(300, 300), GameStatus.Ongoing, None
      )
      val json = event.toJson
      assertTrue(json.contains("\"turn\":\"black\""))
    },
    test("MovePlayed with black turn") {
      val event: GameEvent = GameEvent.MovePlayed("e7e5", "fen", Color.Black, GameClock(300, 295))
      val json = event.toJson
      assertTrue(json.contains("\"turn\":\"black\""))
    },
    test("GameEnd with white winner") {
      val event: GameEvent = GameEvent.GameEnd(Some(Color.White), GameStatus.Checkmate)
      val json = event.toJson
      assertTrue(json.contains("\"white\""))
    },
    test("GameEnd with no winner") {
      val event: GameEvent = GameEvent.GameEnd(None, GameStatus.Stalemate)
      val json = event.toJson
      assertTrue(json.contains("\"winner\":null"))
    },
    test("GameState with winner Some(Black) encodes black") {
      val event: GameEvent = GameEvent.GameState(
        "fen", "e2e4", Color.White, GameClock(300, 300), GameStatus.Checkmate, Some(Color.Black)
      )
      val json = event.toJson
      assertTrue(json.contains("\"winner\":\"black\""))
    },
    test("GameState with winner Some(White) encodes white") {
      val event: GameEvent = GameEvent.GameState(
        "fen", "e2e4", Color.Black, GameClock(300, 300), GameStatus.Checkmate, Some(Color.White)
      )
      val json = event.toJson
      assertTrue(json.contains("\"winner\":\"white\""))
    },
  )
