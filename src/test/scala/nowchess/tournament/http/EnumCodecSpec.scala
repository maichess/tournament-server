package nowchess.tournament.http

import zio.test.*
import zio.json.*
import nowchess.tournament.domain.game.GameStatus
import nowchess.tournament.domain.tournament.TournamentFormat
import nowchess.tournament.http.codec.JsonCodecs.given

object EnumCodecSpec extends ZIOSpecDefault:

  def spec = suite("Enum codecs for new variants")(
    test("pending game status round-trips") {
      assertTrue(
        GameStatus.Pending.toJson == "\"pending\"",
        "\"pending\"".fromJson[GameStatus] == Right(GameStatus.Pending),
      )
    },
    test("randomKnockout format round-trips") {
      assertTrue(
        (TournamentFormat.RandomKnockout: TournamentFormat).toJson == "\"randomKnockout\"",
        "\"randomKnockout\"".fromJson[TournamentFormat] == Right(TournamentFormat.RandomKnockout),
      )
    },
  )
