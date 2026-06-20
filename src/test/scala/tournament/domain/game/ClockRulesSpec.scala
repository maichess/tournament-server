package tournament.domain.game

import zio.test.*
import tournament.domain.model.Color
import java.time.Instant

object ClockRulesSpec extends ZIOSpecDefault:

  private val t0  = Instant.EPOCH
  private val t10 = Instant.EPOCH.plusSeconds(10)

  def spec = suite("ClockRules")(
    test("elapsedSeconds is the gap between the two instants"):
      assertTrue(ClockRules.elapsedSeconds(t0, t10) == 10.0)
    ,
    test("elapsedSeconds never goes negative if the clock appears to move backwards"):
      assertTrue(ClockRules.elapsedSeconds(t10, t0) == 0.0)
    ,
    test("hasFlagged is true once the time spent exceeds the time remaining"):
      assertTrue(ClockRules.hasFlagged(GameClock(5.0, 60.0, 2), Color.White, t0, t10))
    ,
    test("hasFlagged is false while time remains, ignoring the increment"):
      assertTrue(!ClockRules.hasFlagged(GameClock(60.0, 60.0, 2), Color.White, t0, t10))
    ,
    test("applyMove deducts the spent time and then credits the increment"):
      val (updated, flagged) = ClockRules.applyMove(GameClock(60.0, 30.0, 2), Color.White, t0, t10)
      // white: 60 - 10 + 2 = 52; black untouched
      assertTrue(!flagged, updated.whiteTime == 52.0, updated.blackTime == 30.0)
    ,
    test("applyMove settles the black clock when it is black to move"):
      val (updated, flagged) = ClockRules.applyMove(GameClock(60.0, 40.0, 1), Color.Black, t0, t10)
      assertTrue(!flagged, updated.blackTime == 31.0, updated.whiteTime == 60.0)
    ,
    test("applyMove flags and zeroes the clock without crediting the increment"):
      val (updated, flagged) = ClockRules.applyMove(GameClock(5.0, 30.0, 2), Color.White, t0, t10)
      assertTrue(flagged, updated.whiteTime == 0.0)
  )
