package nowchess.tournament.domain.game

import zio.test.*
import nowchess.tournament.domain.model.GameId

object GameSchedulerSpec extends ZIOSpecDefault:

  private def gid(n: Int) = GameId(s"g$n")
  private val pending = Vector(gid(1), gid(2), gid(3))

  def spec = suite("GameScheduler")(
    test("no cap activates everything pending") {
      assertTrue(GameScheduler.toActivate(0, pending, None) == pending)
    },
    test("cap fills remaining slots up to the limit") {
      assertTrue(GameScheduler.toActivate(0, pending, Some(2)) == Vector(gid(1), gid(2)))
    },
    test("cap accounts for games already running") {
      assertTrue(GameScheduler.toActivate(1, pending, Some(2)) == Vector(gid(1)))
    },
    test("no slots free when at the cap") {
      assertTrue(GameScheduler.toActivate(2, pending, Some(2)).isEmpty)
    },
    test("never returns a negative slice when over the cap") {
      assertTrue(GameScheduler.toActivate(5, pending, Some(2)).isEmpty)
    },
    test("empty pending queue yields nothing") {
      assertTrue(GameScheduler.toActivate(0, Vector.empty, Some(3)).isEmpty)
    },
  )
