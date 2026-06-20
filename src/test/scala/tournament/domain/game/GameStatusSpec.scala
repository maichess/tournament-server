package tournament.domain.game

import zio.test.*

object GameStatusFlagsSpec extends ZIOSpecDefault:

  def spec = suite("GameStatus")(
    test("pending is not terminal") {
      assertTrue(!GameStatus.Pending.isTerminal)
    },
    test("ongoing is not terminal") {
      assertTrue(!GameStatus.Ongoing.isTerminal)
    },
    test("decisive and drawn statuses are terminal") {
      assertTrue(
        GameStatus.Checkmate.isTerminal,
        GameStatus.Stalemate.isTerminal,
        GameStatus.Draw.isTerminal,
        GameStatus.Resigned.isTerminal,
        GameStatus.Timeout.isTerminal,
      )
    },
  )
