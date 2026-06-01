package nowchess.tournament.domain.pairing

import zio.test.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.standing.Result
import nowchess.tournament.domain.round.*

object GroupStagePairingSpec extends ZIOSpecDefault:

  private def bot(n: Int) = BotRef(BotId(s"b$n"), s"Bot$n")

  def spec = suite("GroupStagePairing")(
    test("assigns groups correctly") {
      val players = (1 to 8).map(bot).toVector
      val groups = GroupStagePairing.assignGroups(players, 4)
      assertTrue(groups.size == 2) &&
      assertTrue(groups(0).size == 4) &&
      assertTrue(groups(1).size == 4)
    },
    test("group phase rounds = round-robin rounds for group size") {
      assertTrue(GroupStagePairing.groupPhaseRounds(4) == 3)
    },
    test("group phase produces pairings within groups") {
      val players = (1 to 8).map(bot).toVector
      val pairs = GroupStagePairing.pair(players, Vector.empty, Vector.empty, 1, groupSize = 4)
      // 2 groups of 4, round-robin round 1: 2 pairs per group = 4 total
      assertTrue(pairs.size == 4)
    },
    test("selects top from each group for knockout") {
      val players = (1 to 4).map(bot).toVector
      val groups = Vector(Vector(bot(1), bot(2)), Vector(bot(3), bot(4)))
      val standings = Vector(
        Result(1, 2.0, 0, bot(1), 1, 1, 0, 0),
        Result(2, 0.0, 0, bot(2), 1, 0, 0, 1),
        Result(1, 2.0, 0, bot(3), 1, 1, 0, 0),
        Result(2, 0.0, 0, bot(4), 1, 0, 0, 1),
      )
      val winners = GroupStagePairing.selectGroupWinners(groups, standings)
      assertTrue(winners.size == 2) &&
      assertTrue(winners(0) == bot(1)) &&
      assertTrue(winners(1) == bot(3))
    },
    test("empty with less than 2") {
      assertTrue(GroupStagePairing.pair(Vector(bot(1)), Vector.empty, Vector.empty, 1, 4).isEmpty)
    },
    test("knockout phase after group phase") {
      val players = (1 to 8).map(bot).toVector
      // Group phase has 3 rounds for groups of 4
      // Round 4 is knockout round 1
      val standings = (1 to 8).map(n => Result(n, (9 - n).toDouble, 0, bot(n), 3, 0, 0, 0)).toVector
      // Need completed group rounds to compute knockout
      // For round 4, knockout round 1, it selects group winners from standings
      val pairs = GroupStagePairing.pair(players, standings, Vector.empty, 4, groupSize = 4)
      // Each group of 4 sends top 2, so 4 qualifiers → 2 pairs
      assertTrue(pairs.size == 2)
    },
  )
