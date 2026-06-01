package nowchess.tournament.domain.pairing

import zio.test.*
import nowchess.tournament.domain.model.*

object RoundRobinPairingSpec extends ZIOSpecDefault:

  private def bot(n: Int) = BotRef(BotId(s"b$n"), s"Bot$n")

  def spec = suite("RoundRobinPairing")(
    test("4 players need 3 rounds") {
      assertTrue(RoundRobinPairing.totalRounds(4) == 3)
    },
    test("3 players need 3 rounds (odd → padded to 4)") {
      assertTrue(RoundRobinPairing.totalRounds(3) == 3)
    },
    test("2 players need 1 round") {
      assertTrue(RoundRobinPairing.totalRounds(2) == 1)
    },
    test("1 player needs 0 rounds") {
      assertTrue(RoundRobinPairing.totalRounds(1) == 0)
    },
    test("4 players: each round has 2 pairings") {
      val players = (1 to 4).map(bot).toVector
      for round <- 1 to 3 do
        val pairs = RoundRobinPairing.pairingsForRound(players, round)
        assert(pairs.size == 2)(Assertion.isTrue)
      assertCompletes
    },
    test("4 players: all pairs appear across all rounds") {
      val players = (1 to 4).map(bot).toVector
      val allPairs = (1 to 3).flatMap(r => RoundRobinPairing.pairingsForRound(players, r))
      val pairSet = allPairs.map((a, b) => Set(a.id, b.id)).toSet
      // C(4,2) = 6 unique pairs
      assertTrue(pairSet.size == 6)
    },
    test("3 players: produces 1 pairing per round (one bye)") {
      val players = (1 to 3).map(bot).toVector
      for round <- 1 to 3 do
        val pairs = RoundRobinPairing.pairingsForRound(players, round)
        assert(pairs.size == 1)(Assertion.isTrue)
      assertCompletes
    },
    test("empty with less than 2") {
      assertTrue(RoundRobinPairing.pair(Vector(bot(1)), Vector.empty, Vector.empty, 1).isEmpty)
    },
  )
