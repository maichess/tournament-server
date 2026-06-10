package nowchess.tournament.domain.pairing

import zio.test.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.round.*

object RandomKnockoutPairingSpec extends ZIOSpecDefault:

  private def bot(n: Int) = BotRef(BotId(s"b$n"), s"Bot$n")

  def spec = suite("RandomKnockoutPairing")(
    test("empty with less than 2 participants") {
      val algo = RandomKnockoutPairing(42L)
      assertTrue(algo.pair(Vector(bot(1)), Vector.empty, Vector.empty, 1).isEmpty)
    },
    test("first round draws every participant exactly once") {
      val algo = RandomKnockoutPairing(42L)
      val players = (1 to 4).map(bot).toVector
      val pairs = algo.pair(players, Vector.empty, Vector.empty, 1)
      val drawn = pairs.flatMap((a, b) => Vector(a.id, b.id)).toSet
      assertTrue(pairs.size == 2) &&
      assertTrue(drawn == players.map(_.id).toSet)
    },
    test("two players produce a single first-round pairing") {
      val algo = RandomKnockoutPairing(7L)
      val pairs = algo.pair(Vector(bot(1), bot(2)), Vector.empty, Vector.empty, 1)
      assertTrue(pairs.size == 1)
    },
    test("different seeds can produce different first-round draws") {
      val players = (1 to 8).map(bot).toVector
      val a = RandomKnockoutPairing(1L).pair(players, Vector.empty, Vector.empty, 1)
      val b = RandomKnockoutPairing(999L).pair(players, Vector.empty, Vector.empty, 1)
      assertTrue(a != b)
    },
    test("a fixed seed is deterministic") {
      val players = (1 to 8).map(bot).toVector
      val a = RandomKnockoutPairing(123L).pair(players, Vector.empty, Vector.empty, 1)
      val b = RandomKnockoutPairing(123L).pair(players, Vector.empty, Vector.empty, 1)
      assertTrue(a == b)
    },
    test("later rounds advance the winners") {
      val algo = RandomKnockoutPairing(42L)
      val round1 = Round(1, Vector(
        Pairing(bot(1), bot(4), Vector.empty, Some(GameOutcome.White)),
        Pairing(bot(2), bot(3), Vector.empty, Some(GameOutcome.Black)),
      ))
      val players = (1 to 4).map(bot).toVector
      val pairs = algo.pair(players, Vector.empty, Vector(round1), 2)
      assertTrue(pairs.size == 1) &&
      assertTrue(pairs(0) == (bot(1), bot(3)))
    },
    test("totalRounds mirrors elimination bracket") {
      assertTrue(RandomKnockoutPairing.totalRounds(8) == 3) &&
      assertTrue(RandomKnockoutPairing.totalRounds(4) == 2)
    },
  )
