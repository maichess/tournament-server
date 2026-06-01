package nowchess.tournament.domain.pairing

import zio.test.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.standing.Result
import nowchess.tournament.domain.round.*

object SwissPairingSpec extends ZIOSpecDefault:

  private def bot(n: Int) = BotRef(BotId(s"b$n"), s"Bot$n")
  private val bots4 = (1 to 4).map(bot).toVector
  private val bots3 = (1 to 3).map(bot).toVector

  private def mkStandings(points: (Int, Double)*): Vector[Result] =
    points.toVector.map: (n, pts) =>
      Result(0, pts, 0, bot(n), 0, 0, 0, 0)

  def spec = suite("SwissPairing")(
    test("pairs 4 players into 2 pairs") {
      val pairs = SwissPairing.pair(bots4, Vector.empty, Vector.empty, 1)
      assertTrue(pairs.size == 2) &&
      assertTrue(pairs.flatMap((a, b) => Vector(a, b)).distinct.size == 4)
    },
    test("pairs 3 players into 1 pair (odd count, one bye)") {
      val pairs = SwissPairing.pair(bots3, Vector.empty, Vector.empty, 1)
      assertTrue(pairs.size == 1)
    },
    test("sorts by standings, top-ranked paired first") {
      val standings = mkStandings((1, 0.0), (2, 2.0), (3, 1.0), (4, 3.0))
      val pairs = SwissPairing.pair(bots4, standings, Vector.empty, 2)
      // Sorted: bot4(3.0), bot2(2.0), bot3(1.0), bot1(0.0)
      assertTrue(pairs.size == 2) &&
      assertTrue(pairs(0) == (bot(4), bot(2))) &&
      assertTrue(pairs(1) == (bot(3), bot(1)))
    },
    test("avoids rematches when possible") {
      val prevPairing = Pairing(bot(1), bot(2), Vector.empty, Some(GameOutcome.White))
      val prevRound = Round(1, Vector(prevPairing))
      val pairs = SwissPairing.pair(bots4, Vector.empty, Vector(prevRound), 2)
      val hasPrevMatch = pairs.exists((a, b) =>
        (a.id == bot(1).id && b.id == bot(2).id) || (a.id == bot(2).id && b.id == bot(1).id))
      assertTrue(!hasPrevMatch)
    },
    test("falls back to rematch if unavoidable") {
      val bots2 = Vector(bot(1), bot(2))
      val prevPairing = Pairing(bot(1), bot(2), Vector.empty, Some(GameOutcome.White))
      val prevRound = Round(1, Vector(prevPairing))
      val pairs = SwissPairing.pair(bots2, Vector.empty, Vector(prevRound), 2)
      assertTrue(pairs.size == 1)
    },
    test("empty with less than 2 participants") {
      assertTrue(SwissPairing.pair(Vector(bot(1)), Vector.empty, Vector.empty, 1).isEmpty) &&
      assertTrue(SwissPairing.pair(Vector.empty, Vector.empty, Vector.empty, 1).isEmpty)
    },
  )
