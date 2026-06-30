package tournament.domain.round

import zio.test.*
import tournament.domain.model.*

object PairingSpec extends ZIOSpecDefault:

  private val bot1 = BotRef(BotId("b1"), "Bot1")
  private val bot2 = BotRef(BotId("b2"), "Bot2")

  private def mkPairing(numMatches: Int, outcomes: Vector[Option[GameOutcome]] = Vector.empty) =
    val matches = (0 until numMatches).toVector.map: i =>
      Match(GameId(s"g$i"), bot1.id, outcomes.lift(i).flatten, None)
    Pairing(bot1, bot2, matches, None)

  def spec = suite("Pairing")(
    suite("best-of-1")(
      test("not complete when no outcome") {
        val p = mkPairing(1)
        assertTrue(!p.isComplete(1))
      },
      test("complete with white win") {
        val p = mkPairing(1, Vector(Some(GameOutcome.White)))
        assertTrue(p.earlyWinner(1).contains(GameOutcome.White))
      },
      test("complete with draw") {
        val p = mkPairing(1, Vector(Some(GameOutcome.Draw)))
        assertTrue(p.earlyWinner(1).contains(GameOutcome.Draw))
      },
      test("no winner for non-positive matchesPerPairing") {
        val p = mkPairing(0)
        assertTrue(p.earlyWinner(0).isEmpty, !p.isComplete(0))
      },
    ),
    suite("best-of-3")(
      test("white wins 2-0 early") {
        val p = mkPairing(3, Vector(Some(GameOutcome.White), Some(GameOutcome.White)))
        assertTrue(p.earlyWinner(3).contains(GameOutcome.White))
      },
      test("not decided at 1-0") {
        val p = mkPairing(3, Vector(Some(GameOutcome.White)))
        assertTrue(p.earlyWinner(3).isEmpty)
      },
      test("black wins 2-1") {
        val p = mkPairing(3, Vector(
          Some(GameOutcome.White), Some(GameOutcome.Black), Some(GameOutcome.Black),
        ))
        assertTrue(p.earlyWinner(3).contains(GameOutcome.Black))
      },
      test("1-1-1 draws result in draw") {
        val p = mkPairing(3, Vector(
          Some(GameOutcome.White), Some(GameOutcome.Black), Some(GameOutcome.Draw),
        ))
        assertTrue(p.earlyWinner(3).contains(GameOutcome.Draw))
      },
      test("not complete at 1-1 with pending game") {
        val p = mkPairing(3, Vector(Some(GameOutcome.White), Some(GameOutcome.Black)))
        assertTrue(p.earlyWinner(3).isEmpty)
      },
    ),
    suite("best-of-5")(
      test("white wins 3-0 early") {
        val p = mkPairing(5, Vector(
          Some(GameOutcome.White), Some(GameOutcome.White), Some(GameOutcome.White),
        ))
        assertTrue(p.earlyWinner(5).contains(GameOutcome.White))
      },
      test("not decided at 2-2 with pending") {
        val p = mkPairing(5, Vector(
          Some(GameOutcome.White), Some(GameOutcome.Black),
          Some(GameOutcome.White), Some(GameOutcome.Black),
        ))
        assertTrue(p.earlyWinner(5).isEmpty)
      },
      test("3-2 win for black") {
        val p = mkPairing(5, Vector(
          Some(GameOutcome.White), Some(GameOutcome.Black),
          Some(GameOutcome.White), Some(GameOutcome.Black), Some(GameOutcome.Black),
        ))
        assertTrue(p.earlyWinner(5).contains(GameOutcome.Black))
      },
    ),
    suite("reversed colours")(
      test("earlyWinner normalises a reversed-colour game to the nominal white") {
        // bot1 is the pairing's nominal white. g1: bot1 plays white and wins
        // (White). g2: colours reversed, so bot2 is white and bot1 (black) wins
        // — recorded as Black but credited to the nominal white. So bot1 wins 2-0.
        val matches = Vector(
          Match(GameId("g1"), bot1.id, Some(GameOutcome.White), None),
          Match(GameId("g2"), bot2.id, Some(GameOutcome.Black), None),
        )
        val p = Pairing(bot1, bot2, matches, None)
        assertTrue(p.earlyWinner(2).contains(GameOutcome.White))
      },
      test("earlyWinner normalises a reversed-colour white win to the nominal black") {
        // g1: colours reversed (bot2 plays white) and white wins, i.e. bot2 won
        // — credited to the pairing's nominal black. So black takes the pairing.
        val matches = Vector(
          Match(GameId("g1"), bot2.id, Some(GameOutcome.White), None),
          Match(GameId("g2"), bot2.id, Some(GameOutcome.White), None),
        )
        val p = Pairing(bot1, bot2, matches, None)
        assertTrue(p.earlyWinner(2).contains(GameOutcome.Black))
      },
      test("earlyWinner keeps a reversed draw as a draw") {
        val matches = Vector(
          Match(GameId("g1"), bot1.id, Some(GameOutcome.Draw), None),
          Match(GameId("g2"), bot2.id, Some(GameOutcome.Draw), None),
        )
        val p = Pairing(bot1, bot2, matches, None)
        assertTrue(p.earlyWinner(2).contains(GameOutcome.Draw))
      },
    ),
    suite("recordResult")(
      test("records outcome and computes aggregate") {
        val p = mkPairing(1)
        val updated = p.recordResult(GameId("g0"), GameOutcome.White, "e2e4", 1)
        assertTrue(updated.matches.head.outcome.contains(GameOutcome.White)) &&
        assertTrue(updated.matches.head.moves.contains("e2e4")) &&
        assertTrue(updated.aggregateOutcome.contains(GameOutcome.White))
      },
      test("partial best-of-3 does not set aggregate") {
        val p = mkPairing(3)
        val updated = p.recordResult(GameId("g0"), GameOutcome.White, "e2e4", 3)
        assertTrue(updated.matches.head.outcome.contains(GameOutcome.White)) &&
        assertTrue(updated.aggregateOutcome.isEmpty)
      },
    ),
    suite("Round")(
      test("isComplete when all pairings complete") {
        val p1 = mkPairing(1, Vector(Some(GameOutcome.White)))
          .copy(aggregateOutcome = Some(GameOutcome.White))
        val round = Round(1, Vector(p1))
        assertTrue(round.isComplete(1))
      },
      test("not complete when pairing pending") {
        val p1 = mkPairing(1)
        val round = Round(1, Vector(p1))
        assertTrue(!round.isComplete(1))
      },
    ),
  )
