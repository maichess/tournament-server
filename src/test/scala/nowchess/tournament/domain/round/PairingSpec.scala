package nowchess.tournament.domain.round

import zio.test.*
import nowchess.tournament.domain.model.*

object PairingSpec extends ZIOSpecDefault:

  private val bot1 = BotRef(BotId("b1"), "Bot1")
  private val bot2 = BotRef(BotId("b2"), "Bot2")

  private def mkPairing(numMatches: Int, outcomes: Vector[Option[GameOutcome]] = Vector.empty) =
    val matches = (0 until numMatches).toVector.map: i =>
      Match(GameId(s"g$i"), outcomes.lift(i).flatten, None)
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
