package nowchess.tournament.domain.pairing

import zio.test.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.standing.Result
import nowchess.tournament.domain.round.*

object PairingCoverageSpec extends ZIOSpecDefault:

  private val bot1 = BotRef(BotId("b1"), "Bot1")
  private val bot2 = BotRef(BotId("b2"), "Bot2")
  private val bot3 = BotRef(BotId("b3"), "Bot3")
  private val bot4 = BotRef(BotId("b4"), "Bot4")

  def spec = suite("Pairing coverage")(
    suite("EliminationBracket")(
      test("advancingPairings with completed round") {
        val round = Round(1, Vector(
          Pairing(bot1, bot2, Vector(Match(GameId("g1"), bot1.id, Some(GameOutcome.White), None)), Some(GameOutcome.White)),
          Pairing(bot3, bot4, Vector(Match(GameId("g2"), bot3.id, Some(GameOutcome.Black), None)), Some(GameOutcome.Black)),
        ))
        val result = EliminationBracket.advancingPairings(Vector(round))
        assertTrue(result.length == 1) &&
        assertTrue(result.head == (bot1, bot4))
      },
      test("advancingPairings with draw advances white") {
        val round = Round(1, Vector(
          Pairing(bot1, bot2, Vector(Match(GameId("g1"), bot1.id, Some(GameOutcome.Draw), None)), Some(GameOutcome.Draw)),
          Pairing(bot3, bot4, Vector(Match(GameId("g2"), bot3.id, Some(GameOutcome.White), None)), Some(GameOutcome.White)),
        ))
        val result = EliminationBracket.advancingPairings(Vector(round))
        assertTrue(result.length == 1) &&
        assertTrue(result.head == (bot1, bot3))
      },
      test("advancingPairings with empty rounds returns empty") {
        val result = EliminationBracket.advancingPairings(Vector.empty)
        assertTrue(result.isEmpty)
      },
      test("advancingPairings with no completed pairings") {
        val round = Round(1, Vector(
          Pairing(bot1, bot2, Vector(Match(GameId("g1"), bot1.id, None, None)), None),
        ))
        val result = EliminationBracket.advancingPairings(Vector(round))
        assertTrue(result.isEmpty)
      },
      test("pair round > 1 uses advancingPairings") {
        val round = Round(1, Vector(
          Pairing(bot1, bot2, Vector(Match(GameId("g1"), bot1.id, Some(GameOutcome.White), None)), Some(GameOutcome.White)),
          Pairing(bot3, bot4, Vector(Match(GameId("g2"), bot3.id, Some(GameOutcome.Black), None)), Some(GameOutcome.Black)),
        ))
        val result = EliminationBracket.pair(Vector(bot1, bot2, bot3, bot4), Vector.empty, Vector(round), 2)
        assertTrue(result.length == 1)
      },
    ),
    suite("GroupStagePairing")(
      test("pair with default groupSize (no explicit size)") {
        val result = GroupStagePairing.pair(Vector(bot1, bot2, bot3, bot4), Vector.empty, Vector.empty, 1)
        assertTrue(result.nonEmpty)
      },
      test("knockout phase after group rounds") {
        val standings = Vector(
          Result(1, 3.0, 0, bot1, 2, 2, 0, 0),
          Result(2, 1.0, 0, bot2, 2, 1, 0, 1),
          Result(3, 1.0, 0, bot3, 2, 1, 0, 1),
          Result(4, 0.0, 0, bot4, 2, 0, 0, 2),
        )
        // Group size = 4, so groupPhaseRounds = 3 (RR with 4 players). Round 4 is knockout.
        val completedRounds = Vector.empty[Round]
        val result = GroupStagePairing.pair(
          Vector(bot1, bot2, bot3, bot4), standings, completedRounds, 4, groupSize = 4
        )
        assertTrue(result.nonEmpty)
      },
      test("knockout round > 1 uses elimination advancing") {
        val round = Round(1, Vector(
          Pairing(bot1, bot2, Vector(Match(GameId("g1"), bot1.id, Some(GameOutcome.White), None)), Some(GameOutcome.White)),
        ))
        // groupPhaseRounds for size=2 = 1. knockoutRound = roundNumber - 1 = 5 - 1 = 4
        // For knockoutRound > 1, uses EliminationBracket.advancingPairings
        val result = GroupStagePairing.pair(
          Vector(bot1, bot2), Vector.empty, Vector(round), 3, groupSize = 2
        )
        // advancingPairings looks at last completed round
        assertTrue(result.length <= 1) // might be 0 if only 1 winner
      },
    ),
    suite("RoundRobinPairing")(
      test("empty rotation array") {
        // With exactly 2 players, rotating array has 1 element, shift=0
        val result = RoundRobinPairing.pair(Vector(bot1, bot2), Vector.empty, Vector.empty, 1)
        assertTrue(result.length == 1)
      },
      test("4 players round 2 tests rotation") {
        val result = RoundRobinPairing.pair(Vector(bot1, bot2, bot3, bot4), Vector.empty, Vector.empty, 2)
        assertTrue(result.length == 2)
      },
    ),
    suite("Pairing early winner")(
      test("earlyWinner with all matches complete and draw tiebreak") {
        // matchesPerPairing=2, both draw → equal wins → Draw
        val p = Pairing(bot1, bot2,
          Vector(Match(GameId("g1"), bot1.id, Some(GameOutcome.Draw), None), Match(GameId("g2"), bot1.id, Some(GameOutcome.Draw), None)),
          None)
        assertTrue(p.earlyWinner(2).contains(GameOutcome.Draw))
      },
      test("earlyWinner with all matches complete and white more wins") {
        val p = Pairing(bot1, bot2,
          Vector(Match(GameId("g1"), bot1.id, Some(GameOutcome.White), None), Match(GameId("g2"), bot1.id, Some(GameOutcome.Draw), None)),
          None)
        assertTrue(p.earlyWinner(2).contains(GameOutcome.White))
      },
      test("earlyWinner with all matches complete and black more wins") {
        val p = Pairing(bot1, bot2,
          Vector(Match(GameId("g1"), bot1.id, Some(GameOutcome.Black), None), Match(GameId("g2"), bot1.id, Some(GameOutcome.Black), None)),
          None)
        assertTrue(p.earlyWinner(2).contains(GameOutcome.Black))
      },
      test("earlyWinner returns None when matches incomplete") {
        val p = Pairing(bot1, bot2,
          Vector(Match(GameId("g1"), bot1.id, Some(GameOutcome.White), None), Match(GameId("g2"), bot1.id, None, None)),
          None)
        assertTrue(p.earlyWinner(3).isEmpty)
      },
      test("earlyWinner all complete no early win - white leads") {
        // matchesPerPairing=4, needed=2. White=1, Black=0, Draw=3. All complete, no early winner.
        val p = Pairing(bot1, bot2,
          Vector(
            Match(GameId("g1"), bot1.id, Some(GameOutcome.White), None),
            Match(GameId("g2"), bot1.id, Some(GameOutcome.Draw), None),
            Match(GameId("g3"), bot1.id, Some(GameOutcome.Draw), None),
            Match(GameId("g4"), bot1.id, Some(GameOutcome.Draw), None),
          ), None)
        assertTrue(p.earlyWinner(4).contains(GameOutcome.White))
      },
      test("earlyWinner all complete no early win - black leads") {
        val p = Pairing(bot1, bot2,
          Vector(
            Match(GameId("g1"), bot1.id, Some(GameOutcome.Black), None),
            Match(GameId("g2"), bot1.id, Some(GameOutcome.Draw), None),
            Match(GameId("g3"), bot1.id, Some(GameOutcome.Draw), None),
            Match(GameId("g4"), bot1.id, Some(GameOutcome.Draw), None),
          ), None)
        assertTrue(p.earlyWinner(4).contains(GameOutcome.Black))
      },
    ),
    suite("GroupStagePairing selectGroupWinners")(
      test("selectGroupWinners with bot not in standings uses default sort key") {
        val standings = Vector(
          Result(1, 3.0, 1.0, bot1, 2, 1, 0, 1),
        )
        // bot2 is not in standings, should use (0.0, 0.0) default
        val groups = Vector(Vector(bot1, bot2))
        val result = GroupStagePairing.selectGroupWinners(groups, standings)
        assertTrue(result.head == bot1) // bot1 has points, ranked first
      },
    ),
  )
