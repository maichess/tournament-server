package tournament.domain.pairing

import zio.test.*
import tournament.domain.model.*
import tournament.domain.round.*

object EliminationBracketSpec extends ZIOSpecDefault:

  private def bot(n: Int) = BotRef(BotId(s"b$n"), s"Bot$n")

  def spec = suite("EliminationBracket")(
    test("totalRounds for 8 players is 3") {
      assertTrue(EliminationBracket.totalRounds(8) == 3)
    },
    test("totalRounds for 4 players is 2") {
      assertTrue(EliminationBracket.totalRounds(4) == 2)
    },
    test("totalRounds for 5 players is 3") {
      assertTrue(EliminationBracket.totalRounds(5) == 3)
    },
    test("totalRounds for 2 players is 1") {
      assertTrue(EliminationBracket.totalRounds(2) == 1)
    },
    test("totalRounds for 1 player is 0") {
      assertTrue(EliminationBracket.totalRounds(1) == 0)
    },
    test("first round seeds: 1v4, 2v3 for 4 players") {
      val players = (1 to 4).map(bot).toVector
      val pairs = EliminationBracket.firstRoundPairings(players)
      assertTrue(pairs.size == 2) &&
      assertTrue(pairs(0) == (bot(1), bot(4))) &&
      assertTrue(pairs(1) == (bot(2), bot(3)))
    },
    test("advancing pairs winners of previous round") {
      val round1 = Round(1, Vector(
        Pairing(bot(1), bot(4), Vector.empty, Some(GameOutcome.White)),
        Pairing(bot(2), bot(3), Vector.empty, Some(GameOutcome.Black)),
      ))
      val pairs = EliminationBracket.advancingPairings(Vector(round1))
      // Winners: bot1 (white won), bot3 (black won)
      assertTrue(pairs.size == 1) &&
      assertTrue(pairs(0) == (bot(1), bot(3)))
    },
    test("draw in elimination: white advances") {
      val round1 = Round(1, Vector(
        Pairing(bot(1), bot(2), Vector.empty, Some(GameOutcome.Draw)),
        Pairing(bot(3), bot(4), Vector.empty, Some(GameOutcome.White)),
      ))
      val pairs = EliminationBracket.advancingPairings(Vector(round1))
      assertTrue(pairs.size == 1) &&
      assertTrue(pairs(0) == (bot(1), bot(3)))
    },
    test("empty with less than 2") {
      assertTrue(EliminationBracket.pair(Vector(bot(1)), Vector.empty, Vector.empty, 1).isEmpty)
    },
    test("no advancing pairs when round incomplete") {
      val round1 = Round(1, Vector(
        Pairing(bot(1), bot(2), Vector.empty, None),
      ))
      val pairs = EliminationBracket.advancingPairings(Vector(round1))
      assertTrue(pairs.isEmpty)
    },
  )
