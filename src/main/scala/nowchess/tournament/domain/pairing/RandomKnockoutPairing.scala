package nowchess.tournament.domain.pairing

import nowchess.tournament.domain.model.BotRef
import nowchess.tournament.domain.standing.Result
import nowchess.tournament.domain.round.Round

/** Knockout bracket whose first-round pairings are drawn at random (seeded for
  * determinism). The winner of each match advances, so the bracket collapses to
  * a single champion — a random-draw knockout tree. Later rounds reuse the
  * elimination "winner advances" logic, including its white-advances-on-draw
  * tiebreak.
  */
final class RandomKnockoutPairing(seed: Long) extends PairingAlgorithm:

  override def pair(
    participants: Vector[BotRef],
    standings: Vector[Result],
    completedRounds: Vector[Round],
    roundNumber: Int,
  ): Vector[(BotRef, BotRef)] =
    if participants.size < 2 then Vector.empty
    else if roundNumber == 1 then firstRoundPairings(participants)
    else EliminationBracket.advancingPairings(completedRounds)

  private[pairing] def firstRoundPairings(participants: Vector[BotRef]): Vector[(BotRef, BotRef)] =
    val shuffled = new scala.util.Random(seed).shuffle(participants)
    val pairs = Vector.newBuilder[(BotRef, BotRef)]
    for i <- 0 until shuffled.size / 2 do
      pairs += ((shuffled(i * 2), shuffled(i * 2 + 1)))
    pairs.result()

object RandomKnockoutPairing:
  def totalRounds(numParticipants: Int): Int =
    EliminationBracket.totalRounds(numParticipants)
