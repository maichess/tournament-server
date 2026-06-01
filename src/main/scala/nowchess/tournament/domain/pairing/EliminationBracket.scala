package nowchess.tournament.domain.pairing

import nowchess.tournament.domain.model.{BotRef, BotId, GameOutcome}
import nowchess.tournament.domain.standing.Result
import nowchess.tournament.domain.round.Round

object EliminationBracket extends PairingAlgorithm:

  override def pair(
    participants: Vector[BotRef],
    standings: Vector[Result],
    completedRounds: Vector[Round],
    roundNumber: Int,
  ): Vector[(BotRef, BotRef)] =
    if participants.size < 2 then Vector.empty
    else if roundNumber == 1 then firstRoundPairings(participants)
    else advancingPairings(completedRounds)

  private[pairing] def firstRoundPairings(participants: Vector[BotRef]): Vector[(BotRef, BotRef)] =
    // Seed 1 vs last, 2 vs second-to-last, etc.
    val n = participants.size
    val pairs = Vector.newBuilder[(BotRef, BotRef)]
    for i <- 0 until n / 2 do
      pairs += ((participants(i), participants(n - 1 - i)))
    pairs.result()

  private[pairing] def advancingPairings(completedRounds: Vector[Round]): Vector[(BotRef, BotRef)] =
    val lastRound = completedRounds.lastOption.getOrElse(return Vector.empty)
    val winners = lastRound.pairings.flatMap: p =>
      p.aggregateOutcome match
        case Some(GameOutcome.White) => Some(p.white)
        case Some(GameOutcome.Black) => Some(p.black)
        case Some(GameOutcome.Draw)  => Some(p.white) // tiebreak: white advances on draw
        case None                    => None

    if winners.size < 2 then Vector.empty
    else
      val pairs = Vector.newBuilder[(BotRef, BotRef)]
      for i <- 0 until winners.size / 2 do
        pairs += ((winners(i * 2), winners(i * 2 + 1)))
      pairs.result()

  def totalRounds(numParticipants: Int): Int =
    if numParticipants <= 1 then 0
    else math.ceil(math.log(numParticipants.toDouble) / math.log(2.0)).toInt
