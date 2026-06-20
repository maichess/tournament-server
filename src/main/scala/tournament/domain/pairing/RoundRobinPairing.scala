package tournament.domain.pairing

import tournament.domain.model.BotRef
import tournament.domain.standing.Result
import tournament.domain.round.Round

object RoundRobinPairing extends PairingAlgorithm:

  override def pair(
    participants: Vector[BotRef],
    standings: Vector[Result],
    completedRounds: Vector[Round],
    roundNumber: Int,
  ): Vector[(BotRef, BotRef)] =
    if participants.size < 2 then Vector.empty
    else pairingsForRound(participants, roundNumber)

  private[pairing] def pairingsForRound(
    participants: Vector[BotRef],
    roundNumber: Int,
  ): Vector[(BotRef, BotRef)] =
    // Circle method: fix first player, rotate the rest
    val players = if participants.size % 2 == 0 then participants
                  else participants :+ null.asInstanceOf[BotRef] // bye placeholder
    val n = players.size
    val fixed = players.head
    val rotating = players.tail.toArray

    // Rotate (roundNumber - 1) times
    val shift = (roundNumber - 1) % (n - 1)
    val rotated = rotate(rotating, shift)

    val pairs = Vector.newBuilder[(BotRef, BotRef)]
    // First pair: fixed vs last of rotated
    if fixed != null && rotated.last != null then
      pairs += ((fixed, rotated.last))
    // Remaining pairs from outside in
    for i <- 0 until (n / 2 - 1) do
      val a = rotated(i)
      val b = rotated(n - 2 - i)
      if a != null && b != null then
        pairs += ((a, b))
    pairs.result()

  private def rotate[A: scala.reflect.ClassTag](arr: Array[A], shift: Int): Array[A] =
    val s = shift % arr.length
    val (left, right) = arr.splitAt(s)
    right ++ left

  def totalRounds(numParticipants: Int): Int =
    if numParticipants <= 1 then 0
    else if numParticipants % 2 == 0 then numParticipants - 1
    else numParticipants
