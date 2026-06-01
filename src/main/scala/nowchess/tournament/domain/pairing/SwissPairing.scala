package nowchess.tournament.domain.pairing

import nowchess.tournament.domain.model.{BotRef, BotId}
import nowchess.tournament.domain.standing.Result
import nowchess.tournament.domain.round.Round

object SwissPairing extends PairingAlgorithm:

  override def pair(
    participants: Vector[BotRef],
    standings: Vector[Result],
    completedRounds: Vector[Round],
    roundNumber: Int,
  ): Vector[(BotRef, BotRef)] =
    if participants.size < 2 then Vector.empty
    else
      val previousOpponents = buildOpponentMap(completedRounds)
      val sorted = sortByStandings(participants, standings)
      greedyPair(sorted, previousOpponents)

  private def sortByStandings(participants: Vector[BotRef], standings: Vector[Result]): Vector[BotRef] =
    val pointsMap = standings.map(r => r.bot.id -> r.points).toMap
    participants.sortBy(b => -pointsMap.getOrElse(b.id, 0.0))

  private def buildOpponentMap(rounds: Vector[Round]): Map[BotId, Set[BotId]] =
    rounds.foldLeft(Map.empty[BotId, Set[BotId]]): (acc, round) =>
      round.pairings.foldLeft(acc): (m, p) =>
        val wSet = m.getOrElse(p.white.id, Set.empty) + p.black.id
        val bSet = m.getOrElse(p.black.id, Set.empty) + p.white.id
        m.updated(p.white.id, wSet).updated(p.black.id, bSet)

  private[pairing] def greedyPair(
    sorted: Vector[BotRef],
    previousOpponents: Map[BotId, Set[BotId]],
  ): Vector[(BotRef, BotRef)] =
    val paired = scala.collection.mutable.Set.empty[BotId]
    val result = Vector.newBuilder[(BotRef, BotRef)]

    for i <- sorted.indices do
      if !paired.contains(sorted(i).id) then
        val bot = sorted(i)
        val opponent = sorted.indices.drop(i + 1)
          .filterNot(j => paired.contains(sorted(j).id))
          .find: j =>
            val opp = sorted(j)
            !previousOpponents.getOrElse(bot.id, Set.empty).contains(opp.id)
          .orElse:
            sorted.indices.drop(i + 1)
              .find(j => !paired.contains(sorted(j).id))

        opponent.foreach: j =>
          val opp = sorted(j)
          paired += bot.id
          paired += opp.id
          result += ((bot, opp))

    result.result()
