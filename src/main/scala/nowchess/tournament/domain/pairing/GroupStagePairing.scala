package nowchess.tournament.domain.pairing

import nowchess.tournament.domain.model.BotRef
import nowchess.tournament.domain.standing.Result
import nowchess.tournament.domain.round.Round

object GroupStagePairing extends PairingAlgorithm:

  override def pair(
    participants: Vector[BotRef],
    standings: Vector[Result],
    completedRounds: Vector[Round],
    roundNumber: Int,
  ): Vector[(BotRef, BotRef)] =
    pair(participants, standings, completedRounds, roundNumber, groupSize = 4)

  def pair(
    participants: Vector[BotRef],
    standings: Vector[Result],
    completedRounds: Vector[Round],
    roundNumber: Int,
    groupSize: Int,
  ): Vector[(BotRef, BotRef)] =
    if participants.size < 2 then Vector.empty
    else
      val groups = assignGroups(participants, groupSize)
      val groupRounds = groupPhaseRounds(groupSize)
      if roundNumber <= groupRounds then
        groupPhasePairings(groups, roundNumber)
      else
        val knockoutRound = roundNumber - groupRounds
        knockoutPairings(groups, standings, completedRounds, knockoutRound)

  private[pairing] def assignGroups(participants: Vector[BotRef], groupSize: Int): Vector[Vector[BotRef]] =
    participants.grouped(groupSize).toVector

  private[pairing] def groupPhaseRounds(groupSize: Int): Int =
    RoundRobinPairing.totalRounds(groupSize)

  private[pairing] def groupPhasePairings(
    groups: Vector[Vector[BotRef]],
    roundNumber: Int,
  ): Vector[(BotRef, BotRef)] =
    groups.flatMap(group => RoundRobinPairing.pairingsForRound(group, roundNumber))

  private[pairing] def knockoutPairings(
    groups: Vector[Vector[BotRef]],
    standings: Vector[Result],
    completedRounds: Vector[Round],
    knockoutRound: Int,
  ): Vector[(BotRef, BotRef)] =
    if knockoutRound == 1 then
      val qualifiers = selectGroupWinners(groups, standings)
      EliminationBracket.firstRoundPairings(qualifiers)
    else
      EliminationBracket.advancingPairings(completedRounds)

  private[pairing] def selectGroupWinners(
    groups: Vector[Vector[BotRef]],
    standings: Vector[Result],
  ): Vector[BotRef] =
    val standingsMap = standings.map(r => r.bot.id -> r).toMap
    groups.flatMap: group =>
      group
        .sortBy(b => standingsMap.get(b.id).map(r => (-r.points, -r.tieBreak)).getOrElse((0.0, 0.0)))
        .take(topNFromGroup(group.size))

  private def topNFromGroup(groupSize: Int): Int =
    math.max(1, groupSize / 2)
