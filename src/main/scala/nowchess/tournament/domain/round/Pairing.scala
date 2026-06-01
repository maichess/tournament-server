package nowchess.tournament.domain.round

import nowchess.tournament.domain.model.*

final case class Pairing(
  white: BotRef,
  black: BotRef,
  matches: Vector[Match],
  aggregateOutcome: Option[GameOutcome],
):
  def isComplete(matchesPerPairing: Int): Boolean =
    aggregateOutcome.isDefined || earlyWinner(matchesPerPairing).isDefined

  def earlyWinner(matchesPerPairing: Int): Option[GameOutcome] =
    val needed = (matchesPerPairing + 1) / 2
    val whiteWins = matches.count(_.outcome.contains(GameOutcome.White))
    val blackWins = matches.count(_.outcome.contains(GameOutcome.Black))
    if whiteWins >= needed then Some(GameOutcome.White)
    else if blackWins >= needed then Some(GameOutcome.Black)
    else if matches.count(_.outcome.isDefined) == matchesPerPairing then
      if whiteWins > blackWins then Some(GameOutcome.White)
      else if blackWins > whiteWins then Some(GameOutcome.Black)
      else Some(GameOutcome.Draw)
    else None

  def recordResult(gameId: GameId, outcome: GameOutcome, moves: String, matchesPerPairing: Int): Pairing =
    val updatedMatches = this.matches.map: m =>
      if m.gameId == gameId then m.copy(outcome = Some(outcome), moves = Some(moves))
      else m
    val updated = copy(matches = updatedMatches)
    updated.copy(aggregateOutcome = updated.earlyWinner(matchesPerPairing))
