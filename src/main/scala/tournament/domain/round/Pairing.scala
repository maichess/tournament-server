package tournament.domain.round

import tournament.domain.model.*

final case class Pairing(
  white: BotRef,
  black: BotRef,
  matches: Vector[Match],
  aggregateOutcome: Option[GameOutcome],
):
  def isComplete(matchesPerPairing: Int): Boolean =
    aggregateOutcome.isDefined || earlyWinner(matchesPerPairing).isDefined

  /** A game's outcome from the pairing's nominal White perspective, flipping it
    * when colours were reversed for that game (multi-position opening book). */
  private def outcomeForNominalWhite(m: Match): Option[GameOutcome] =
    m.outcome.map: o =>
      if m.whiteId == white.id then o
      else
        o match
          case GameOutcome.White => GameOutcome.Black
          case GameOutcome.Black => GameOutcome.White
          case GameOutcome.Draw  => GameOutcome.Draw

  def earlyWinner(matchesPerPairing: Int): Option[GameOutcome] =
    val needed = matchesPerPairing / 2 + 1
    val whiteWins = matches.count(outcomeForNominalWhite(_).contains(GameOutcome.White))
    val blackWins = matches.count(outcomeForNominalWhite(_).contains(GameOutcome.Black))
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
