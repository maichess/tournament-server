package tournament.domain.tournament

import tournament.domain.model.*

final case class TournamentConfig(
  name: String,
  nbRounds: Int,
  clock: Clock,
  rated: Boolean,
  format: TournamentFormat,
  startPosition: StartPosition,
  matchesPerPairing: Int,
  maxConcurrentGames: Option[Int] = None,
  startPositions: Vector[StartPosition] = Vector.empty,
):
  /** The ordered (white, black, startPosition) game specs for one pairing.
    * With a multi-position opening book, every position is played twice with
    * the colours reversed; otherwise the single start position is played
    * `matchesPerPairing` times. `matchesPerPairing` is kept consistent with the
    * number of specs (set to `2 * startPositions.size` for a book) so the
    * pairing-completion and early-winner logic counts the right total.
    */
  def pairingGameSpecs(first: BotRef, second: BotRef): Vector[(BotRef, BotRef, StartPosition)] =
    if startPositions.nonEmpty then
      startPositions.flatMap(pos => Vector((first, second, pos), (second, first, pos)))
    else
      Vector.fill(matchesPerPairing)((first, second, startPosition))
