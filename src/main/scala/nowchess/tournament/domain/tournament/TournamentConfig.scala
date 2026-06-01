package nowchess.tournament.domain.tournament

import nowchess.tournament.domain.model.*

final case class TournamentConfig(
  name: String,
  nbRounds: Int,
  clock: Clock,
  rated: Boolean,
  format: TournamentFormat,
  startPosition: StartPosition,
  matchesPerPairing: Int,
)
