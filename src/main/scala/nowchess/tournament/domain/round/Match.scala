package nowchess.tournament.domain.round

import nowchess.tournament.domain.model.*

final case class Match(
  gameId: GameId,
  outcome: Option[GameOutcome],
  moves: Option[String],
)
