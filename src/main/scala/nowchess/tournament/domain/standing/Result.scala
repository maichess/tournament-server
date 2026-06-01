package nowchess.tournament.domain.standing

import nowchess.tournament.domain.model.BotRef

final case class Result(
  rank: Int,
  points: Double,
  tieBreak: Double,
  bot: BotRef,
  nbGames: Int,
  wins: Int,
  draws: Int,
  losses: Int,
)
