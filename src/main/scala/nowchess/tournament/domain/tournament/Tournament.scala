package nowchess.tournament.domain.tournament

import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.round.Round
import java.time.Instant

final case class Tournament(
  id: TournamentId,
  config: TournamentConfig,
  status: TournamentStatus,
  participants: Vector[BotRef],
  rounds: Vector[Round],
  currentRound: Int,
  director: UserId,
  createdAt: Instant,
  startedAt: Option[Instant],
  winner: Option[BotRef],
)
