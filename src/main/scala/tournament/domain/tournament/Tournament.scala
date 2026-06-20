package tournament.domain.tournament

import tournament.domain.model.*
import tournament.domain.round.Round
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
  finishedAt: Option[Instant] = None,
  winner: Option[BotRef],
  seed: Long = 0L,
)
