package tournament.domain.pairing

import tournament.domain.model.BotRef
import tournament.domain.standing.Result
import tournament.domain.round.Round

trait PairingAlgorithm:
  def pair(
    participants: Vector[BotRef],
    standings: Vector[Result],
    completedRounds: Vector[Round],
    roundNumber: Int,
  ): Vector[(BotRef, BotRef)]
