package nowchess.tournament.domain.pairing

import nowchess.tournament.domain.model.BotRef
import nowchess.tournament.domain.standing.Result
import nowchess.tournament.domain.round.Round

trait PairingAlgorithm:
  def pair(
    participants: Vector[BotRef],
    standings: Vector[Result],
    completedRounds: Vector[Round],
    roundNumber: Int,
  ): Vector[(BotRef, BotRef)]
