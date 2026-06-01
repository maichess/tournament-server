package nowchess.tournament.domain.event

import nowchess.tournament.domain.model.*

enum TournamentEvent:
  case TournamentStarted
  case RoundStarted(round: Int)
  case GameStart(round: Int, gameId: GameId, color: Color)
  case RoundFinished(round: Int)
  case TournamentFinished(winner: BotRef)
