package nowchess.tournament.domain.event

import nowchess.tournament.domain.model.Color
import nowchess.tournament.domain.game.{GameStatus, GameClock}

enum GameEvent:
  case GameState(
    fen: String,
    moves: String,
    turn: Color,
    clock: GameClock,
    status: GameStatus,
    winner: Option[Color],
  )
  case MovePlayed(
    uci: String,
    fen: String,
    turn: Color,
    clock: GameClock,
  )
  case GameEnd(
    winner: Option[Color],
    status: GameStatus,
  )
