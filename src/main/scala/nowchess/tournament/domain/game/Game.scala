package nowchess.tournament.domain.game

import nowchess.tournament.domain.model.*

final case class Game(
  id: GameId,
  tournamentId: TournamentId,
  round: Int,
  white: BotRef,
  black: BotRef,
  moves: Vector[String],
  status: GameStatus,
  turn: Color,
  winner: Option[Color],
  clock: GameClock,
  startPosition: StartPosition,
  fen: String,
):
  def movesUci: String = moves.mkString(" ")

  def currentPlayer: BotRef = turn match
    case Color.White => white
    case Color.Black => black

  def toOutcome: Option[GameOutcome] = winner match
    case Some(Color.White) => Some(GameOutcome.White)
    case Some(Color.Black) => Some(GameOutcome.Black)
    case None if status.isTerminal => Some(GameOutcome.Draw)
    case None => None

final case class GameClock(
  whiteTime: Double,
  blackTime: Double,
)
