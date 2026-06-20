package tournament.domain.game

import tournament.domain.model.*
import java.time.Instant

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
  lastMoveAt: Instant = Instant.EPOCH,
  startedAt: Option[Instant] = None,
  endedAt: Option[Instant] = None,
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

  def durationMillis: Option[Long] =
    for
      s <- startedAt
      e <- endedAt
    yield e.toEpochMilli - s.toEpochMilli

final case class GameClock(
  whiteTime: Double,
  blackTime: Double,
  increment: Int = 0,
):
  def timeForTurn(turn: Color): Double = turn match
    case Color.White => whiteTime
    case Color.Black => blackTime

  def withTimeForTurn(turn: Color, time: Double): GameClock = turn match
    case Color.White => copy(whiteTime = time)
    case Color.Black => copy(blackTime = time)

