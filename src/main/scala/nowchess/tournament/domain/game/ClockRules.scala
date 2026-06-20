package nowchess.tournament.domain.game

import nowchess.tournament.domain.model.Color
import java.time.{Duration, Instant}

/** Pure chess-clock arithmetic shared by the move path and the timeout daemon so
  * the two never disagree. Times are in seconds; `lastMoveAt` marks when the
  * player to move started their turn (set on activation and after every move).
  */
object ClockRules:

  /** Wall-clock seconds the player to move has spent so far, never negative. */
  def elapsedSeconds(lastMoveAt: Instant, now: Instant): Double =
    math.max(0.0, Duration.between(lastMoveAt, now).toMillis / 1000.0)

  /** Whether the player to move has run out of time as of `now`. Checked against
    * the remaining time *before* any increment is credited, as flag-fall rules
    * require. */
  def hasFlagged(clock: GameClock, turn: Color, lastMoveAt: Instant, now: Instant): Boolean =
    clock.timeForTurn(turn) - elapsedSeconds(lastMoveAt, now) <= 0.0

  /** Settles the clock when the player to move completes a move at `now`:
    * deduct the time they spent, and credit the Fischer increment *only* if they
    * did not flag. Returns the new clock and whether the player flagged. */
  def applyMove(clock: GameClock, turn: Color, lastMoveAt: Instant, now: Instant): (GameClock, Boolean) =
    val remaining = clock.timeForTurn(turn) - elapsedSeconds(lastMoveAt, now)
    if remaining <= 0.0 then (clock.withTimeForTurn(turn, 0.0), true)
    else (clock.withTimeForTurn(turn, remaining + clock.increment.toDouble), false)
