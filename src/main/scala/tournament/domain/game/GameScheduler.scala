package tournament.domain.game

import tournament.domain.model.GameId

/** Pure scheduling decision for the per-round concurrency cap.
  *
  * Given how many games are already running and the queue of pending games (in
  * canonical order), returns the prefix of the queue that should be promoted to
  * `Ongoing`. A cap of `None` means unlimited concurrency (activate everything).
  */
object GameScheduler:

  def toActivate(
    ongoingCount: Int,
    pending: Vector[GameId],
    cap: Option[Int],
  ): Vector[GameId] =
    cap match
      case None      => pending
      case Some(max) => pending.take(math.max(0, max - ongoingCount))
