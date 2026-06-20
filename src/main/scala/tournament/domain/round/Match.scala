package tournament.domain.round

import tournament.domain.model.*

/** One game within a pairing. `whiteId` is the bot that actually played White in
  * this game, which can differ from the pairing's nominal White when a
  * multi-position opening book reverses colours. `outcome` is recorded relative
  * to this game's own colours; the pairing normalises it via `whiteId`.
  */
final case class Match(
  gameId: GameId,
  whiteId: BotId,
  outcome: Option[GameOutcome],
  moves: Option[String],
)
