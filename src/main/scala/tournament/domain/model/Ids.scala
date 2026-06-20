package tournament.domain.model

opaque type TournamentId = String
object TournamentId:
  def apply(value: String): TournamentId = value
  extension (id: TournamentId) def value: String = id

opaque type BotId = String
object BotId:
  def apply(value: String): BotId = value
  extension (id: BotId) def value: String = id

opaque type UserId = String
object UserId:
  def apply(value: String): UserId = value
  extension (id: UserId) def value: String = id

opaque type GameId = String
object GameId:
  def apply(value: String): GameId = value
  extension (id: GameId) def value: String = id
