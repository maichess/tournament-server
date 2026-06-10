package nowchess.tournament.domain.model

/** A bot registered permanently with the server so it can be added to a
  * tournament by id alone. `endpoint` is the bot's external play URL; the
  * tournament server stores it for reference but does not drive game transport.
  */
final case class RegisteredBot(id: BotId, name: String, endpoint: Option[String]):
  def toRef: BotRef = BotRef(id, name)
