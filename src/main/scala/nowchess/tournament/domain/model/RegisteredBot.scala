package nowchess.tournament.domain.model

/** A bot registered permanently with the server so it can be added to a
  * tournament by id alone. `endpoint` is the bot's external play URL; the
  * tournament server stores it for reference but does not drive game transport.
  *
  * Optional metadata fields (family, strategyType, engineType, modelVersion)
  * are provided by the registering client for analytics enrichment. All are
  * optional and absent means the field will not appear in analytics exports.
  */
final case class RegisteredBot(
  id: BotId,
  name: String,
  endpoint: Option[String],
  family: Option[String] = None,
  strategyType: Option[String] = None,
  engineType: Option[String] = None,
  modelVersion: Option[String] = None,
):
  def toRef: BotRef = BotRef(id, name)
