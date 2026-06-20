package tournament.domain.model

final case class Variant(key: String, name: String)

object Variant:
  val standard: Variant = Variant("standard", "Standard")
