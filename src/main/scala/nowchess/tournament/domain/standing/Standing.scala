package nowchess.tournament.domain.standing

final case class Standing(
  page: Int,
  players: Vector[Result],
)
