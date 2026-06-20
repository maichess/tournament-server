package tournament.domain.round

final case class Round(
  number: Int,
  pairings: Vector[Pairing],
):
  def isComplete(matchesPerPairing: Int): Boolean =
    pairings.forall(_.isComplete(matchesPerPairing))
