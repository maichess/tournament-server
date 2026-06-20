package tournament.domain.model

enum StartPosition:
  case Standard
  case FromFen(fen: String)

  def toFen: String = this match
    case Standard       => StartPosition.standardFen
    case FromFen(fen)   => fen

object StartPosition:
  val standardFen: String = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

  def fromString(s: String): StartPosition =
    if s == "standard" || s.isEmpty then Standard
    else FromFen(s)
