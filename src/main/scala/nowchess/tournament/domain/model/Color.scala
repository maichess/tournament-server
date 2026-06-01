package nowchess.tournament.domain.model

enum Color:
  case White, Black

  def opposite: Color = this match
    case White => Black
    case Black => White
