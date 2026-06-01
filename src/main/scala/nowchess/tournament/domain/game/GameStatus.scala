package nowchess.tournament.domain.game

enum GameStatus:
  case Ongoing, Checkmate, Stalemate, Draw, Resigned, Timeout

  def isTerminal: Boolean = this != Ongoing
