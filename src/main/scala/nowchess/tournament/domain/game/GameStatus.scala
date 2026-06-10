package nowchess.tournament.domain.game

enum GameStatus:
  case Pending, Ongoing, Checkmate, Stalemate, Draw, Resigned, Timeout

  def isTerminal: Boolean = this != Ongoing && this != Pending
