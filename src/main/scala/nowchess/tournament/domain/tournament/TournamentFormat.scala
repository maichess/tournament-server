package nowchess.tournament.domain.tournament

enum TournamentFormat:
  case Swiss
  case SingleElimination
  case DoubleElimination
  case GroupStage(groupSize: Int)
  case League
  case RandomKnockout
