package tournament.domain.standing

import tournament.domain.model.*
import tournament.domain.round.{Round, Pairing}
import tournament.domain.tournament.Tournament

object ScoringRules:

  final case class BotStats(
    points: Double,
    wins: Int,
    draws: Int,
    losses: Int,
    nbGames: Int,
    opponents: Vector[BotId],
  )

  def computeStandings(tournament: Tournament): Vector[Result] =
    val stats = computeStats(tournament)
    val pointsMap = stats.map((id, s) => id -> s.points)
    val withTieBreak = stats.map: (botId, s) =>
      val tb = buchholzTieBreak(s.opponents, pointsMap)
      (botId, s, tb)
    val sorted = withTieBreak.toVector
      .sortBy((_, s, tb) => (-s.points, -tb, -s.wins))
    sorted.zipWithIndex.map: (entry, idx) =>
      val (botId, s, tb) = entry
      val bot = tournament.participants.find(_.id == botId).getOrElse(BotRef(botId, "unknown"))
      Result(
        rank = idx + 1,
        points = s.points,
        tieBreak = tb,
        bot = bot,
        nbGames = s.nbGames,
        wins = s.wins,
        draws = s.draws,
        losses = s.losses,
      )

  def computeStats(tournament: Tournament): Map[BotId, BotStats] =
    val initial = tournament.participants.map(b => b.id -> BotStats(0, 0, 0, 0, 0, Vector.empty)).toMap
    tournament.rounds.foldLeft(initial): (acc, round) =>
      round.pairings.foldLeft(acc): (stats, pairing) =>
        pairing.aggregateOutcome match
          case None => stats
          case Some(outcome) =>
            val whiteId = pairing.white.id
            val blackId = pairing.black.id
            val (wp, bp) = outcome match
              case GameOutcome.White => (1.0, 0.0)
              case GameOutcome.Black => (0.0, 1.0)
              case GameOutcome.Draw  => (0.5, 0.5)
            val (ww, wd, wl) = outcome match
              case GameOutcome.White => (1, 0, 0)
              case GameOutcome.Black => (0, 0, 1)
              case GameOutcome.Draw  => (0, 1, 0)
            val (bw, bd, bl) = outcome match
              case GameOutcome.White => (0, 0, 1)
              case GameOutcome.Black => (1, 0, 0)
              case GameOutcome.Draw  => (0, 1, 0)

            val ws = stats.getOrElse(whiteId, BotStats(0, 0, 0, 0, 0, Vector.empty))
            val bs = stats.getOrElse(blackId, BotStats(0, 0, 0, 0, 0, Vector.empty))
            stats
              .updated(whiteId, ws.copy(
                points = ws.points + wp, wins = ws.wins + ww, draws = ws.draws + wd,
                losses = ws.losses + wl, nbGames = ws.nbGames + 1, opponents = ws.opponents :+ blackId
              ))
              .updated(blackId, bs.copy(
                points = bs.points + bp, wins = bs.wins + bw, draws = bs.draws + bd,
                losses = bs.losses + bl, nbGames = bs.nbGames + 1, opponents = bs.opponents :+ whiteId
              ))

  def buchholzTieBreak(opponents: Vector[BotId], pointsMap: Map[BotId, Double]): Double =
    opponents.map(id => pointsMap.getOrElse(id, 0.0)).sum
