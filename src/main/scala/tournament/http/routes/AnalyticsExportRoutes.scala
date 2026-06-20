package tournament.http.routes

import zio.*
import zio.http.*
import zio.json.*
import tournament.domain.model.*
import tournament.domain.game.GameStatus
import tournament.domain.tournament.TournamentStatus
import tournament.domain.standing.ScoringRules
import tournament.domain.error.DomainError
import tournament.service.{TournamentService, BotRegistryService}
import tournament.persistence.GameRepository
import tournament.http.codec.JsonCodecs.{*, given}

object AnalyticsExportRoutes:

  def routes: Routes[TournamentService & GameRepository & BotRegistryService, Nothing] =
    Routes(
      Method.GET / "api" / "tournament" / string("id") / "analytics-export" ->
        handler((id: String, _: Request) => analyticsExport(id)),
    )

  private def analyticsExport(id: String): ZIO[TournamentService & GameRepository & BotRegistryService, Nothing, Response] =
    (for
      tournament <- TournamentService.get(TournamentId(id))
      _ <- ZIO.when(tournament.status != TournamentStatus.Finished)(
        ZIO.fail(DomainError.Conflict("tournament is not finished yet"))
      )
      _ <- ZIO.when(tournament.startedAt.isEmpty)(
        ZIO.fail(DomainError.Conflict("tournament was cancelled before it started and has no analytics data"))
      )
      games    <- GameRepository.findByTournament(TournamentId(id))
      // Only fetch metadata for bots that participated; the full registry may be large.
      participantIds = tournament.participants.map(_.id)
      botEntries <- ZIO.foreach(participantIds)(BotRegistryService.get)
      botMap      = botEntries.flatten.map(b => b.id -> b).toMap
      standings = ScoringRules.computeStandings(tournament)
      exportedAt <- zio.Clock.instant

      formatStr = encodeAsString(tournament.config.format)

      exportGames = games.map: g =>
        val whiteMeta = botMap.get(g.white.id)
        val blackMeta = botMap.get(g.black.id)
        val winnerColor = g.winner.map(c => if c == Color.White then "white" else "black")
        val winnerBotId = g.winner.map:
          case Color.White => g.white.id.value
          case Color.Black => g.black.id.value
        AnalyticsExportGame(
          gameId            = g.id.value,
          tournamentId      = g.tournamentId.value,
          round             = g.round,
          whiteBotId        = g.white.id.value,
          whiteBotName      = g.white.name,
          whiteBotFamily    = whiteMeta.flatMap(_.family),
          whiteStrategyType = whiteMeta.flatMap(_.strategyType),
          whiteEngineType   = whiteMeta.flatMap(_.engineType),
          whiteModelVersion = whiteMeta.flatMap(_.modelVersion),
          blackBotId        = g.black.id.value,
          blackBotName      = g.black.name,
          blackBotFamily    = blackMeta.flatMap(_.family),
          blackStrategyType = blackMeta.flatMap(_.strategyType),
          blackEngineType   = blackMeta.flatMap(_.engineType),
          blackModelVersion = blackMeta.flatMap(_.modelVersion),
          winner            = winnerColor.orElse(if g.status.isTerminal then Some("draw") else None),
          winnerBotId       = winnerBotId,
          terminationReason = encodeAsString(g.status),
          totalPly          = g.moves.size,
          moves             = g.movesUci,
          startedAt         = g.startedAt.map(_.toString),
          endedAt           = g.endedAt.map(_.toString),
          durationMillis    = g.durationMillis,
        )

      exportStandings = standings.map: r =>
        val meta = botMap.get(r.bot.id)
        AnalyticsExportStanding(
          tournamentId = tournament.id.value,
          botId        = r.bot.id.value,
          botName      = r.bot.name,
          botFamily    = meta.flatMap(_.family),
          strategyType = meta.flatMap(_.strategyType),
          engineType   = meta.flatMap(_.engineType),
          modelVersion = meta.flatMap(_.modelVersion),
          rank         = r.rank,
          points       = r.points,
          wins         = r.wins,
          draws        = r.draws,
          losses       = r.losses,
          nbGames      = r.nbGames,
          tieBreak     = r.tieBreak,
        )

      response = AnalyticsExportResponse(
        schemaVersion = "1.0",
        tournamentId  = tournament.id.value,
        format        = formatStr,
        clock         = tournament.config.clock,
        rated         = tournament.config.rated,
        nbRounds      = tournament.config.nbRounds,
        startedAt     = tournament.startedAt.map(_.toString),
        finishedAt    = tournament.finishedAt.map(_.toString),
        exportedAt    = exportedAt.toString,
        standings     = exportStandings,
        games         = exportGames,
      )
    yield Response.json(response.toJson)
    ).catchAll(e => ZIO.succeed(TournamentRoutes.errorToResponse(e)))
