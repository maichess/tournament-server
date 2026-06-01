package nowchess.tournament.http.routes

import zio.*
import zio.http.*
import zio.stream.*
import zio.json.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.standing.ScoringRules
import nowchess.tournament.service.*
import nowchess.tournament.persistence.GameRepository
import nowchess.tournament.http.codec.JsonCodecs.{*, given}

object ResultRoutes:

  def routes: Routes[TournamentService & GameRepository, Nothing] =
    Routes(
      Method.GET / "api" / "tournament" / string("id") / "results" ->
        handler((id: String, req: Request) => getResults(id, req)),
      Method.GET / "api" / "tournament" / string("id") / "round" / int("round") ->
        handler((id: String, round: Int, _: Request) => getRound(id, round)),
      Method.GET / "api" / "tournament" / string("id") / "export" / "games" ->
        handler((id: String, req: Request) => exportGames(id, req)),
    )

  private def getResults(id: String, req: Request): ZIO[TournamentService, Nothing, Response] =
    (for
      t <- TournamentService.get(TournamentId(id))
      results = ScoringRules.computeStandings(t)
      nb = req.url.queryParams.getAll("nb").headOption.flatMap(_.toIntOption)
      limited = nb.map(n => results.take(n)).getOrElse(results)
      ndjson = limited.map(_.toJson).mkString("\n") + "\n"
    yield Response(
      status = Status.Ok,
      headers = Headers(Header.ContentType(MediaType("application", "x-ndjson"))),
      body = Body.fromString(ndjson),
    )).catchAll(e => ZIO.succeed(TournamentRoutes.errorToResponse(e)))

  private def getRound(id: String, round: Int): ZIO[TournamentService, Nothing, Response] =
    (for
      t <- TournamentService.get(TournamentId(id))
      r <- ZIO.fromOption(t.rounds.find(_.number == round))
        .mapError(_ => nowchess.tournament.domain.error.DomainError.NotFound(s"round $round not found"))
    yield Response.json(RoundResponse(r.number, r.pairings).toJson)
    ).catchAll(e => ZIO.succeed(TournamentRoutes.errorToResponse(e)))

  private def exportGames(id: String, req: Request): ZIO[TournamentService & GameRepository, Nothing, Response] =
    val acceptNdjson = req.header(Header.Accept).exists(_.mimeTypes.exists(_.mediaType.subType == "x-ndjson"))
    (for
      t <- TournamentService.get(TournamentId(id))
      games <- GameRepository.findByTournament(TournamentId(id))
    yield
      if acceptNdjson then
        val ndjson = games.map: g =>
          GameExportJson(g.id.value, g.round, g.white, g.black,
            g.winner.map(c => if c == Color.White then "white" else "black"),
            g.movesUci).toJson
        .mkString("\n") + "\n"
        Response(
          status = Status.Ok,
          headers = Headers(Header.ContentType(MediaType("application", "x-ndjson"))),
          body = Body.fromString(ndjson),
        )
      else
        val pgn = games.map(gameToPgn).mkString("\n\n") + "\n"
        Response(
          status = Status.Ok,
          headers = Headers(Header.ContentType(MediaType("application", "x-chess-pgn"))),
          body = Body.fromString(pgn),
        )
    ).catchAll(e => ZIO.succeed(TournamentRoutes.errorToResponse(e)))

  private def gameToPgn(g: nowchess.tournament.domain.game.Game): String =
    val result = g.winner match
      case Some(Color.White) => "1-0"
      case Some(Color.Black) => "0-1"
      case None if g.status.isTerminal => "1/2-1/2"
      case _ => "*"
    s"""[White "${g.white.name}"]
       |[Black "${g.black.name}"]
       |[Result "$result"]
       |
       |${g.movesUci} $result""".stripMargin
