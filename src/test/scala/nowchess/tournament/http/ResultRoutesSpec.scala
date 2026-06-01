package nowchess.tournament.http

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.http.*
import zio.json.*
import nowchess.tournament.http.routes.{TournamentRoutes, ParticipationRoutes, ResultRoutes}
import nowchess.tournament.http.codec.JsonCodecs.given
import nowchess.tournament.http.RouteTestHelpers.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.service.TournamentService

object ResultRoutesSpec extends ZIOSpecDefault:

  val allRoutes = TournamentRoutes.routes ++ ParticipationRoutes.routes ++ ResultRoutes.routes

  private def createStartedTournament =
    for
      createRes <- allRoutes.runZIO(
        Request.post(
          URL(Path.root / "api" / "tournament"),
          Body.fromString(createTournamentBody()),
        ).addHeaders(authHeader("director-token"))
      )
      body <- createRes.body.asString
      id = extractId(body)
      _ <- TournamentService.join(TournamentId(id), testBot1)
      _ <- TournamentService.join(TournamentId(id), testBot2)
      _ <- allRoutes.runZIO(
        Request.post(URL(Path.root / "api" / "tournament" / id / "start"), Body.empty)
          .addHeaders(authHeader("director-token"))
      )
    yield id

  def spec = suite("ResultRoutes")(
    test("GET /results returns standings as NDJSON"):
      for
        id <- createStartedTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "results"))
        )
        body <- response.body.asString
      yield assertTrue(
        response.status == Status.Ok,
        body.contains("\"rank\""),
      )
    ,
    test("GET /results with nb limits results"):
      for
        id <- createStartedTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "results").setQueryParams(QueryParams("nb" -> "1")))
        )
        body <- response.body.asString
        lines = body.trim.split("\n")
      yield assertTrue(lines.length == 1)
    ,
    test("GET /round/{round} returns pairings"):
      for
        id <- createStartedTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "round" / "1"))
        )
        body <- response.body.asString
      yield assertTrue(
        response.status == Status.Ok,
        body.contains("\"pairings\""),
      )
    ,
    test("GET /round/{round} not found"):
      for
        id <- createStartedTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "round" / "99"))
        )
      yield assertTrue(response.status == Status.NotFound)
    ,
    test("GET /results for nonexistent tournament"):
      for
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / "fake" / "results"))
        )
      yield assertTrue(response.status == Status.NotFound)
    ,
    test("GET /export/games returns PGN by default"):
      for
        id <- createStartedTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "export" / "games"))
        )
        body <- response.body.asString
      yield assertTrue(
        response.status == Status.Ok,
        response.header(Header.ContentType).exists(_.mediaType.subType == "x-chess-pgn"),
      )
    ,
    test("GET /export/games with Accept: ndjson returns NDJSON"):
      for
        id <- createStartedTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "export" / "games"))
            .addHeaders(Headers(Header.Accept(MediaType("application", "x-ndjson"))))
        )
        body <- response.body.asString
      yield assertTrue(
        response.status == Status.Ok,
        response.header(Header.ContentType).exists(_.mediaType.subType == "x-ndjson"),
      )
    ,
  ).provide(allLayers) @@ TestAspect.sequential
