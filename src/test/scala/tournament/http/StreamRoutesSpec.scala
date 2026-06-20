package tournament.http

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.http.*
import zio.json.*
import tournament.http.routes.{TournamentRoutes, ParticipationRoutes, StreamRoutes, GameRoutes}
import tournament.http.codec.JsonCodecs.given
import tournament.http.RouteTestHelpers.*
import tournament.domain.model.*
import tournament.service.{TournamentService, StreamService}
import tournament.domain.event.TournamentEvent

object StreamRoutesSpec extends ZIOSpecDefault:

  val allRoutes = TournamentRoutes.routes ++ ParticipationRoutes.routes ++ StreamRoutes.routes

  def spec = suite("StreamRoutes")(
    test("GET /stream returns NDJSON response"):
      for
        createRes <- allRoutes.runZIO(
          Request.post(
            URL(Path.root / "api" / "tournament"),
            Body.fromString(createTournamentBody()),
          ).addHeaders(authHeader("director-token"))
        )
        body <- createRes.body.asString
        id = extractId(body)
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "stream"))
            .addHeaders(authHeader("bot1-token"))
        )
      yield assertTrue(
        response.status == Status.Ok,
        response.header(Header.ContentType).exists(_.mediaType.subType == "x-ndjson"),
      )
    ,
    test("GET /stream without auth fails"):
      for
        createRes <- allRoutes.runZIO(
          Request.post(
            URL(Path.root / "api" / "tournament"),
            Body.fromString(createTournamentBody()),
          ).addHeaders(authHeader("director-token"))
        )
        body <- createRes.body.asString
        id = extractId(body)
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "stream"))
        )
      yield assertTrue(response.status == Status.Unauthorized)
    ,
    test("GET /stream for nonexistent tournament fails"):
      for
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / "fake" / "stream"))
            .addHeaders(authHeader("bot1-token"))
        )
      yield assertTrue(response.status == Status.NotFound)
    ,
  ).provide(allLayers) @@ TestAspect.sequential
