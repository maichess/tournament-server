package nowchess.tournament.http

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.http.*
import zio.json.*
import nowchess.tournament.http.routes.{TournamentRoutes, ParticipationRoutes}
import nowchess.tournament.http.codec.JsonCodecs.given
import nowchess.tournament.http.RouteTestHelpers.*

object ParticipationRoutesSpec extends ZIOSpecDefault:

  val tRoutes = TournamentRoutes.routes
  val pRoutes = ParticipationRoutes.routes
  val allRoutes = tRoutes ++ pRoutes

  private def createAndGetId =
    for
      createRes <- allRoutes.runZIO(
        Request.post(
          URL(Path.root / "api" / "tournament"),
          Body.fromString(createTournamentBody()),
        ).addHeaders(authHeader("director-token"))
      )
      body <- createRes.body.asString
    yield extractId(body)

  def spec = suite("ParticipationRoutes")(
    test("POST /join lets bot join"):
      for
        id <- createAndGetId
        joinRes <- allRoutes.runZIO(
          Request.post(URL(Path.root / "api" / "tournament" / id / "join"), Body.empty)
            .addHeaders(authHeader("bot1-token"))
        )
        joinBody <- joinRes.body.asString
      yield assertTrue(
        joinRes.status == Status.Ok,
        joinBody.contains("\"ok\":true"),
      )
    ,
    test("POST /join requires bot account"):
      for
        id <- createAndGetId
        joinRes <- allRoutes.runZIO(
          Request.post(URL(Path.root / "api" / "tournament" / id / "join"), Body.empty)
            .addHeaders(authHeader("user-token"))
        )
      yield assertTrue(joinRes.status == Status.Forbidden)
    ,
    test("POST /withdraw lets bot withdraw"):
      for
        id <- createAndGetId
        _ <- allRoutes.runZIO(
          Request.post(URL(Path.root / "api" / "tournament" / id / "join"), Body.empty)
            .addHeaders(authHeader("bot1-token"))
        )
        wRes <- allRoutes.runZIO(
          Request.post(URL(Path.root / "api" / "tournament" / id / "withdraw"), Body.empty)
            .addHeaders(authHeader("bot1-token"))
        )
        wBody <- wRes.body.asString
      yield assertTrue(
        wRes.status == Status.Ok,
        wBody.contains("\"ok\":true"),
      )
    ,
    test("POST /join without auth fails"):
      for
        id <- createAndGetId
        joinRes <- allRoutes.runZIO(
          Request.post(URL(Path.root / "api" / "tournament" / id / "join"), Body.empty)
        )
      yield assertTrue(joinRes.status == Status.Unauthorized)
    ,
    test("POST /join to nonexistent tournament"):
      for
        joinRes <- allRoutes.runZIO(
          Request.post(URL(Path.root / "api" / "tournament" / "fake" / "join"), Body.empty)
            .addHeaders(authHeader("bot1-token"))
        )
      yield assertTrue(joinRes.status == Status.NotFound)
    ,
    test("POST /join duplicate bot"):
      for
        id <- createAndGetId
        _ <- allRoutes.runZIO(
          Request.post(URL(Path.root / "api" / "tournament" / id / "join"), Body.empty)
            .addHeaders(authHeader("bot1-token"))
        )
        dupRes <- allRoutes.runZIO(
          Request.post(URL(Path.root / "api" / "tournament" / id / "join"), Body.empty)
            .addHeaders(authHeader("bot1-token"))
        )
      yield assertTrue(dupRes.status == Status.Conflict)
    ,
  ).provide(allLayers) @@ TestAspect.sequential
