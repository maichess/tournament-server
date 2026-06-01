package nowchess.tournament.http

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.http.*
import zio.json.*
import nowchess.tournament.http.routes.TournamentRoutes
import nowchess.tournament.http.codec.JsonCodecs.given
import nowchess.tournament.http.RouteTestHelpers.*

object TournamentRoutesSpec extends ZIOSpecDefault:

  val routes = TournamentRoutes.routes

  def spec = suite("TournamentRoutes")(
    test("POST /api/tournament creates tournament"):
      for
        response <- routes.runZIO(
          Request.post(
            URL(Path.root / "api" / "tournament"),
            Body.fromString(createTournamentBody()),
          ).addHeaders(authHeader("director-token"))
        )
        body <- response.body.asString
      yield assertTrue(
        response.status == Status.Created,
        body.contains("\"fullName\":\"Test\""),
      )
    ,
    test("POST /api/tournament without auth fails"):
      for
        response <- routes.runZIO(
          Request.post(
            URL(Path.root / "api" / "tournament"),
            Body.fromString(createTournamentBody()),
          )
        )
      yield assertTrue(response.status == Status.Unauthorized)
    ,
    test("GET /api/tournament lists tournaments"):
      for
        _ <- routes.runZIO(
          Request.post(
            URL(Path.root / "api" / "tournament"),
            Body.fromString(createTournamentBody(name = "Listed")),
          ).addHeaders(authHeader("director-token"))
        )
        response <- routes.runZIO(Request.get(URL(Path.root / "api" / "tournament")))
        body <- response.body.asString
      yield assertTrue(
        response.status == Status.Ok,
        body.contains("\"created\""),
      )
    ,
    test("GET /api/tournament/{id} returns tournament"):
      for
        createRes <- routes.runZIO(
          Request.post(
            URL(Path.root / "api" / "tournament"),
            Body.fromString(createTournamentBody(name = "GetMe")),
          ).addHeaders(authHeader("director-token"))
        )
        body <- createRes.body.asString
        id = extractId(body)
        response <- routes.runZIO(Request.get(URL(Path.root / "api" / "tournament" / id)))
        getBody <- response.body.asString
      yield assertTrue(
        response.status == Status.Ok,
        getBody.contains("GetMe"),
      )
    ,
    test("GET /api/tournament/{id} not found"):
      for
        response <- routes.runZIO(Request.get(URL(Path.root / "api" / "tournament" / "nonexistent")))
      yield assertTrue(response.status == Status.NotFound)
    ,
    test("DELETE /api/tournament/{id} terminates tournament"):
      for
        createRes <- routes.runZIO(
          Request.post(
            URL(Path.root / "api" / "tournament"),
            Body.fromString(createTournamentBody(name = "DeleteMe")),
          ).addHeaders(authHeader("director-token"))
        )
        body <- createRes.body.asString
        id = extractId(body)
        delRes <- routes.runZIO(
          Request.delete(URL(Path.root / "api" / "tournament" / id))
            .addHeaders(authHeader("director-token"))
        )
        getRes <- routes.runZIO(Request.get(URL(Path.root / "api" / "tournament" / id)))
        getBody <- getRes.body.asString
      yield assertTrue(
        delRes.status == Status.NoContent,
        getBody.contains("\"status\":\"finished\""),
      )
    ,
    test("POST /api/tournament with invalid form returns bad request"):
      for
        response <- routes.runZIO(
          Request.post(
            URL(Path.root / "api" / "tournament"),
            Body.fromString("name=Test"),
          ).addHeaders(authHeader("director-token"))
        )
      yield assertTrue(response.status == Status.BadRequest)
    ,
    test("POST /api/tournament/{id}/start starts tournament"):
      for
        createRes <- routes.runZIO(
          Request.post(
            URL(Path.root / "api" / "tournament"),
            Body.fromString(createTournamentBody(name = "StartMe")),
          ).addHeaders(authHeader("director-token"))
        )
        body <- createRes.body.asString
        id = extractId(body)
        _ <- nowchess.tournament.service.TournamentService.join(
          nowchess.tournament.domain.model.TournamentId(id), testBot1)
        _ <- nowchess.tournament.service.TournamentService.join(
          nowchess.tournament.domain.model.TournamentId(id), testBot2)
        startRes <- routes.runZIO(
          Request.post(URL(Path.root / "api" / "tournament" / id / "start"), Body.empty)
            .addHeaders(authHeader("director-token"))
        )
        startBody <- startRes.body.asString
      yield assertTrue(
        startRes.status == Status.Ok,
        startBody.contains("\"status\":\"started\""),
      )
    ,
    test("DELETE /api/tournament by non-director is forbidden"):
      for
        createRes <- routes.runZIO(
          Request.post(
            URL(Path.root / "api" / "tournament"),
            Body.fromString(createTournamentBody(name = "ForbidDelete")),
          ).addHeaders(authHeader("director-token"))
        )
        body <- createRes.body.asString
        id = extractId(body)
        delRes <- routes.runZIO(
          Request.delete(URL(Path.root / "api" / "tournament" / id))
            .addHeaders(authHeader("user-token"))
        )
      yield assertTrue(delRes.status == Status.Forbidden)
    ,
    test("invalid token returns unauthorized"):
      for
        response <- routes.runZIO(
          Request.post(
            URL(Path.root / "api" / "tournament"),
            Body.fromString(createTournamentBody()),
          ).addHeaders(authHeader("bad-token"))
        )
      yield assertTrue(response.status == Status.Unauthorized)
    ,
  ).provide(allLayers) @@ TestAspect.sequential
