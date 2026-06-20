package tournament.http

import zio.*
import zio.test.*
import zio.http.*
import tournament.http.routes.{OpeningRoutes, BotRegistryRoutes, TournamentRoutes, ParticipationRoutes}
import tournament.http.codec.JsonCodecs.given
import tournament.http.RouteTestHelpers.*
import tournament.service.*
import tournament.persistence.*

object RegistryRoutesSpec extends ZIOSpecDefault:

  private val routes =
    OpeningRoutes.routes ++ BotRegistryRoutes.routes ++ TournamentRoutes.routes ++ ParticipationRoutes.routes

  private val layer =
    (InMemoryTournamentRepository.layer ++
     InMemoryGameRepository.layer ++
     StreamServiceLive.layer ++
     testAuthService ++
     (InMemoryOpeningRepository.layer >>> OpeningServiceLive.layer) ++
     ((InMemoryBotRegistryRepository.layer ++ testAuthService) >>> BotRegistryServiceLive.layer) ++
     Scope.default) >+>
    TournamentServiceLive.layer

  private def get(path: Path) = routes.runZIO(Request.get(URL(path)))
  private def post(path: Path, body: String, token: Option[String]) =
    val base = Request.post(URL(path), Body.fromString(body))
    routes.runZIO(token.fold(base)(t => base.addHeaders(authHeader(t))))
  private def del(path: Path, token: Option[String]) =
    val base = Request.delete(URL(path))
    routes.runZIO(token.fold(base)(t => base.addHeaders(authHeader(t))))

  private val openings = Path.root / "api" / "openings"
  private val bots = Path.root / "api" / "bots"

  def spec = suite("Registry & participant routes")(

    test("GET /api/openings is public and lists the catalog") {
      for
        r <- get(openings)
        body <- r.body.asString
      yield assertTrue(r.status == Status.Ok, body.contains("vienna"))
    },
    test("POST /api/openings registers a custom opening") {
      for
        r <- post(openings, """{"name":"Wild","fen":"some-fen"}""", Some("director-token"))
        body <- r.body.asString
      yield assertTrue(r.status == Status.Created, body.contains("\"key\":\"wild\""))
    },
    test("POST /api/openings requires auth") {
      for r <- post(openings, """{"name":"Wild","fen":"some-fen"}""", None)
      yield assertTrue(r.status == Status.Unauthorized)
    },
    test("POST /api/openings rejects invalid json") {
      for r <- post(openings, "not-json", Some("director-token"))
      yield assertTrue(r.status == Status.BadRequest)
    },
    test("POST /api/openings rejects a blank name") {
      for r <- post(openings, """{"name":"  ","fen":"x"}""", Some("director-token"))
      yield assertTrue(r.status == Status.BadRequest)
    },

    test("GET /api/bots is public") {
      for r <- get(bots)
      yield assertTrue(r.status == Status.Ok)
    },
    test("POST /api/bots registers a bot") {
      for
        r <- post(bots, """{"name":"Stockfish","endpoint":"http://x"}""", Some("director-token"))
        body <- r.body.asString
      yield assertTrue(r.status == Status.Created, body.contains("Stockfish"))
    },
    test("POST /api/bots requires auth") {
      for r <- post(bots, """{"name":"X"}""", None)
      yield assertTrue(r.status == Status.Unauthorized)
    },
    test("POST /api/bots rejects invalid json") {
      for r <- post(bots, "nope", Some("director-token"))
      yield assertTrue(r.status == Status.BadRequest)
    },
    test("DELETE /api/bots/{id} removes a bot") {
      for
        reg <- ZIO.service[BotRegistryService]
        bot <- reg.register("Doomed", None)
        r <- del(bots / bot.id.value, Some("director-token"))
      yield assertTrue(r.status == Status.NoContent)
    },
    test("DELETE /api/bots/{id} requires auth") {
      for r <- del(bots / "anything", None)
      yield assertTrue(r.status == Status.Unauthorized)
    },

    test("POST /participants adds a registered bot to the tournament") {
      for
        reg <- ZIO.service[BotRegistryService]
        bot <- reg.register("Adder", None)
        cr <- post(Path.root / "api" / "tournament", createTournamentBody(), Some("director-token"))
        crBody <- cr.body.asString
        id = extractId(crBody)
        r <- post(Path.root / "api" / "tournament" / id / "participants", s"""{"botId":"${bot.id.value}"}""", Some("director-token"))
        body <- r.body.asString
      yield assertTrue(r.status == Status.Ok, body.contains("\"ok\":true"))
    },
    test("POST /participants is forbidden for non-directors") {
      for
        reg <- ZIO.service[BotRegistryService]
        bot <- reg.register("Adder2", None)
        cr <- post(Path.root / "api" / "tournament", createTournamentBody(), Some("director-token"))
        crBody <- cr.body.asString
        id = extractId(crBody)
        r <- post(Path.root / "api" / "tournament" / id / "participants", s"""{"botId":"${bot.id.value}"}""", Some("user-token"))
      yield assertTrue(r.status == Status.Forbidden)
    },
    test("POST /participants fails for an unknown bot") {
      for
        cr <- post(Path.root / "api" / "tournament", createTournamentBody(), Some("director-token"))
        crBody <- cr.body.asString
        id = extractId(crBody)
        r <- post(Path.root / "api" / "tournament" / id / "participants", """{"botId":"ghost"}""", Some("director-token"))
      yield assertTrue(r.status == Status.NotFound)
    },
    test("POST /participants rejects invalid json") {
      for
        cr <- post(Path.root / "api" / "tournament", createTournamentBody(), Some("director-token"))
        crBody <- cr.body.asString
        id = extractId(crBody)
        r <- post(Path.root / "api" / "tournament" / id / "participants", "broken", Some("director-token"))
      yield assertTrue(r.status == Status.BadRequest)
    },
    test("POST /participants requires auth") {
      for r <- post(Path.root / "api" / "tournament" / "x" / "participants", """{"botId":"y"}""", None)
      yield assertTrue(r.status == Status.Unauthorized)
    },

    test("POST /api/openings with a failing body returns bad request") {
      for
        r <- routes.runZIO(Request.post(URL(openings), failingBody).addHeaders(authHeader("director-token")))
      yield assertTrue(r.status == Status.BadRequest)
    },
    test("POST /api/bots with a failing body returns bad request") {
      for
        r <- routes.runZIO(Request.post(URL(bots), failingBody).addHeaders(authHeader("director-token")))
      yield assertTrue(r.status == Status.BadRequest)
    },
    test("POST /participants with a failing body returns bad request") {
      for
        cr <- post(Path.root / "api" / "tournament", createTournamentBody(), Some("director-token"))
        crBody <- cr.body.asString
        id = extractId(crBody)
        r <- routes.runZIO(
          Request.post(URL(Path.root / "api" / "tournament" / id / "participants"), failingBody)
            .addHeaders(authHeader("director-token")))
      yield assertTrue(r.status == Status.BadRequest)
    },
  ).provide(layer) @@ TestAspect.sequential

  private def failingBody =
    Body.fromStreamChunked(zio.stream.ZStream.fail(new Exception("bad stream")).map((_: Nothing) => 0.toByte))
