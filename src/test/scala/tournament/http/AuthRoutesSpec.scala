package tournament.http

import zio.*
import zio.http.*
import zio.test.*
import zio.json.*
import tournament.http.routes.AuthRoutes
import tournament.service.*
import tournament.persistence.*

object AuthRoutesSpec extends ZIOSpecDefault:

  private val routes = AuthRoutes.routes
  private val layers = (InMemoryIdentityRepository.layer >>> JwtAuthService.layer("test-secret")) ++ Scope.default

  private def post(body: String): Request =
    Request.post(
      URL(Path.root / "api" / "auth" / "register"),
      Body.fromString(body),
    )

  def spec = suite("AuthRoutes")(
    test("registers a bot and returns 201 with id and token"):
      for
        resp <- routes.runZIO(post("""{"name":"MyBot","isBot":true}"""))
        body <- resp.body.asString
      yield assertTrue(
        resp.status == Status.Created,
        body.contains("\"id\""),
        body.contains("\"token\""),
      )
    ,
    test("registers a user and returns 201"):
      for
        resp <- routes.runZIO(post("""{"name":"Alice","isBot":false}"""))
        body <- resp.body.asString
      yield assertTrue(
        resp.status == Status.Created,
        body.contains("\"id\""),
      )
    ,
    test("defaults isBot to false when omitted"):
      for
        resp <- routes.runZIO(post("""{"name":"Director"}"""))
        body <- resp.body.asString
      yield assertTrue(
        resp.status == Status.Created,
        body.contains("usr_"),
      )
    ,
    test("returns same id for duplicate registration"):
      for
        resp1 <- routes.runZIO(post("""{"name":"DupeBot","isBot":true}"""))
        body1 <- resp1.body.asString
        resp2 <- routes.runZIO(post("""{"name":"DupeBot","isBot":true}"""))
        body2 <- resp2.body.asString
        id1 = extractId(body1)
        id2 = extractId(body2)
      yield assertTrue(id1 == id2)
    ,
    test("rejects blank name"):
      for
        resp <- routes.runZIO(post("""{"name":"  ","isBot":true}"""))
      yield assertTrue(resp.status == Status.BadRequest)
    ,
    test("rejects invalid JSON"):
      for
        resp <- routes.runZIO(post("not-json"))
      yield assertTrue(resp.status == Status.BadRequest)
    ,
    test("rejects missing name field"):
      for
        resp <- routes.runZIO(post("""{"isBot":true}"""))
      yield assertTrue(resp.status == Status.BadRequest)
    ,
  ).provideShared(layers)

  private def extractId(json: String): String =
    val marker = "\"id\":\""
    val idx = json.indexOf(marker) + marker.length
    json.substring(idx, json.indexOf("\"", idx))
