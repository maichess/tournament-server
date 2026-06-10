package nowchess.tournament.http

import zio.*
import zio.test.*
import zio.http.*
import nowchess.tournament.http.routes.AuthRoutes
import nowchess.tournament.service.{AuthService, AuthContext, RegisterResult}

object AuthRoutesFailureSpec extends ZIOSpecDefault:

  private val routes = AuthRoutes.routes

  // An AuthService whose registration always fails, to exercise the error path.
  private val failingAuth: ULayer[AuthService] = ZLayer.succeed:
    new AuthService:
      override def validateToken(token: String): Task[AuthContext] =
        ZIO.fail(new Exception("nope"))
      override def register(name: String, isBot: Boolean): Task[RegisterResult] =
        ZIO.fail(new Exception("registration unavailable"))

  private val url = URL(Path.root / "api" / "auth" / "register")

  def spec = suite("AuthRoutes failure paths")(
    test("a failing request body yields bad request") {
      val failingBody =
        Body.fromStreamChunked(zio.stream.ZStream.fail(new Exception("bad stream")).map((_: Nothing) => 0.toByte))
      for r <- routes.runZIO(Request.post(url, failingBody))
      yield assertTrue(r.status == Status.BadRequest)
    },
    test("a failing registration yields bad request") {
      for r <- routes.runZIO(Request.post(url, Body.fromString("""{"name":"Valid","isBot":true}""")))
      yield assertTrue(r.status == Status.BadRequest)
    },
  ).provide(failingAuth ++ Scope.default)
