package nowchess.tournament.http

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.http.*
import nowchess.tournament.http.middleware.AuthMiddleware
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.error.DomainError
import nowchess.tournament.service.*

object AuthMiddlewareSpec extends ZIOSpecDefault:

  val testAuth = RouteTestHelpers.testAuthService

  def spec = suite("AuthMiddleware")(
    test("extractAuth with valid bot token"):
      val req = Request.get(URL.empty).addHeaders(RouteTestHelpers.authHeader("bot1-token"))
      for
        ctx <- AuthMiddleware.extractAuth(req)
      yield assertTrue(
        ctx.userId == UserId("bot1"),
        ctx.botId.contains(BotId("bot1")),
        ctx.isBot,
      )
    ,
    test("extractAuth with valid user token"):
      val req = Request.get(URL.empty).addHeaders(RouteTestHelpers.authHeader("user-token"))
      for
        ctx <- AuthMiddleware.extractAuth(req)
      yield assertTrue(
        ctx.userId == UserId("user1"),
        !ctx.isBot,
      )
    ,
    test("extractAuth without auth header fails"):
      val req = Request.get(URL.empty)
      for
        result <- AuthMiddleware.extractAuth(req).either
      yield assertTrue(result.isLeft)
    ,
    test("extractAuth with invalid token fails"):
      val req = Request.get(URL.empty).addHeaders(RouteTestHelpers.authHeader("bad-token"))
      for
        result <- AuthMiddleware.extractAuth(req).either
      yield assertTrue(result.isLeft)
    ,
    test("extractAuth with non-Bearer auth header fails"):
      val req = Request.get(URL.empty).addHeaders(Headers(Header.Authorization.Basic("user", "pass")))
      for
        result <- AuthMiddleware.extractAuth(req).either
      yield assertTrue(result.isLeft)
    ,
    test("requireBot with bot context succeeds"):
      val ctx = AuthContext(UserId("bot1"), Some(BotId("bot1")), isBot = true, name = "TestBot")
      for
        bot <- AuthMiddleware.requireBot(ctx)
      yield assertTrue(bot.id == BotId("bot1"), bot.name == "TestBot")
    ,
    test("requireBot with user context fails"):
      val ctx = AuthContext(UserId("user1"), None, isBot = false, name = "user")
      for
        result <- AuthMiddleware.requireBot(ctx).either
      yield assertTrue(result.isLeft)
    ,
  ).provide(testAuth)
