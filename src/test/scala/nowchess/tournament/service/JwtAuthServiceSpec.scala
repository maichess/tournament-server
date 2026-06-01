package nowchess.tournament.service

import zio.*
import zio.test.*
import zio.test.Assertion.*
import nowchess.tournament.domain.model.*

object JwtAuthServiceSpec extends ZIOSpecDefault:

  private def makeToken(sub: String, isBot: Boolean): String =
    val header = java.util.Base64.getUrlEncoder.encodeToString("""{"alg":"none"}""".getBytes)
    val payload = java.util.Base64.getUrlEncoder.encodeToString(
      s"""{"sub":"$sub","isBot":"$isBot"}""".getBytes
    )
    s"$header.$payload.sig"

  private val service = JwtAuthService("secret")

  def spec = suite("JwtAuthService")(
    test("validates bot token"):
      for
        ctx <- service.validateToken(makeToken("bot1", isBot = true))
      yield assertTrue(
        ctx.userId == UserId("bot1"),
        ctx.botId.contains(BotId("bot1")),
        ctx.isBot,
      )
    ,
    test("validates user token"):
      for
        ctx <- service.validateToken(makeToken("user1", isBot = false))
      yield assertTrue(
        ctx.userId == UserId("user1"),
        ctx.botId.isEmpty,
        !ctx.isBot,
      )
    ,
    test("rejects invalid token format"):
      for
        result <- service.validateToken("invalid").either
      yield assertTrue(result.isLeft)
    ,
    test("rejects token with missing sub field"):
      val header = java.util.Base64.getUrlEncoder.encodeToString("""{"alg":"none"}""".getBytes)
      val payload = java.util.Base64.getUrlEncoder.encodeToString("""{"isBot":"true"}""".getBytes)
      val token = s"$header.$payload.sig"
      for
        result <- service.validateToken(token).either
      yield assertTrue(result.isLeft)
    ,
    test("layer provides AuthService"):
      (for
        ctx <- AuthService.validateToken(makeToken("layerBot", isBot = true))
      yield assertTrue(ctx.userId == UserId("layerBot"))
      ).provide(JwtAuthService.layer("test-secret"))
    ,
  )
