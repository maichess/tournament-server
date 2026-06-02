package nowchess.tournament.service

import zio.*
import zio.test.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.persistence.{InMemoryIdentityRepository, IdentityRepository}
import java.nio.charset.StandardCharsets
import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

object JwtAuthServiceSpec extends ZIOSpecDefault:

  private val testSecret = "secret"
  private val encoder = Base64.getUrlEncoder.withoutPadding

  private def b64(s: String): String =
    encoder.encodeToString(s.getBytes(StandardCharsets.UTF_8))

  private def sign(input: String, secret: String): String =
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(SecretKeySpec(secret.getBytes(StandardCharsets.UTF_8), "HmacSHA256"))
    encoder.encodeToString(mac.doFinal(input.getBytes(StandardCharsets.UTF_8)))

  private def makeToken(
    sub: String,
    isBot: Boolean,
    secret: String = testSecret,
    alg: String = "HS256",
  ): String =
    val header = b64(s"""{"alg":"$alg"}""")
    val payload = b64(s"""{"sub":"$sub","isBot":"$isBot"}""")
    val signature = sign(s"$header.$payload", secret)
    s"$header.$payload.$signature"

  private val serviceLayers = InMemoryIdentityRepository.layer >>> JwtAuthService.layer(testSecret)

  def spec = suite("JwtAuthService")(
    test("validates bot token"):
      for
        service <- ZIO.service[AuthService]
        ctx <- service.validateToken(makeToken("bot1", isBot = true))
      yield assertTrue(
        ctx.userId == UserId("bot1"),
        ctx.botId.contains(BotId("bot1")),
        ctx.isBot,
      )
    ,
    test("validates user token"):
      for
        service <- ZIO.service[AuthService]
        ctx <- service.validateToken(makeToken("user1", isBot = false))
      yield assertTrue(
        ctx.userId == UserId("user1"),
        ctx.botId.isEmpty,
        !ctx.isBot,
      )
    ,
    test("rejects single-part token"):
      for
        service <- ZIO.service[AuthService]
        result <- service.validateToken("invalid").either
      yield assertTrue(result.isLeft)
    ,
    test("rejects two-part token"):
      for
        service <- ZIO.service[AuthService]
        result <- service.validateToken("a.b").either
      yield assertTrue(result.isLeft)
    ,
    test("rejects alg=none"):
      for
        service <- ZIO.service[AuthService]
        result <- service.validateToken(makeToken("bot1", isBot = true, alg = "none")).either
      yield assertTrue(result.isLeft)
    ,
    test("rejects unsupported algorithm"):
      for
        service <- ZIO.service[AuthService]
        result <- service.validateToken(makeToken("bot1", isBot = true, alg = "RS256")).either
      yield assertTrue(result.isLeft)
    ,
    test("rejects token signed with a different secret"):
      val token = makeToken("bot1", isBot = true, secret = "different-secret")
      for
        service <- ZIO.service[AuthService]
        result <- service.validateToken(token).either
      yield assertTrue(result.isLeft)
    ,
    test("rejects tampered payload (sig no longer matches)"):
      val parts = makeToken("bot1", isBot = true).split('.')
      val tamperedPayload = b64("""{"sub":"attacker","isBot":"true"}""")
      val tampered = s"${parts(0)}.$tamperedPayload.${parts(2)}"
      for
        service <- ZIO.service[AuthService]
        result <- service.validateToken(tampered).either
      yield assertTrue(result.isLeft)
    ,
    test("rejects token with missing sub field"):
      val header = b64("""{"alg":"HS256"}""")
      val payload = b64("""{"isBot":"true"}""")
      val signature = sign(s"$header.$payload", testSecret)
      for
        service <- ZIO.service[AuthService]
        result <- service.validateToken(s"$header.$payload.$signature").either
      yield assertTrue(result.isLeft)
    ,
    test("layer provides AuthService"):
      (for ctx <- AuthService.validateToken(makeToken("layerBot", isBot = true, secret = "test-secret"))
      yield assertTrue(ctx.userId == UserId("layerBot"))
      ).provide(InMemoryIdentityRepository.layer >>> JwtAuthService.layer("test-secret"))
    ,
    test("register creates a new bot identity and returns valid token"):
      for
        service <- ZIO.service[AuthService]
        result <- service.register("TestBot", isBot = true)
        ctx <- service.validateToken(result.token)
      yield assertTrue(
        result.id.startsWith("bot_"),
        ctx.userId == UserId(result.id),
        ctx.isBot,
      )
    ,
    test("register creates a new user identity and returns valid token"):
      for
        service <- ZIO.service[AuthService]
        result <- service.register("TestUser", isBot = false)
        ctx <- service.validateToken(result.token)
      yield assertTrue(
        result.id.startsWith("usr_"),
        ctx.userId == UserId(result.id),
        !ctx.isBot,
      )
    ,
    test("register returns existing identity for same name and isBot"):
      for
        service <- ZIO.service[AuthService]
        first <- service.register("SameBot", isBot = true)
        second <- service.register("SameBot", isBot = true)
      yield assertTrue(first.id == second.id)
    ,
    test("register distinguishes bot and user with same name"):
      for
        service <- ZIO.service[AuthService]
        botResult <- service.register("Shared", isBot = true)
        userResult <- service.register("Shared", isBot = false)
      yield assertTrue(botResult.id != userResult.id)
    ,
    test("createToken produces a roundtrip-valid token"):
      for
        service <- ZIO.service[AuthService]
        svc = service.asInstanceOf[JwtAuthService]
        token = svc.createToken("test-sub", isBot = true)
        ctx <- service.validateToken(token)
      yield assertTrue(
        ctx.userId == UserId("test-sub"),
        ctx.isBot,
      )
    ,
  ).provideShared(serviceLayers)
