package nowchess.tournament.service

import zio.*
import nowchess.tournament.domain.model.{BotId, UserId}
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

final class JwtAuthService(secret: String) extends AuthService:
  private val decoder = Base64.getUrlDecoder
  private val encoder = Base64.getUrlEncoder.withoutPadding

  override def validateToken(token: String): Task[AuthContext] =
    ZIO.attempt:
      val parts = token.split('.')
      if parts.length != 3 then throw new Exception("invalid token format")
      val header = new String(decoder.decode(parts(0)), StandardCharsets.UTF_8)
      if !extractField(header, "alg").contains("HS256") then
        throw new Exception("unsupported algorithm")
      val expected = encoder.encodeToString(hmacSha256(s"${parts(0)}.${parts(1)}"))
      if !constantTimeEquals(expected, parts(2)) then
        throw new Exception("invalid signature")
      val payload = new String(decoder.decode(parts(1)), StandardCharsets.UTF_8)
      val sub = extractField(payload, "sub").getOrElse(throw new Exception("missing sub"))
      val isBot = extractField(payload, "isBot").contains("true")
      AuthContext(
        userId = UserId(sub),
        botId = if isBot then Some(BotId(sub)) else None,
        isBot = isBot,
      )

  private def hmacSha256(input: String): Array[Byte] =
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(SecretKeySpec(secret.getBytes(StandardCharsets.UTF_8), "HmacSHA256"))
    mac.doFinal(input.getBytes(StandardCharsets.UTF_8))

  private def constantTimeEquals(a: String, b: String): Boolean =
    MessageDigest.isEqual(
      a.getBytes(StandardCharsets.UTF_8),
      b.getBytes(StandardCharsets.UTF_8),
    )

  private def extractField(json: String, field: String): Option[String] =
    val pattern = s""""$field"\\s*:\\s*"?([^",}]+)"?""".r
    pattern.findFirstMatchIn(json).map(_.group(1))

object JwtAuthService:
  def layer(secret: String): ULayer[AuthService] =
    ZLayer.succeed(JwtAuthService(secret))
