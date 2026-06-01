package nowchess.tournament.service

import zio.*
import nowchess.tournament.domain.model.{BotId, UserId}

final class JwtAuthService(secret: String) extends AuthService:
  override def validateToken(token: String): Task[AuthContext] =
    ZIO.attempt:
      val parts = token.split('.')
      if parts.length < 2 then throw new Exception("invalid token format")
      val payload = new String(java.util.Base64.getUrlDecoder.decode(parts(1)), "UTF-8")
      val sub = extractField(payload, "sub").getOrElse(throw new Exception("missing sub"))
      val isBot = extractField(payload, "isBot").contains("true")
      AuthContext(
        userId = UserId(sub),
        botId = if isBot then Some(BotId(sub)) else None,
        isBot = isBot,
      )

  private def extractField(json: String, field: String): Option[String] =
    val pattern = s""""$field"\\s*:\\s*"?([^",}]+)"?""".r
    pattern.findFirstMatchIn(json).map(_.group(1))

object JwtAuthService:
  def layer(secret: String): ULayer[AuthService] =
    ZLayer.succeed(JwtAuthService(secret))
