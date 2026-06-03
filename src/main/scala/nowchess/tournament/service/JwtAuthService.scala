package nowchess.tournament.service

import zio.*
import nowchess.tournament.domain.model.{BotId, UserId}
import nowchess.tournament.persistence.{Identity, IdentityRepository}
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.{Base64, UUID}
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

final class JwtAuthService(secret: String, identityRepo: IdentityRepository) extends AuthService:
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
      val name = extractField(payload, "name").getOrElse("")
      AuthContext(
        userId = UserId(sub),
        botId = if isBot then Some(BotId(sub)) else None,
        isBot = isBot,
        name = name,
      )

  override def register(name: String, isBot: Boolean): Task[RegisterResult] =
    for
      existing <- identityRepo.findByName(name, isBot)
      identity <- existing match
        case Some(id) => ZIO.succeed(id)
        case None =>
          val prefix = if isBot then "bot_" else "usr_"
          val id = Identity(prefix + UUID.randomUUID().toString.take(8), name, isBot)
          identityRepo.save(id).as(id)
      token = createToken(identity.id, identity.isBot, identity.name)
    yield RegisterResult(identity.id, token)

  def createToken(sub: String, isBot: Boolean, name: String = ""): String =
    val header = encoder.encodeToString("""{"alg":"HS256"}""".getBytes(StandardCharsets.UTF_8))
    val isBotStr = if isBot then "true" else "false"
    val escapedName = name.replace("\\", "\\\\").replace("\"", "\\\"")
    val payload = encoder.encodeToString(s"""{"sub":"$sub","isBot":$isBotStr,"name":"$escapedName"}""".getBytes(StandardCharsets.UTF_8))
    val signature = encoder.encodeToString(hmacSha256(s"$header.$payload"))
    s"$header.$payload.$signature"

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
  def layer(secret: String): URLayer[IdentityRepository, AuthService] =
    ZLayer:
      ZIO.service[IdentityRepository].map(repo => JwtAuthService(secret, repo))
