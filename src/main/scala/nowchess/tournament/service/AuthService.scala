package nowchess.tournament.service

import zio.*
import nowchess.tournament.domain.model.{BotId, UserId}

final case class AuthContext(
  userId: UserId,
  botId: Option[BotId],
  isBot: Boolean,
)

trait AuthService:
  def validateToken(token: String): Task[AuthContext]

object AuthService:
  def validateToken(token: String): ZIO[AuthService, Throwable, AuthContext] =
    ZIO.serviceWithZIO(_.validateToken(token))
