package nowchess.tournament.service

import zio.*
import nowchess.tournament.domain.model.{BotId, UserId}

final case class AuthContext(
  userId: UserId,
  botId: Option[BotId],
  isBot: Boolean,
)

final case class RegisterResult(id: String, token: String)

trait AuthService:
  def validateToken(token: String): Task[AuthContext]
  def register(name: String, isBot: Boolean): Task[RegisterResult]

object AuthService:
  def validateToken(token: String): ZIO[AuthService, Throwable, AuthContext] =
    ZIO.serviceWithZIO(_.validateToken(token))

  def register(name: String, isBot: Boolean): ZIO[AuthService, Throwable, RegisterResult] =
    ZIO.serviceWithZIO(_.register(name, isBot))
