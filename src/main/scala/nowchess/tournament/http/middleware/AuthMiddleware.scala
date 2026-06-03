package nowchess.tournament.http.middleware

import zio.*
import zio.http.*
import nowchess.tournament.domain.model.{BotId, UserId, BotRef}
import nowchess.tournament.domain.error.DomainError
import nowchess.tournament.service.{AuthService, AuthContext}

object AuthMiddleware:

  def extractAuth(req: Request): ZIO[AuthService, DomainError, AuthContext] =
    val tokenOpt = req.header(Header.Authorization).flatMap:
      case Header.Authorization.Bearer(token) => Some(token.value.asString)
      case _ => None
    tokenOpt match
      case None => ZIO.fail(DomainError.Unauthorized("missing authorization header"))
      case Some(token) =>
        AuthService.validateToken(token).mapError(e => DomainError.Unauthorized(e.getMessage))

  def requireBot(ctx: AuthContext): IO[DomainError, BotRef] =
    ctx.botId match
      case Some(botId) if ctx.isBot => ZIO.succeed(BotRef(botId, ctx.name))
      case _ => ZIO.fail(DomainError.Forbidden("must be a bot account"))
