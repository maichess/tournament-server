package nowchess.tournament.http.routes

import zio.*
import zio.http.*
import zio.json.*
import nowchess.tournament.domain.model.{BotId, RegisteredBot}
import nowchess.tournament.domain.error.DomainError
import nowchess.tournament.service.BotRegistryService
import nowchess.tournament.service.AuthService
import nowchess.tournament.http.codec.JsonCodecs.{*, given}
import nowchess.tournament.http.middleware.AuthMiddleware

object BotRegistryRoutes:

  final case class RegisterBotRequest(name: String, endpoint: Option[String])
  object RegisterBotRequest:
    given JsonDecoder[RegisterBotRequest] = DeriveJsonDecoder.gen[RegisterBotRequest]

  final case class BotListResponse(bots: Vector[RegisteredBot])
  object BotListResponse:
    given JsonEncoder[BotListResponse] = DeriveJsonEncoder.gen[BotListResponse]

  def routes: Routes[BotRegistryService & AuthService, Nothing] =
    Routes(
      Method.GET / "api" / "bots" -> handler(listBots),
      Method.POST / "api" / "bots" -> handler((req: Request) => registerBot(req)),
      Method.DELETE / "api" / "bots" / string("id") ->
        handler((id: String, req: Request) => deleteBot(id, req)),
    )

  private def listBots: ZIO[BotRegistryService, Nothing, Response] =
    BotRegistryService.list.map: bots =>
      Response.json(BotListResponse(bots).toJson)
    .catchAll(e => ZIO.succeed(TournamentRoutes.errorToResponse(e)))

  private def registerBot(req: Request): ZIO[BotRegistryService & AuthService, Nothing, Response] =
    (for
      _ <- AuthMiddleware.extractAuth(req)
      body <- req.body.asString.mapError(e => DomainError.BadRequest(e.getMessage))
      parsed <- ZIO.fromEither(body.fromJson[RegisterBotRequest])
        .mapError(err => DomainError.BadRequest(s"invalid request body: $err"))
      bot <- BotRegistryService.register(parsed.name, parsed.endpoint)
    yield Response.json(bot.toJson).status(Status.Created)
    ).catchAll(e => ZIO.succeed(TournamentRoutes.errorToResponse(e)))

  private def deleteBot(id: String, req: Request): ZIO[BotRegistryService & AuthService, Nothing, Response] =
    (for
      _ <- AuthMiddleware.extractAuth(req)
      _ <- BotRegistryService.delete(BotId(id))
    yield Response.status(Status.NoContent)
    ).catchAll(e => ZIO.succeed(TournamentRoutes.errorToResponse(e)))
