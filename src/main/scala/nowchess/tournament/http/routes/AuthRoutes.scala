package nowchess.tournament.http.routes

import zio.*
import zio.http.*
import zio.json.*
import nowchess.tournament.domain.error.DomainError
import nowchess.tournament.service.{AuthService, RegisterResult}
import nowchess.tournament.http.codec.JsonCodecs.given

object AuthRoutes:

  final case class RegisterRequest(name: String, isBot: Option[Boolean])
  object RegisterRequest:
    given JsonDecoder[RegisterRequest] = DeriveJsonDecoder.gen[RegisterRequest]

  final case class RegisterResponse(id: String, token: String)
  object RegisterResponse:
    given JsonEncoder[RegisterResponse] = DeriveJsonEncoder.gen[RegisterResponse]

  def routes: Routes[AuthService, Nothing] =
    Routes(
      Method.POST / "api" / "auth" / "register" -> handler((req: Request) => register(req)),
    )

  private def register(req: Request): ZIO[AuthService, Nothing, Response] =
    (for
      body <- req.body.asString.mapError(e => DomainError.BadRequest(e.getMessage))
      parsed <- ZIO.fromEither(body.fromJson[RegisterRequest])
        .mapError(err => DomainError.BadRequest(s"invalid request body: $err"))
      _ <- ZIO.when(parsed.name.isBlank)(ZIO.fail(DomainError.BadRequest("name must not be blank")))
      result <- AuthService.register(parsed.name.trim, parsed.isBot.getOrElse(false))
        .mapError(e => DomainError.BadRequest(e.getMessage))
    yield
      val resp = RegisterResponse(result.id, result.token)
      Response.json(resp.toJson).status(Status.Created)
    ).catchAll(e => ZIO.succeed(TournamentRoutes.errorToResponse(e)))
