package tournament.http.routes

import zio.*
import zio.http.*
import zio.json.*
import tournament.domain.opening.Opening
import tournament.domain.error.DomainError
import tournament.service.OpeningService
import tournament.service.AuthService
import tournament.http.codec.JsonCodecs.{*, given}
import tournament.http.middleware.AuthMiddleware

object OpeningRoutes:

  final case class RegisterOpeningRequest(name: String, fen: String, key: Option[String])
  object RegisterOpeningRequest:
    given JsonDecoder[RegisterOpeningRequest] = DeriveJsonDecoder.gen[RegisterOpeningRequest]

  final case class OpeningListResponse(openings: Vector[Opening])
  object OpeningListResponse:
    given JsonEncoder[OpeningListResponse] = DeriveJsonEncoder.gen[OpeningListResponse]

  def routes: Routes[OpeningService & AuthService, Nothing] =
    Routes(
      Method.GET / "api" / "openings" -> handler(listOpenings),
      Method.POST / "api" / "openings" -> handler((req: Request) => registerOpening(req)),
    )

  private def listOpenings: ZIO[OpeningService, Nothing, Response] =
    OpeningService.list.map: openings =>
      Response.json(OpeningListResponse(openings).toJson)
    .catchAll(e => ZIO.succeed(TournamentRoutes.errorToResponse(e)))

  private def registerOpening(req: Request): ZIO[OpeningService & AuthService, Nothing, Response] =
    (for
      _ <- AuthMiddleware.extractAuth(req)
      body <- req.body.asString.mapError(e => DomainError.BadRequest(e.getMessage))
      parsed <- ZIO.fromEither(body.fromJson[RegisterOpeningRequest])
        .mapError(err => DomainError.BadRequest(s"invalid request body: $err"))
      opening <- OpeningService.register(parsed.name, parsed.fen, parsed.key)
    yield Response.json(opening.toJson).status(Status.Created)
    ).catchAll(e => ZIO.succeed(TournamentRoutes.errorToResponse(e)))
