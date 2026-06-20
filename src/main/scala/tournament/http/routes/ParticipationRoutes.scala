package tournament.http.routes

import zio.*
import zio.http.*
import zio.json.*
import tournament.domain.model.*
import tournament.domain.error.DomainError
import tournament.service.*
import tournament.http.codec.JsonCodecs.{*, given}
import tournament.http.middleware.AuthMiddleware

object ParticipationRoutes:

  final case class AddParticipantRequest(botId: String)
  object AddParticipantRequest:
    given JsonDecoder[AddParticipantRequest] = DeriveJsonDecoder.gen[AddParticipantRequest]

  def routes: Routes[TournamentService & AuthService, Nothing] =
    Routes(
      Method.POST / "api" / "tournament" / string("id") / "join" ->
        handler((id: String, req: Request) => joinTournament(id, req)),
      Method.POST / "api" / "tournament" / string("id") / "withdraw" ->
        handler((id: String, req: Request) => withdrawTournament(id, req)),
      Method.POST / "api" / "tournament" / string("id") / "participants" ->
        handler((id: String, req: Request) => addParticipant(id, req)),
    )

  private def addParticipant(id: String, req: Request): ZIO[TournamentService & AuthService, Nothing, Response] =
    (for
      ctx <- AuthMiddleware.extractAuth(req)
      body <- req.body.asString.mapError(e => DomainError.BadRequest(e.getMessage))
      parsed <- ZIO.fromEither(body.fromJson[AddParticipantRequest])
        .mapError(err => DomainError.BadRequest(s"invalid request body: $err"))
      _ <- TournamentService.addRegisteredBot(TournamentId(id), BotId(parsed.botId), ctx.userId)
    yield Response.json(OkResponse(true).toJson)
    ).catchAll(e => ZIO.succeed(TournamentRoutes.errorToResponse(e)))

  private def joinTournament(id: String, req: Request): ZIO[TournamentService & AuthService, Nothing, Response] =
    (for
      ctx <- AuthMiddleware.extractAuth(req)
      bot <- AuthMiddleware.requireBot(ctx)
      _ <- TournamentService.join(TournamentId(id), bot)
    yield Response.json(OkResponse(true).toJson)
    ).catchAll(e => ZIO.succeed(TournamentRoutes.errorToResponse(e)))

  private def withdrawTournament(id: String, req: Request): ZIO[TournamentService & AuthService, Nothing, Response] =
    (for
      ctx <- AuthMiddleware.extractAuth(req)
      bot <- AuthMiddleware.requireBot(ctx)
      _ <- TournamentService.withdraw(TournamentId(id), bot.id)
    yield Response.json(OkResponse(true).toJson)
    ).catchAll(e => ZIO.succeed(TournamentRoutes.errorToResponse(e)))
