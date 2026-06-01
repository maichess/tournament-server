package nowchess.tournament.http.routes

import zio.*
import zio.http.*
import zio.json.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.service.*
import nowchess.tournament.http.codec.JsonCodecs.{*, given}
import nowchess.tournament.http.middleware.AuthMiddleware

object ParticipationRoutes:

  def routes: Routes[TournamentService & AuthService, Nothing] =
    Routes(
      Method.POST / "api" / "tournament" / string("id") / "join" ->
        handler((id: String, req: Request) => joinTournament(id, req)),
      Method.POST / "api" / "tournament" / string("id") / "withdraw" ->
        handler((id: String, req: Request) => withdrawTournament(id, req)),
    )

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
