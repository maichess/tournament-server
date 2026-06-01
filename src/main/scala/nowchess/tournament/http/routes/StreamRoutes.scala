package nowchess.tournament.http.routes

import zio.*
import zio.http.*
import zio.json.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.service.*
import nowchess.tournament.http.codec.JsonCodecs.{*, given}
import nowchess.tournament.http.middleware.AuthMiddleware

object StreamRoutes:

  def routes: Routes[TournamentService & StreamService & AuthService, Nothing] =
    Routes(
      Method.GET / "api" / "tournament" / string("id") / "stream" ->
        handler((id: String, req: Request) => streamTournament(id, req)),
    )

  private def streamTournament(id: String, req: Request): ZIO[TournamentService & StreamService & AuthService, Nothing, Response] =
    (for
      _ <- AuthMiddleware.extractAuth(req)
      _ <- TournamentService.get(TournamentId(id))
      stream <- StreamService.subscribeTournament(TournamentId(id))
      body = Body.fromStreamChunked(stream.mapConcatChunk(e => zio.Chunk.fromArray((e.toJson + "\n").getBytes)))
    yield Response(
      status = Status.Ok,
      headers = Headers(Header.ContentType(MediaType("application", "x-ndjson"))),
      body = body,
    )).catchAll(e => ZIO.succeed(TournamentRoutes.errorToResponse(e)))
