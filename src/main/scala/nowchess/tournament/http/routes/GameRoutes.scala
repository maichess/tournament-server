package nowchess.tournament.http.routes

import zio.*
import zio.http.*
import zio.json.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.service.*
import nowchess.tournament.http.codec.JsonCodecs.{*, given}
import nowchess.tournament.http.middleware.AuthMiddleware

object GameRoutes:

  def routes: Routes[GameService & StreamService & AuthService, Nothing] =
    Routes(
      Method.GET / "api" / "tournament" / string("id") / "game" / string("gameId") ->
        handler((id: String, gameId: String, _: Request) => getGame(gameId)),
      Method.GET / "api" / "tournament" / string("id") / "game" / string("gameId") / "stream" ->
        handler((id: String, gameId: String, req: Request) => streamGame(gameId, req)),
      Method.POST / "api" / "tournament" / string("id") / "game" / string("gameId") / "move" / string("uci") ->
        handler((id: String, gameId: String, uci: String, req: Request) => makeMove(gameId, uci, req)),
    )

  private def getGame(gameId: String): ZIO[GameService, Nothing, Response] =
    GameService.getGame(GameId(gameId)).map: g =>
      Response.json(g.toJson)
    .catchAll(e => ZIO.succeed(TournamentRoutes.errorToResponse(e)))

  private def streamGame(gameId: String, req: Request): ZIO[GameService & StreamService & AuthService, Nothing, Response] =
    (for
      _ <- AuthMiddleware.extractAuth(req)
      game <- GameService.getGame(GameId(gameId))
      stream <- StreamService.subscribeGame(GameId(gameId))
      body = Body.fromStreamChunked(stream.mapConcatChunk(e => zio.Chunk.fromArray((e.toJson + "\n").getBytes)))
    yield Response(
      status = Status.Ok,
      headers = Headers(Header.ContentType(MediaType("application", "x-ndjson"))),
      body = body,
    )).catchAll(e => ZIO.succeed(TournamentRoutes.errorToResponse(e)))

  private def makeMove(gameId: String, uci: String, req: Request): ZIO[GameService & AuthService, Nothing, Response] =
    (for
      ctx <- AuthMiddleware.extractAuth(req)
      bot <- AuthMiddleware.requireBot(ctx)
      _ <- GameService.makeMove(GameId(gameId), uci, bot.id)
    yield Response.json(OkResponse(true).toJson)
    ).catchAll(e => ZIO.succeed(TournamentRoutes.errorToResponse(e)))
