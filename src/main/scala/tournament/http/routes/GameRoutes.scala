package tournament.http.routes

import zio.*
import zio.http.*
import zio.json.*
import zio.stream.ZStream
import tournament.domain.model.*
import tournament.domain.event.GameEvent
import tournament.service.*
import tournament.http.codec.JsonCodecs.{*, given}
import tournament.http.middleware.AuthMiddleware

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

  private def isGameEnd(e: GameEvent): Boolean = e match
    case _: GameEvent.GameEnd => true
    case _                    => false

  private def streamGame(gameId: String, req: Request): ZIO[GameService & StreamService & AuthService, Nothing, Response] =
    (for
      _ <- AuthMiddleware.extractAuth(req)
      game <- GameService.getGame(GameId(gameId))
      snapshot = GameEvent.GameState(game.fen, game.movesUci, game.turn, game.clock, game.status, game.winner)
      stream <- StreamService.subscribeGame(GameId(gameId))
      events = if game.status.isTerminal then ZStream.succeed(snapshot)
               else ZStream.succeed(snapshot) ++ stream
    yield NdjsonStream.response(events, closeWhen = Some(isGameEnd))
    ).catchAll(e => ZIO.succeed(TournamentRoutes.errorToResponse(e)))

  private def makeMove(gameId: String, uci: String, req: Request): ZIO[GameService & AuthService, Nothing, Response] =
    (for
      ctx <- AuthMiddleware.extractAuth(req)
      bot <- AuthMiddleware.requireBot(ctx)
      _ <- GameService.makeMove(GameId(gameId), uci, bot.id)
    yield Response.json(OkResponse(true).toJson)
    ).catchAll(e => ZIO.succeed(TournamentRoutes.errorToResponse(e)))
