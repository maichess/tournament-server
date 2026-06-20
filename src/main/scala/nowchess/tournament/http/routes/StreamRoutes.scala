package nowchess.tournament.http.routes

import zio.*
import zio.http.*
import zio.stream.ZStream
import nowchess.tournament.domain.model.*
import nowchess.tournament.service.*
import nowchess.tournament.persistence.GameRepository
import nowchess.tournament.http.codec.JsonCodecs.{*, given}
import nowchess.tournament.http.middleware.AuthMiddleware

object StreamRoutes:

  def routes: Routes[TournamentService & StreamService & AuthService & GameRepository, Nothing] =
    Routes(
      Method.GET / "api" / "tournament" / string("id") / "stream" ->
        handler((id: String, req: Request) => streamTournament(id, req)),
    )

  private def streamTournament(id: String, req: Request): ZIO[TournamentService & StreamService & AuthService & GameRepository, Nothing, Response] =
    (for
      _      <- AuthMiddleware.extractAuth(req)
      t      <- TournamentService.get(TournamentId(id))
      games  <- GameRepository.findByTournament(TournamentId(id))
      stream <- StreamService.subscribeTournament(TournamentId(id))
      // Replay gameStart for the games currently in play so a late subscriber
      // catches up, using the same source of truth as the live activation path.
      events = ZStream.fromIterable(GameActivation.activeGameStarts(t, games)) ++ stream
    yield NdjsonStream.response(events)
    ).catchAll(e => ZIO.succeed(TournamentRoutes.errorToResponse(e)))
