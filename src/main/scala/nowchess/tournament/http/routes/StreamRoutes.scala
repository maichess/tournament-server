package nowchess.tournament.http.routes

import zio.*
import zio.http.*
import zio.json.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.service.*
import nowchess.tournament.http.codec.JsonCodecs.{*, given}
import nowchess.tournament.http.middleware.AuthMiddleware

import nowchess.tournament.domain.tournament.*
import nowchess.tournament.domain.event.TournamentEvent

object StreamRoutes:

  def routes: Routes[TournamentService & StreamService & AuthService, Nothing] =
    Routes(
      Method.GET / "api" / "tournament" / string("id") / "stream" ->
        handler((id: String, req: Request) => streamTournament(id, req)),
    )

  private def streamTournament(id: String, req: Request): ZIO[TournamentService & StreamService & AuthService, Nothing, Response] =
    (for
      _ <- AuthMiddleware.extractAuth(req)
      t <- TournamentService.get(TournamentId(id))
      stream <- StreamService.subscribeTournament(TournamentId(id))
      initialEvents = if (t.status == TournamentStatus.Started && t.rounds.nonEmpty) {
        val currentRound = t.rounds.last
        currentRound.pairings.filter(_.aggregateOutcome.isEmpty).flatMap { pairing =>
          val firstGameId = pairing.matches.head.gameId
          List(
            TournamentEvent.GameStart(t.currentRound, firstGameId, Color.White),
            TournamentEvent.GameStart(t.currentRound, firstGameId, Color.Black)
          )
        }.toVector
      } else Vector.empty
      fullStream = zio.stream.ZStream.fromIterable(initialEvents) ++ stream
      stringStream = fullStream.map(e => e.toJson + "\n")
      heartbeatStream = zio.stream.ZStream.tick(10.seconds).map(_ => "\n")
      combinedStream = stringStream.merge(heartbeatStream)
      body = Body.fromStreamChunked(combinedStream.mapConcatChunk(s => zio.Chunk.fromArray(s.getBytes)))
    yield Response(
      status = Status.Ok,
      headers = Headers(
        Header.ContentType(MediaType("application", "x-ndjson")),
        Header.Custom("Cache-Control", "no-cache"),
        Header.Custom("Connection", "keep-alive")
      ),
      body = body,
    )).catchAll(e => ZIO.succeed(TournamentRoutes.errorToResponse(e)))
