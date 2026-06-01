package nowchess.tournament.http

import zio.*
import zio.test.*
import zio.http.*
import zio.json.*
import zio.stream.*
import nowchess.tournament.http.routes.{TournamentRoutes, ParticipationRoutes, GameRoutes, StreamRoutes}
import nowchess.tournament.http.codec.JsonCodecs.given
import nowchess.tournament.http.RouteTestHelpers.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.game.GameClock
import nowchess.tournament.domain.event.{TournamentEvent, GameEvent}
import nowchess.tournament.service.{TournamentService, StreamService, GameService}
import nowchess.tournament.persistence.GameRepository

object StreamBodySpec extends ZIOSpecDefault:

  val allRoutes = TournamentRoutes.routes ++ ParticipationRoutes.routes ++ GameRoutes.routes ++ StreamRoutes.routes

  private def createStartedTournament =
    for
      createRes <- allRoutes.runZIO(
        Request.post(
          URL(Path.root / "api" / "tournament"),
          Body.fromString(createTournamentBody()),
        ).addHeaders(authHeader("director-token"))
      )
      body <- createRes.body.asString
      id = extractId(body)
      _ <- TournamentService.join(TournamentId(id), testBot1)
      _ <- TournamentService.join(TournamentId(id), testBot2)
      _ <- allRoutes.runZIO(
        Request.post(URL(Path.root / "api" / "tournament" / id / "start"), Body.empty)
          .addHeaders(authHeader("director-token"))
      )
      games <- GameRepository.findByTournament(TournamentId(id))
    yield (id, games.head.id)

  def spec = suite("Stream body consumption")(
    test("game stream body delivers NDJSON events when consumed") {
      for
        (tid, gameId) <- createStartedTournament
        // Get the streaming response
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / tid / "game" / gameId.value / "stream"))
            .addHeaders(authHeader("bot1-token"))
        )
        // Publish an event after subscription is created
        _ <- StreamService.publishGame(gameId, GameEvent.MovePlayed("e2e4", "fen", Color.White, GameClock(300, 300)))
        // Consume the first chunk from the stream body
        chunk <- response.body.asStream.take(1).runCollect.timeout(2.seconds)
      yield assertTrue(chunk.isDefined)
    },
    test("tournament stream body delivers NDJSON events when consumed") {
      for
        createRes <- allRoutes.runZIO(
          Request.post(
            URL(Path.root / "api" / "tournament"),
            Body.fromString(createTournamentBody()),
          ).addHeaders(authHeader("director-token"))
        )
        body <- createRes.body.asString
        id = extractId(body)
        // Get the streaming response
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "stream"))
            .addHeaders(authHeader("bot1-token"))
        )
        // Publish an event
        _ <- StreamService.publishTournament(TournamentId(id), TournamentEvent.TournamentStarted)
        // Consume the first chunk
        chunk <- response.body.asStream.take(1).runCollect.timeout(2.seconds)
      yield assertTrue(chunk.isDefined)
    },
  ).provide(allLayers) @@ TestAspect.sequential
