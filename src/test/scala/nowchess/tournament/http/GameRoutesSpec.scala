package nowchess.tournament.http

import zio.*
import zio.stream.*
import zio.test.*
import zio.test.Assertion.*
import zio.http.*
import zio.json.*
import nowchess.tournament.http.routes.{TournamentRoutes, ParticipationRoutes, GameRoutes}
import nowchess.tournament.http.codec.JsonCodecs.given
import nowchess.tournament.http.RouteTestHelpers.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.service.TournamentService
import nowchess.tournament.persistence.GameRepository

object GameRoutesSpec extends ZIOSpecDefault:

  val allRoutes = TournamentRoutes.routes ++ ParticipationRoutes.routes ++ GameRoutes.routes

  private def createStartedAndGetGameId =
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
      gameId = games.head.id.value
    yield (id, gameId)

  def spec = suite("GameRoutes")(
    test("GET /game/{gameId} returns game state"):
      for
        (tid, gid) <- createStartedAndGetGameId
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / tid / "game" / gid))
        )
        body <- response.body.asString
      yield assertTrue(
        response.status == Status.Ok,
        body.contains("\"status\":\"ongoing\""),
      )
    ,
    test("GET /game/{gameId} not found"):
      for
        (tid, _) <- createStartedAndGetGameId
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / tid / "game" / "nonexistent"))
        )
      yield assertTrue(response.status == Status.NotFound)
    ,
    test("POST /move/{uci} makes a move"):
      for
        (tid, gid) <- createStartedAndGetGameId
        game <- GameRepository.get(GameId(gid))
        whiteToken = if game.get.white.id == BotId("bot1") then "bot1-token" else "bot2-token"
        response <- allRoutes.runZIO(
          Request.post(
            URL(Path.root / "api" / "tournament" / tid / "game" / gid / "move" / "e2e4"),
            Body.empty,
          ).addHeaders(authHeader(whiteToken))
        )
        body <- response.body.asString
      yield assertTrue(
        response.status == Status.Ok,
        body.contains("\"ok\":true"),
      )
    ,
    test("POST /move without auth fails"):
      for
        (tid, gid) <- createStartedAndGetGameId
        response <- allRoutes.runZIO(
          Request.post(
            URL(Path.root / "api" / "tournament" / tid / "game" / gid / "move" / "e2e4"),
            Body.empty,
          )
        )
      yield assertTrue(response.status == Status.Unauthorized)
    ,
    test("POST /move by non-bot fails"):
      for
        (tid, gid) <- createStartedAndGetGameId
        response <- allRoutes.runZIO(
          Request.post(
            URL(Path.root / "api" / "tournament" / tid / "game" / gid / "move" / "e2e4"),
            Body.empty,
          ).addHeaders(authHeader("user-token"))
        )
      yield assertTrue(response.status == Status.Forbidden)
    ,
    test("GET /game/{gameId}/stream returns NDJSON"):
      for
        (tid, gid) <- createStartedAndGetGameId
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / tid / "game" / gid / "stream"))
            .addHeaders(authHeader("bot1-token"))
        )
      yield assertTrue(
        response.status == Status.Ok,
        response.header(Header.ContentType).exists(_.mediaType.subType == "x-ndjson"),
      )
    ,
    test("GET /game/{gameId}/stream emits full game state snapshot on connect"):
      for
        (tid, gid) <- createStartedAndGetGameId
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / tid / "game" / gid / "stream"))
            .addHeaders(authHeader("bot1-token"))
        )
        firstLine <- response.body.asStream
          .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
          .runHead
      yield assertTrue(
        firstLine.exists(_.contains("\"type\":\"gameState\"")),
        firstLine.exists(_.contains("\"status\":\"ongoing\"")),
        firstLine.exists(_.contains("\"turn\":\"white\"")),
      )
    ,
    test("GET /game/{gameId}/stream without auth fails"):
      for
        (tid, gid) <- createStartedAndGetGameId
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / tid / "game" / gid / "stream"))
        )
      yield assertTrue(response.status == Status.Unauthorized)
    ,
  ).provide(allLayers) @@ TestAspect.sequential
