package nowchess.tournament.http

import zio.*
import zio.test.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.*
import nowchess.tournament.http.routes.{TournamentRoutes, ParticipationRoutes, GameRoutes, StreamRoutes, NdjsonStream}
import nowchess.tournament.http.codec.JsonCodecs.given
import nowchess.tournament.http.RouteTestHelpers.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.game.GameStatus
import nowchess.tournament.domain.event.GameEvent
import nowchess.tournament.service.{TournamentService, StreamService}
import nowchess.tournament.persistence.GameRepository

object GameStreamCloseSpec extends ZIOSpecDefault:

  val allRoutes = TournamentRoutes.routes ++ ParticipationRoutes.routes ++ GameRoutes.routes ++ StreamRoutes.routes

  private def startedGame =
    for
      createRes <- allRoutes.runZIO(
        Request.post(URL(Path.root / "api" / "tournament"), Body.fromString(createTournamentBody()))
          .addHeaders(authHeader("director-token"))
      )
      body <- createRes.body.asString
      id    = extractId(body)
      _ <- TournamentService.join(TournamentId(id), testBot1)
      _ <- TournamentService.join(TournamentId(id), testBot2)
      _ <- allRoutes.runZIO(
        Request.post(URL(Path.root / "api" / "tournament" / id / "start"), Body.empty)
          .addHeaders(authHeader("director-token"))
      )
      games <- GameRepository.findByTournament(TournamentId(id))
    yield (id, games.head.id)

  private def lines(resp: Response): ZStream[Any, Throwable, String] =
    resp.body.asStream.via(ZPipeline.utf8Decode >>> ZPipeline.splitLines).filter(_.nonEmpty)

  def spec = suite("Game stream lifecycle")(
    test("the game stream delivers events and closes after a GameEnd"):
      for
        (tid, gameId) <- startedGame
        resp <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / tid / "game" / gameId.value / "stream"))
            .addHeaders(authHeader("bot1-token"))
        )
        // The subscription's queue is unbounded, so buffering the GameEnd before
        // we consume makes this deterministic: the stream replays the snapshot,
        // delivers the GameEnd, then closes (no fiber-timing race).
        _         <- StreamService.publishGame(gameId, GameEvent.GameEnd(Some(Color.White), GameStatus.Checkmate))
        collected <- lines(resp).runCollect
      yield assertTrue(
        collected.exists(_.contains("gameState")),
        collected.exists(_.contains("gameEnd")),
      )
    ,
    test("streaming an already-finished game emits only its snapshot then closes"):
      for
        (tid, gameId) <- startedGame
        gameRepo <- ZIO.service[GameRepository]
        g        <- gameRepo.get(gameId).map(_.get)
        _        <- gameRepo.save(g.copy(status = GameStatus.Checkmate, winner = Some(Color.White)))
        resp     <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / tid / "game" / gameId.value / "stream"))
            .addHeaders(authHeader("bot1-token"))
        )
        // runCollect returning at all proves the stream closes for a finished game.
        collected <- lines(resp).runCollect
      yield assertTrue(
        collected.exists(_.contains("gameState")),
        !collected.exists(_.contains("gameEnd")),
        collected.forall(l => l.contains("gameState") || l.contains("heartbeat")),
      )
    ,
    test("the heartbeat line is a complete JSON object, not a blank line"):
      assertTrue(NdjsonStream.heartbeatLine.fromJson[Json].isRight)
  ).provide(allLayers) @@ TestAspect.sequential @@ TestAspect.timeout(60.seconds)
