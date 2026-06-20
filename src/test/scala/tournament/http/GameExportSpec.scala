package tournament.http

import zio.*
import zio.test.*
import zio.http.*
import zio.json.*
import tournament.http.routes.{TournamentRoutes, ParticipationRoutes, ResultRoutes, GameRoutes}
import tournament.http.codec.JsonCodecs.given
import tournament.http.RouteTestHelpers.*
import tournament.domain.model.*
import tournament.service.{TournamentService, GameService}
import tournament.persistence.GameRepository

object GameExportSpec extends ZIOSpecDefault:

  val allRoutes = TournamentRoutes.routes ++ ParticipationRoutes.routes ++ ResultRoutes.routes ++ GameRoutes.routes

  private def createAndFinishTournament =
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
      game = games.head
      whiteId = game.white.id
      blackId = game.black.id
      gsvc <- ZIO.service[GameService]
      _ <- gsvc.makeMove(game.id, "f2f3", whiteId)
      _ <- gsvc.makeMove(game.id, "e7e5", blackId)
      _ <- gsvc.makeMove(game.id, "g2g4", whiteId)
      _ <- gsvc.makeMove(game.id, "d8h4", blackId)
    yield id

  def spec = suite("GameExport")(
    test("PGN export contains game result") {
      for
        id <- createAndFinishTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "export" / "games"))
        )
        body <- response.body.asString
      yield assertTrue(
        response.status == Status.Ok,
        body.contains("[White"),
        body.contains("[Black"),
        body.contains("[Result"),
        body.contains("0-1") || body.contains("1-0"),
      )
    },
    test("NDJSON export contains winner field") {
      for
        id <- createAndFinishTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "export" / "games"))
            .addHeaders(Headers(Header.Accept(MediaType("application", "x-ndjson"))))
        )
        body <- response.body.asString
      yield assertTrue(
        response.status == Status.Ok,
        body.contains("\"winner\""),
      )
    },
  ).provide(allLayers) @@ TestAspect.sequential
