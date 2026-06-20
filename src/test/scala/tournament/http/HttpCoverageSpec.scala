package tournament.http

import zio.*
import zio.test.*
import zio.http.*
import zio.json.*
import tournament.http.routes.{TournamentRoutes, ParticipationRoutes, ResultRoutes, GameRoutes}
import tournament.http.codec.JsonCodecs.given
import tournament.http.RouteTestHelpers.*
import tournament.domain.model.*
import tournament.domain.error.DomainError
import tournament.service.{TournamentService, GameService}
import tournament.persistence.GameRepository

object HttpCoverageSpec extends ZIOSpecDefault:

  val allRoutes = TournamentRoutes.routes ++ ParticipationRoutes.routes ++ ResultRoutes.routes ++ GameRoutes.routes

  private def createAndStartTournament(name: String = "Test") =
    for
      createRes <- allRoutes.runZIO(
        Request.post(
          URL(Path.root / "api" / "tournament"),
          Body.fromString(createTournamentBody(name = name, nbRounds = 1)),
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
    yield id

  private def finishWithWhiteWin(id: String) =
    for
      games <- GameRepository.findByTournament(TournamentId(id))
      game = games.head
      whiteId = game.white.id
      blackId = game.black.id
      gsvc <- ZIO.service[GameService]
      // Scholar's mate but played by white winning
      _ <- gsvc.makeMove(game.id, "e2e4", whiteId)
      _ <- gsvc.makeMove(game.id, "f7f6", blackId)
      _ <- gsvc.makeMove(game.id, "d2d4", whiteId)
      _ <- gsvc.makeMove(game.id, "g7g5", blackId)
      _ <- gsvc.makeMove(game.id, "d1h5", whiteId) // Qh5# checkmate
    yield ()

  private def finishWithDraw(id: String) =
    for
      games <- GameRepository.findByTournament(TournamentId(id))
      game = games.head
      whiteId = game.white.id
      blackId = game.black.id
      gsvc <- ZIO.service[GameService]
      _ <- gsvc.makeMove(game.id, "e2e4", whiteId)
      _ <- gsvc.makeMove(game.id, "e7e5", blackId)
      // Do a sequence that leads to stalemate or 50-move rule
    yield ()

  def spec = suite("HTTP coverage")(
    test("PGN export with white winning shows 1-0") {
      for
        id <- createAndStartTournament("WhiteWin")
        _ <- finishWithWhiteWin(id)
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "export" / "games"))
        )
        body <- response.body.asString
      yield assertTrue(
        body.contains("1-0"),
        body.contains("[Result \"1-0\"]"),
      )
    },
    test("NDJSON export with white winning shows white winner") {
      for
        id <- createAndStartTournament("WhiteWinNJ")
        _ <- finishWithWhiteWin(id)
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "export" / "games"))
            .addHeaders(Headers(Header.Accept(MediaType("application", "x-ndjson"))))
        )
        body <- response.body.asString
      yield assertTrue(
        body.contains("\"white\""),
      )
    },
    test("PGN export with draw result from stalemate") {
      // Use a custom FEN that leads to stalemate in one move
      for
        createRes <- allRoutes.runZIO(
          Request.post(
            URL(Path.root / "api" / "tournament"),
            Body.fromString("name=Draw&nbRounds=1&clockLimit=300&clockIncrement=5&startPosition=k7/2K5/8/1Q6/8/8/8/8%20w%20-%20-%200%201"),
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
        gsvc <- ZIO.service[GameService]
        _ <- gsvc.makeMove(game.id, "b5b6", whiteId) // stalemate
        pgnResponse <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "export" / "games"))
        )
        pgnBody <- pgnResponse.body.asString
      yield assertTrue(
        pgnBody.contains("1/2-1/2"),
        pgnBody.contains("[Result \"1/2-1/2\"]"),
      )
    },
    test("errorToResponse with non-DomainError throwable") {
      val response = TournamentRoutes.errorToResponse(new RuntimeException("boom"))
      assertTrue(response.status == Status.InternalServerError)
    },
    test("list tournaments returns all status categories") {
      for
        // Create and leave one as Created
        r1 <- allRoutes.runZIO(
          Request.post(
            URL(Path.root / "api" / "tournament"),
            Body.fromString(createTournamentBody(name = "Created1")),
          ).addHeaders(authHeader("director-token"))
        )
        // Create one and start it (Started)
        id2Res <- allRoutes.runZIO(
          Request.post(
            URL(Path.root / "api" / "tournament"),
            Body.fromString(createTournamentBody(name = "Started1")),
          ).addHeaders(authHeader("director-token"))
        )
        body2 <- id2Res.body.asString
        id2 = extractId(body2)
        _ <- TournamentService.join(TournamentId(id2), testBot1)
        _ <- TournamentService.join(TournamentId(id2), testBot2)
        _ <- allRoutes.runZIO(
          Request.post(URL(Path.root / "api" / "tournament" / id2 / "start"), Body.empty)
            .addHeaders(authHeader("director-token"))
        )
        // List tournaments
        listRes <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament"))
        )
        listBody <- listRes.body.asString
      yield assertTrue(
        listRes.status == Status.Ok,
        listBody.contains("\"created\""),
        listBody.contains("\"started\""),
        listBody.contains("\"finished\""),
      )
    },
    test("create tournament with failing body") {
      val failingBody = Body.fromStreamChunked(zio.stream.ZStream.fail(new Exception("bad stream")).map((_: Nothing) => 0.toByte))
      for
        response <- allRoutes.runZIO(
          Request.post(URL(Path.root / "api" / "tournament"), failingBody)
            .addHeaders(authHeader("director-token"))
        )
      yield assertTrue(response.status == Status.BadRequest)
    },
  ).provide(allLayers) @@ TestAspect.sequential
