package nowchess.tournament.http

import zio.*
import zio.test.*
import zio.http.*
import nowchess.tournament.http.routes.TournamentRoutes
import nowchess.tournament.http.RouteTestHelpers.*

object TournamentRoutesFormSpec extends ZIOSpecDefault:

  val routes = TournamentRoutes.routes

  private def postCreate(body: String) =
    routes.runZIO(
      Request.post(URL(Path.root / "api" / "tournament"), Body.fromString(body))
        .addHeaders(authHeader("director-token"))
    )

  def spec = suite("TournamentRoutes form parsing")(
    test("missing clockLimit") {
      for r <- postCreate("name=Test&nbRounds=2&clockIncrement=5")
      yield assertTrue(r.status == Status.BadRequest)
    },
    test("missing clockIncrement") {
      for r <- postCreate("name=Test&nbRounds=2&clockLimit=300")
      yield assertTrue(r.status == Status.BadRequest)
    },
    test("missing nbRounds") {
      for r <- postCreate("name=Test&clockLimit=300&clockIncrement=5")
      yield assertTrue(r.status == Status.BadRequest)
    },
    test("invalid nbRounds") {
      for r <- postCreate("name=Test&nbRounds=abc&clockLimit=300&clockIncrement=5")
      yield assertTrue(r.status == Status.BadRequest)
    },
    test("with rated=false") {
      for
        r <- postCreate("name=Test&nbRounds=1&clockLimit=300&clockIncrement=5&rated=false")
        body <- r.body.asString
      yield assertTrue(r.status == Status.Created)
    },
    test("with format=league") {
      for
        r <- postCreate("name=Test&nbRounds=1&clockLimit=300&clockIncrement=5&format=league")
        body <- r.body.asString
      yield assertTrue(
        r.status == Status.Created,
        body.contains("\"format\":\"league\""),
      )
    },
    test("with format=singleElimination") {
      for
        r <- postCreate("name=Test&nbRounds=1&clockLimit=300&clockIncrement=5&format=singleElimination")
      yield assertTrue(r.status == Status.Created)
    },
    test("with format=doubleElimination") {
      for
        r <- postCreate("name=Test&nbRounds=1&clockLimit=300&clockIncrement=5&format=doubleElimination")
      yield assertTrue(r.status == Status.Created)
    },
    test("with format=groupStage and groupSize") {
      for
        r <- postCreate("name=Test&nbRounds=1&clockLimit=300&clockIncrement=5&format=groupStage&groupSize=4")
      yield assertTrue(r.status == Status.Created)
    },
    test("with matchesPerPairing=3") {
      for
        r <- postCreate("name=Test&nbRounds=1&clockLimit=300&clockIncrement=5&matchesPerPairing=3")
        body <- r.body.asString
      yield assertTrue(
        r.status == Status.Created,
        body.contains("\"matchesPerPairing\":3"),
      )
    },
    test("with startPosition FEN") {
      for
        r <- postCreate("name=Test&nbRounds=1&clockLimit=300&clockIncrement=5&startPosition=rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR%20b%20KQkq%20e3%200%201")
      yield assertTrue(r.status == Status.Created)
    },
    test("with format=randomKnockout") {
      for
        r <- postCreate("name=Test&nbRounds=1&clockLimit=300&clockIncrement=5&format=randomKnockout")
        body <- r.body.asString
      yield assertTrue(
        r.status == Status.Created,
        body.contains("\"format\":\"randomKnockout\""),
      )
    },
    test("with a named opening resolves the start position") {
      for
        r <- postCreate("name=Test&nbRounds=1&clockLimit=300&clockIncrement=5&opening=vienna")
      yield assertTrue(r.status == Status.Created)
    },
    test("with an unknown opening returns bad request") {
      for
        r <- postCreate("name=Test&nbRounds=1&clockLimit=300&clockIncrement=5&opening=ghost")
      yield assertTrue(r.status == Status.BadRequest)
    },
    test("with maxConcurrentGames") {
      for
        r <- postCreate("name=Test&nbRounds=1&clockLimit=300&clockIncrement=5&maxConcurrentGames=2")
      yield assertTrue(r.status == Status.Created)
    },
    test("with an unknown registered bot returns bad request") {
      for
        r <- postCreate("name=Test&nbRounds=1&clockLimit=300&clockIncrement=5&bots=ghostbot")
      yield assertTrue(r.status == Status.BadRequest)
    },
    test("empty body") {
      for r <- postCreate("")
      yield assertTrue(r.status == Status.BadRequest)
    },
    test("errorToResponse for conflict") {
      for
        // Create tournament, join bot twice → conflict
        r1 <- postCreate(createTournamentBody(name = "Conflict"))
        body <- r1.body.asString
        id = extractId(body)
        _ <- nowchess.tournament.service.TournamentService.join(
          nowchess.tournament.domain.model.TournamentId(id),
          nowchess.tournament.domain.model.BotRef(nowchess.tournament.domain.model.BotId("bot1"), "Bot1"))
        r2 <- routes.runZIO(Request.get(URL(Path.root / "api" / "tournament" / id)))
      yield assertTrue(r1.status == Status.Created)
    },
  ).provide(allLayers) @@ TestAspect.sequential
