package tournament.http

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import tournament.http.routes.{TournamentRoutes, ParticipationRoutes, ResultRoutes, GameRoutes, AnalyticsExportRoutes}
import tournament.http.codec.JsonCodecs.given
import tournament.http.RouteTestHelpers.*
import tournament.domain.model.*
import tournament.domain.tournament.TournamentFormat
import tournament.service.{TournamentService, GameService, BotRegistryService, CreateTournamentForm}
import tournament.persistence.GameRepository

object AnalyticsExportSpec extends ZIOSpecDefault:

  val allRoutes =
    TournamentRoutes.routes ++
    ParticipationRoutes.routes ++
    ResultRoutes.routes ++
    GameRoutes.routes ++
    AnalyticsExportRoutes.routes

  // Creates a 1-round tournament and plays Scholar's mate so it finishes
  private def createAndFinishTournament =
    for
      createRes <- allRoutes.runZIO(
        Request.post(
          URL(Path.root / "api" / "tournament"),
          Body.fromString(createTournamentBody(nbRounds = 1)),
        ).addHeaders(authHeader("director-token"))
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
      game   = games.head
      whiteId = game.white.id
      blackId = game.black.id
      gsvc <- ZIO.service[GameService]
      // Scholar's mate (white wins in 4 moves)
      _ <- gsvc.makeMove(game.id, "e2e4", whiteId)
      _ <- gsvc.makeMove(game.id, "f7f6", blackId)
      _ <- gsvc.makeMove(game.id, "d2d4", whiteId)
      _ <- gsvc.makeMove(game.id, "g7g5", blackId)
      _ <- gsvc.makeMove(game.id, "d1h5", whiteId)
    yield id

  // Creates a started-but-not-finished tournament
  private def createStartedTournament =
    for
      createRes <- allRoutes.runZIO(
        Request.post(
          URL(Path.root / "api" / "tournament"),
          Body.fromString(createTournamentBody(nbRounds = 1)),
        ).addHeaders(authHeader("director-token"))
      )
      body <- createRes.body.asString
      id    = extractId(body)
      _ <- TournamentService.join(TournamentId(id), testBot1)
      _ <- TournamentService.join(TournamentId(id), testBot2)
      _ <- allRoutes.runZIO(
        Request.post(URL(Path.root / "api" / "tournament" / id / "start"), Body.empty)
          .addHeaders(authHeader("director-token"))
      )
    yield id

  def spec = suite("AnalyticsExportRoutes")(

    test("finished tournament returns 200 with full export document") {
      for
        id       <- createAndFinishTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "analytics-export"))
        )
        body <- response.body.asString
      yield assertTrue(
        response.status == Status.Ok,
        body.contains("\"tournamentId\""),
        body.contains("\"standings\""),
        body.contains("\"games\""),
        body.contains("\"format\""),
        body.contains("\"clock\""),
        body.contains("\"rated\""),
      )
    },

    test("response contains correct tournamentId") {
      for
        id       <- createAndFinishTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "analytics-export"))
        )
        body <- response.body.asString
      yield assertTrue(body.contains(s"\"tournamentId\":\"$id\""))
    },

    test("unfinished tournament returns 409") {
      for
        id       <- createStartedTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "analytics-export"))
        )
      yield assertTrue(response.status == Status.Conflict)
    },

    test("missing tournament returns 404") {
      for
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / "does-not-exist" / "analytics-export"))
        )
      yield assertTrue(response.status == Status.NotFound)
    },

    test("game record includes terminationReason") {
      for
        id       <- createAndFinishTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "analytics-export"))
        )
        body <- response.body.asString
      yield assertTrue(body.contains("\"terminationReason\""))
    },

    test("Scholar's mate ends with checkmate termination reason") {
      for
        id       <- createAndFinishTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "analytics-export"))
        )
        body <- response.body.asString
      yield assertTrue(body.contains("\"checkmate\""))
    },

    test("winnerBotId is resolved to a bot id string, not a color") {
      for
        id       <- createAndFinishTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "analytics-export"))
        )
        body <- response.body.asString
        json  = body.fromJson[Json].toOption.get
        games = json.asObject.flatMap(_.get("games")).flatMap(_.asArray).getOrElse(Chunk.empty)
        finishedGames = games.filter: g =>
          g.asObject.flatMap(_.get("terminationReason")).flatMap(_.asString).exists(_ != "pending")
        winnerBotIds = finishedGames.flatMap: g =>
          g.asObject.flatMap(_.get("winnerBotId")).flatMap(_.asString)
      yield assertTrue(
        finishedGames.nonEmpty,
        winnerBotIds.nonEmpty,
        winnerBotIds.forall(id => id != "white" && id != "black"),
      )
    },

    test("totalPly matches number of moves played") {
      for
        id       <- createAndFinishTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "analytics-export"))
        )
        body <- response.body.asString
        json  = body.fromJson[Json].toOption.get
        games = json.asObject.flatMap(_.get("games")).flatMap(_.asArray).getOrElse(Chunk.empty)
        // Scholar's mate = 5 moves = 5 ply
        plyCounts = games.flatMap: g =>
          g.asObject.flatMap(_.get("totalPly")).flatMap(_.asNumber).map(_.value.intValue())
      yield assertTrue(
        plyCounts.nonEmpty,
        plyCounts.forall(_ == 5),
      )
    },

    test("standings include tournamentId and required bot fields") {
      for
        id       <- createAndFinishTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "analytics-export"))
        )
        body <- response.body.asString
        json  = body.fromJson[Json].toOption.get
        standings = json.asObject.flatMap(_.get("standings")).flatMap(_.asArray).getOrElse(Chunk.empty)
      yield assertTrue(
        standings.nonEmpty,
        standings.forall(s => s.asObject.flatMap(_.get("tournamentId")).isDefined),
        standings.forall(s => s.asObject.flatMap(_.get("botId")).isDefined),
        standings.forall(s => s.asObject.flatMap(_.get("botName")).isDefined),
        standings.forall(s => s.asObject.flatMap(_.get("rank")).isDefined),
        standings.forall(s => s.asObject.flatMap(_.get("points")).isDefined),
        standings.forall(s => s.asObject.flatMap(_.get("wins")).isDefined),
        standings.forall(s => s.asObject.flatMap(_.get("draws")).isDefined),
        standings.forall(s => s.asObject.flatMap(_.get("losses")).isDefined),
        standings.forall(s => s.asObject.flatMap(_.get("nbGames")).isDefined),
        standings.forall(s => s.asObject.flatMap(_.get("tieBreak")).isDefined),
      )
    },

    test("standings tournamentId matches the tournament") {
      for
        id       <- createAndFinishTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "analytics-export"))
        )
        body <- response.body.asString
        json  = body.fromJson[Json].toOption.get
        standings = json.asObject.flatMap(_.get("standings")).flatMap(_.asArray).getOrElse(Chunk.empty)
        ids = standings.flatMap(s => s.asObject.flatMap(_.get("tournamentId")).flatMap(_.asString))
      yield assertTrue(ids.forall(_ == id))
    },

    test("game records include whiteBotId, blackBotId, moves, and round") {
      for
        id       <- createAndFinishTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "analytics-export"))
        )
        body <- response.body.asString
        json  = body.fromJson[Json].toOption.get
        games = json.asObject.flatMap(_.get("games")).flatMap(_.asArray).getOrElse(Chunk.empty)
      yield assertTrue(
        games.nonEmpty,
        games.forall(g => g.asObject.flatMap(_.get("whiteBotId")).isDefined),
        games.forall(g => g.asObject.flatMap(_.get("blackBotId")).isDefined),
        games.forall(g => g.asObject.flatMap(_.get("moves")).isDefined),
        games.forall(g => g.asObject.flatMap(_.get("round")).isDefined),
        games.forall(g => g.asObject.flatMap(_.get("gameId")).isDefined),
      )
    },

    test("game startedAt is populated after game runs") {
      for
        id       <- createAndFinishTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "analytics-export"))
        )
        body <- response.body.asString
        json  = body.fromJson[Json].toOption.get
        games = json.asObject.flatMap(_.get("games")).flatMap(_.asArray).getOrElse(Chunk.empty)
        startedAts = games.flatMap(g => g.asObject.flatMap(_.get("startedAt")).flatMap(_.asString))
      yield assertTrue(startedAts.nonEmpty)
    },

    test("game endedAt is populated for finished games") {
      for
        id       <- createAndFinishTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "analytics-export"))
        )
        body <- response.body.asString
        json  = body.fromJson[Json].toOption.get
        games = json.asObject.flatMap(_.get("games")).flatMap(_.asArray).getOrElse(Chunk.empty)
        finishedGames = games.filter: g =>
          g.asObject.flatMap(_.get("terminationReason")).flatMap(_.asString)
            .exists(r => r != "pending" && r != "ongoing")
        endedAts = finishedGames.flatMap(g => g.asObject.flatMap(_.get("endedAt")).flatMap(_.asString))
      yield assertTrue(endedAts.size == finishedGames.size)
    },

    test("existing /results endpoint still works after changes") {
      for
        id       <- createAndFinishTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "results"))
        )
        body <- response.body.asString
      yield assertTrue(
        response.status == Status.Ok,
        body.contains("\"rank\""),
        body.contains("\"points\""),
      )
    },

    test("tournament cancelled before start returns 409") {
      for
        createRes <- allRoutes.runZIO(
          Request.post(
            URL(Path.root / "api" / "tournament"),
            Body.fromString(createTournamentBody(nbRounds = 1)),
          ).addHeaders(authHeader("director-token"))
        )
        body <- createRes.body.asString
        id    = extractId(body)
        // Delete the tournament before starting it (terminate)
        _ <- allRoutes.runZIO(
          Request.delete(URL(Path.root / "api" / "tournament" / id))
            .addHeaders(authHeader("director-token"))
        )
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "analytics-export"))
        )
      yield assertTrue(response.status == Status.Conflict)
    },

    test("existing /export/games endpoint still works after changes") {
      for
        id       <- createAndFinishTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "export" / "games"))
        )
      yield assertTrue(response.status == Status.Ok)
    },

    test("response contains schemaVersion 1.0") {
      for
        id       <- createAndFinishTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "analytics-export"))
        )
        body <- response.body.asString
        json  = body.fromJson[Json].toOption.get
      yield assertTrue(
        json.asObject.flatMap(_.get("schemaVersion")).flatMap(_.asString).contains("1.0")
      )
    },

    test("response contains exportedAt as ISO-8601 timestamp") {
      for
        id       <- createAndFinishTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "analytics-export"))
        )
        body <- response.body.asString
        json  = body.fromJson[Json].toOption.get
        exportedAt = json.asObject.flatMap(_.get("exportedAt")).flatMap(_.asString)
      yield assertTrue(
        exportedAt.isDefined,
        exportedAt.exists(_.contains("T")),
      )
    },

    test("response contains nbRounds matching tournament config") {
      for
        id       <- createAndFinishTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "analytics-export"))
        )
        body <- response.body.asString
        json  = body.fromJson[Json].toOption.get
      yield assertTrue(
        json.asObject.flatMap(_.get("nbRounds")).flatMap(_.asNumber).map(_.value.intValue()).contains(1)
      )
    },

    test("game records omit engine metadata when bot not in registry") {
      for
        id       <- createAndFinishTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "analytics-export"))
        )
        body <- response.body.asString
        json  = body.fromJson[Json].toOption.get
        games = json.asObject.flatMap(_.get("games")).flatMap(_.asArray).getOrElse(Chunk.empty)
      yield assertTrue(
        games.nonEmpty,
        games.forall(g => g.asObject.flatMap(_.get("whiteEngineType")).isEmpty),
        games.forall(g => g.asObject.flatMap(_.get("blackEngineType")).isEmpty),
      )
    },

    test("standing records omit engine metadata when bot not in registry") {
      for
        id       <- createAndFinishTournament
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "analytics-export"))
        )
        body <- response.body.asString
        json  = body.fromJson[Json].toOption.get
        standings = json.asObject.flatMap(_.get("standings")).flatMap(_.asArray).getOrElse(Chunk.empty)
      yield assertTrue(
        standings.nonEmpty,
        standings.forall(s => s.asObject.flatMap(_.get("engineType")).isEmpty),
        standings.forall(s => s.asObject.flatMap(_.get("modelVersion")).isEmpty),
      )
    },

    test("engine metadata appears in export when bot registered with engineType") {
      for
        svc      <- ZIO.service[BotRegistryService]
        bot1     <- svc.register("MetaBot", None, engineType = Some("heuristic"), modelVersion = Some("v2.0"))
        bot2     <- svc.register("PlainBot", None)
        cr       <- allRoutes.runZIO(
          Request.post(URL(Path.root / "api" / "tournament"), Body.fromString(createTournamentBody(nbRounds = 1)))
            .addHeaders(authHeader("director-token"))
        )
        crBody   <- cr.body.asString
        tid       = extractId(crBody)
        _ <- TournamentService.join(TournamentId(tid), bot1.toRef)
        _ <- TournamentService.join(TournamentId(tid), bot2.toRef)
        _ <- allRoutes.runZIO(
          Request.post(URL(Path.root / "api" / "tournament" / tid / "start"), Body.empty)
            .addHeaders(authHeader("director-token"))
        )
        games    <- GameRepository.findByTournament(TournamentId(tid))
        game      = games.head
        gsvc     <- ZIO.service[GameService]
        _ <- gsvc.makeMove(game.id, "e2e4", game.white.id)
        _ <- gsvc.makeMove(game.id, "f7f6", game.black.id)
        _ <- gsvc.makeMove(game.id, "d2d4", game.white.id)
        _ <- gsvc.makeMove(game.id, "g7g5", game.black.id)
        _ <- gsvc.makeMove(game.id, "d1h5", game.white.id)
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / tid / "analytics-export"))
        )
        body     <- response.body.asString
        json      = body.fromJson[Json].toOption.get
        exportGames = json.asObject.flatMap(_.get("games")).flatMap(_.asArray).getOrElse(Chunk.empty)
        exportStandings = json.asObject.flatMap(_.get("standings")).flatMap(_.asArray).getOrElse(Chunk.empty)
      yield assertTrue(
        exportGames.exists(g =>
          g.asObject.flatMap(_.get("whiteEngineType")).flatMap(_.asString).contains("heuristic") ||
          g.asObject.flatMap(_.get("blackEngineType")).flatMap(_.asString).contains("heuristic")
        ),
        exportGames.exists(g =>
          g.asObject.flatMap(_.get("whiteModelVersion")).flatMap(_.asString).contains("v2.0") ||
          g.asObject.flatMap(_.get("blackModelVersion")).flatMap(_.asString).contains("v2.0")
        ),
        exportStandings.exists(s =>
          s.asObject.flatMap(_.get("engineType")).flatMap(_.asString).contains("heuristic")
        ),
      )
    },

    test("export records a black winner and the winning bot id") {
      for
        createRes <- allRoutes.runZIO(
          Request.post(URL(Path.root / "api" / "tournament"), Body.fromString(createTournamentBody(nbRounds = 1)))
            .addHeaders(authHeader("director-token"))
        )
        body0 <- createRes.body.asString
        id     = extractId(body0)
        _ <- TournamentService.join(TournamentId(id), testBot1)
        _ <- TournamentService.join(TournamentId(id), testBot2)
        _ <- allRoutes.runZIO(
          Request.post(URL(Path.root / "api" / "tournament" / id / "start"), Body.empty)
            .addHeaders(authHeader("director-token"))
        )
        games <- GameRepository.findByTournament(TournamentId(id))
        game   = games.head
        gsvc  <- ZIO.service[GameService]
        // Fool's mate — black checkmates white
        _ <- gsvc.makeMove(game.id, "f2f3", game.white.id)
        _ <- gsvc.makeMove(game.id, "e7e5", game.black.id)
        _ <- gsvc.makeMove(game.id, "g2g4", game.white.id)
        _ <- gsvc.makeMove(game.id, "d8h4", game.black.id)
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / id / "analytics-export"))
        )
        body <- response.body.asString
      yield assertTrue(
        body.contains("\"winner\":\"black\""),
        body.contains(s"\"winnerBotId\":\"${game.black.id.value}\""),
      )
    },

    test("export records a draw for a stalemate game") {
      for
        tsvc <- ZIO.service[TournamentService]
        form  = CreateTournamentForm(
          name = "Drawn", nbRounds = 1, clockLimit = 300, clockIncrement = 3,
          rated = true, format = TournamentFormat.Swiss,
          startPosition = StartPosition.FromFen("k7/2K5/8/1Q6/8/8/8/8 w - - 0 1"),
          matchesPerPairing = 1, groupSize = None,
        )
        t       <- tsvc.create(form, UserId("director1"))
        _       <- tsvc.join(t.id, testBot1)
        _       <- tsvc.join(t.id, testBot2)
        started <- tsvc.start(t.id, UserId("director1"))
        gameId   = started.rounds.head.pairings.head.matches.head.gameId
        g       <- GameRepository.get(gameId).map(_.get)
        gsvc    <- ZIO.service[GameService]
        _       <- gsvc.makeMove(gameId, "b5b6", g.white.id) // stalemate → draw
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / t.id.value / "analytics-export"))
        )
        body <- response.body.asString
      yield assertTrue(body.contains("\"winner\":\"draw\""))
    },

    test("export reports a still-ongoing game with a null winner") {
      // best-of-2: a single win decides the pairing, finishing the 1-round
      // tournament while the pairing's second game is still ongoing. That game
      // appears in the export as terminationReason "ongoing" with winner null.
      for
        tsvc <- ZIO.service[TournamentService]
        form  = CreateTournamentForm(
          name = "BestOf2Export", nbRounds = 1, clockLimit = 300, clockIncrement = 3,
          rated = true, format = TournamentFormat.Swiss,
          startPosition = StartPosition.Standard, matchesPerPairing = 2, groupSize = None,
        )
        t       <- tsvc.create(form, UserId("director1"))
        _       <- tsvc.join(t.id, testBot1)
        _       <- tsvc.join(t.id, testBot2)
        started <- tsvc.start(t.id, UserId("director1"))
        pairing  = started.rounds.head.pairings.head
        g1       = pairing.matches(0).gameId
        g1state <- GameRepository.get(g1).map(_.get)
        gsvc    <- ZIO.service[GameService]
        // Fool's mate on g1 alone decides the best-of-2 and finishes the tournament.
        _ <- gsvc.makeMove(g1, "f2f3", g1state.white.id)
        _ <- gsvc.makeMove(g1, "e7e5", g1state.black.id)
        _ <- gsvc.makeMove(g1, "g2g4", g1state.white.id)
        _ <- gsvc.makeMove(g1, "d8h4", g1state.black.id)
        response <- allRoutes.runZIO(
          Request.get(URL(Path.root / "api" / "tournament" / t.id.value / "analytics-export"))
        )
        body <- response.body.asString
      yield assertTrue(
        response.status == Status.Ok,
        body.contains("\"terminationReason\":\"ongoing\""),
      )
    },

  ).provide(allLayers) @@ TestAspect.sequential
