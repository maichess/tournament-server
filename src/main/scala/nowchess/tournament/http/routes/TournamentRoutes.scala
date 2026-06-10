package nowchess.tournament.http.routes

import zio.*
import zio.http.*
import zio.json.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.tournament.*
import nowchess.tournament.domain.error.DomainError
import nowchess.tournament.service.*
import nowchess.tournament.http.codec.JsonCodecs.{*, given}
import nowchess.tournament.http.middleware.AuthMiddleware

object TournamentRoutes:

  def routes: Routes[TournamentService & AuthService, Nothing] =
    Routes(
      Method.GET / "api" / "tournament" -> handler(listTournaments),
      Method.POST / "api" / "tournament" -> handler((req: Request) => createTournament(req)),
      Method.GET / "api" / "tournament" / string("id") ->
        handler((id: String, _: Request) => getTournament(id)),
      Method.DELETE / "api" / "tournament" / string("id") ->
        handler((id: String, req: Request) => deleteTournament(id, req)),
      Method.POST / "api" / "tournament" / string("id") / "start" ->
        handler((id: String, req: Request) => startTournament(id, req)),
    )

  private def listTournaments: ZIO[TournamentService, Nothing, Response] =
    TournamentService.list.map: statusMap =>
      val body = TournamentListResponse(
        created = statusMap.getOrElse(TournamentStatus.Created, Vector.empty),
        started = statusMap.getOrElse(TournamentStatus.Started, Vector.empty),
        finished = statusMap.getOrElse(TournamentStatus.Finished, Vector.empty),
      )
      Response.json(body.toJson)
    .catchAll(e => ZIO.succeed(errorToResponse(e)))

  private def createTournament(req: Request): ZIO[TournamentService & AuthService, Nothing, Response] =
    (for
      ctx <- AuthMiddleware.extractAuth(req)
      body <- req.body.asString.mapError(e => DomainError.BadRequest(e.getMessage))
      form <- parseForm(body)
      tournament <- TournamentService.create(form, ctx.userId)
    yield Response.json(tournament.toJson).status(Status.Created)
    ).catchAll(e => ZIO.succeed(errorToResponse(e)))

  private def getTournament(id: String): ZIO[TournamentService, Nothing, Response] =
    TournamentService.get(TournamentId(id)).map: t =>
      Response.json(t.toJson)
    .catchAll(e => ZIO.succeed(errorToResponse(e)))

  private def deleteTournament(id: String, req: Request): ZIO[TournamentService & AuthService, Nothing, Response] =
    (for
      ctx <- AuthMiddleware.extractAuth(req)
      _ <- TournamentService.delete(TournamentId(id), ctx.userId)
    yield Response.status(Status.NoContent)
    ).catchAll(e => ZIO.succeed(errorToResponse(e)))

  private def startTournament(id: String, req: Request): ZIO[TournamentService & AuthService, Nothing, Response] =
    (for
      ctx <- AuthMiddleware.extractAuth(req)
      t <- TournamentService.start(TournamentId(id), ctx.userId)
    yield Response.json(t.toJson)
    ).catchAll(e => ZIO.succeed(errorToResponse(e)))

  private def parseForm(body: String): IO[DomainError, CreateTournamentForm] =
    val params = body.split('&').flatMap: p =>
      val parts = p.split('=')
      if parts.length == 2 then Some(java.net.URLDecoder.decode(parts(0), "UTF-8") -> java.net.URLDecoder.decode(parts(1), "UTF-8"))
      else None
    .toMap
    for
      name <- ZIO.fromOption(params.get("name")).mapError(_ => DomainError.BadRequest("missing name"))
      nbRounds <- ZIO.fromOption(params.get("nbRounds").flatMap(_.toIntOption)).mapError(_ => DomainError.BadRequest("missing or invalid nbRounds"))
      clockLimit <- ZIO.fromOption(params.get("clockLimit").flatMap(_.toIntOption)).mapError(_ => DomainError.BadRequest("missing or invalid clockLimit"))
      clockInc <- ZIO.fromOption(params.get("clockIncrement").flatMap(_.toIntOption)).mapError(_ => DomainError.BadRequest("missing or invalid clockIncrement"))
    yield CreateTournamentForm(
      name = name,
      nbRounds = nbRounds,
      clockLimit = clockLimit,
      clockIncrement = clockInc,
      rated = params.get("rated").flatMap(_.toBooleanOption).getOrElse(true),
      format = params.get("format").flatMap(f => summon[JsonDecoder[TournamentFormat]].decodeJson(s"\"$f\"").toOption).getOrElse(TournamentFormat.Swiss),
      startPosition = StartPosition.fromString(params.getOrElse("startPosition", "standard")),
      matchesPerPairing = params.get("matchesPerPairing").flatMap(_.toIntOption).getOrElse(1),
      groupSize = params.get("groupSize").flatMap(_.toIntOption),
      opening = params.get("opening"),
      bots = params.get("bots"),
      maxConcurrentGames = params.get("maxConcurrentGames").flatMap(_.toIntOption),
    )

  def errorToResponse(e: Throwable): Response =
    def errJson(de: DomainError): String = (de: DomainError).toJson
    e match
      case e: DomainError.NotFound     => Response.json(errJson(e)).status(Status.NotFound)
      case e: DomainError.Conflict     => Response.json(errJson(e)).status(Status.Conflict)
      case e: DomainError.Forbidden    => Response.json(errJson(e)).status(Status.Forbidden)
      case e: DomainError.BadRequest   => Response.json(errJson(e)).status(Status.BadRequest)
      case e: DomainError.Unauthorized => Response.json(errJson(e)).status(Status.Unauthorized)
      case _                           => Response.json("""{"error":"internal error"}""").status(Status.InternalServerError)
