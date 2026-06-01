package nowchess.tournament.http

import zio.*
import zio.http.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.service.*
import nowchess.tournament.persistence.*

object RouteTestHelpers:

  val testBot1 = BotRef(BotId("bot1"), "TestBot1")
  val testBot2 = BotRef(BotId("bot2"), "TestBot2")
  val testUserId = UserId("user1")

  val testAuthService: ULayer[AuthService] = ZLayer.succeed:
    new AuthService:
      override def validateToken(token: String): Task[AuthContext] =
        token match
          case "bot1-token" => ZIO.succeed(AuthContext(UserId("bot1"), Some(BotId("bot1")), isBot = true))
          case "bot2-token" => ZIO.succeed(AuthContext(UserId("bot2"), Some(BotId("bot2")), isBot = true))
          case "user-token" => ZIO.succeed(AuthContext(UserId("user1"), None, isBot = false))
          case "director-token" => ZIO.succeed(AuthContext(UserId("director1"), None, isBot = false))
          case _ => ZIO.fail(new Exception("invalid token"))

  val allLayers: ZLayer[Any, Nothing, TournamentService & GameService & StreamService & AuthService & GameRepository & Scope] =
    InMemoryTournamentRepository.layer ++
    InMemoryGameRepository.layer ++
    StreamServiceLive.layer ++
    testAuthService ++
    Scope.default >+>
    TournamentServiceLive.layer >+>
    GameServiceLive.layer

  def authHeader(token: String): Headers =
    Headers(Header.Authorization.Bearer(token))

  def extractId(json: String): String =
    val marker = "\"id\":\""
    val idx = json.indexOf(marker) + marker.length
    json.substring(idx, json.indexOf("\"", idx))

  def createTournamentBody(
    name: String = "Test",
    nbRounds: Int = 2,
    clockLimit: Int = 300,
    clockIncrement: Int = 5,
  ): String =
    s"name=$name&nbRounds=$nbRounds&clockLimit=$clockLimit&clockIncrement=$clockIncrement"
