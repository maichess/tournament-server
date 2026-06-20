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
          case "bot1-token" => ZIO.succeed(AuthContext(UserId("bot1"), Some(BotId("bot1")), isBot = true, name = "TestBot1"))
          case "bot2-token" => ZIO.succeed(AuthContext(UserId("bot2"), Some(BotId("bot2")), isBot = true, name = "TestBot2"))
          case "user-token" => ZIO.succeed(AuthContext(UserId("user1"), None, isBot = false, name = "user1"))
          case "director-token" => ZIO.succeed(AuthContext(UserId("director1"), None, isBot = false, name = "director1"))
          case _ => ZIO.fail(new Exception("invalid token"))

      override def register(name: String, isBot: Boolean): Task[RegisterResult] =
        val id = if isBot then "bot_" + name.trim.toLowerCase.replaceAll("[^a-z0-9]+", "_") else "usr_test"
        ZIO.succeed(RegisterResult(id, "test-jwt-token"))

  val allLayers: ZLayer[Any, Nothing, TournamentService & GameService & StreamService & AuthService & GameRepository & BotRegistryService & Scope] =
    InMemoryTournamentRepository.layer ++
    InMemoryGameRepository.layer ++
    StreamServiceLive.layer ++
    testAuthService ++
    (InMemoryOpeningRepository.layer >>> OpeningServiceLive.layer) ++
    ((InMemoryBotRegistryRepository.layer ++ testAuthService) >>> BotRegistryServiceLive.layer) ++
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
