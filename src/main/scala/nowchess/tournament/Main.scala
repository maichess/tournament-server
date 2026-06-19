package nowchess.tournament

import zio.*
import zio.http.*
import nowchess.tournament.config.AppConfig
import nowchess.tournament.http.routes.*
import nowchess.tournament.persistence.{InMemoryTournamentRepository, InMemoryGameRepository, InMemoryIdentityRepository, InMemoryOpeningRepository, InMemoryBotRegistryRepository}
import nowchess.tournament.service.*

object Main extends ZIOAppDefault:

  private val config = AppConfig.fromEnv(sys.env)

  private val corsConfig = Middleware.CorsConfig(
    allowedOrigin = origin => Some(Header.AccessControlAllowOrigin.Specific(origin)),
    allowedMethods = Header.AccessControlAllowMethods.All,
    allowedHeaders = Header.AccessControlAllowHeaders.All
  )

  private val allRoutes =
    (AuthRoutes.routes ++
    TournamentRoutes.routes ++
    ParticipationRoutes.routes ++
    ResultRoutes.routes ++
    StreamRoutes.routes ++
    GameRoutes.routes ++
    OpeningRoutes.routes ++
    BotRegistryRoutes.routes) @@ Middleware.cors(corsConfig)

  override val run: ZIO[Any, Throwable, Nothing] =
    Server.serve(allRoutes).provide(
      Server.defaultWithPort(config.port),
      InMemoryTournamentRepository.layer,
      InMemoryGameRepository.layer,
      InMemoryIdentityRepository.layer,
      InMemoryOpeningRepository.layer,
      InMemoryBotRegistryRepository.layer,
      OpeningServiceLive.layer,
      BotRegistryServiceLive.layer,
      TournamentServiceLive.layer,
      GameServiceLive.layer,
      StreamServiceLive.layer,
      JwtAuthService.layer(config.jwtSecret),
    )
