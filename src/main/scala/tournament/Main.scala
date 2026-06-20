package tournament

import zio.*
import zio.http.*
import tournament.config.AppConfig
import tournament.http.Cors
import tournament.http.routes.*
import tournament.persistence.{InMemoryTournamentRepository, InMemoryGameRepository, InMemoryIdentityRepository, InMemoryOpeningRepository, InMemoryBotRegistryRepository}
import tournament.service.*

object Main extends ZIOAppDefault:

  private val config = AppConfig.fromEnv(sys.env)

  private val corsConfig = Cors.config(config.allowedOrigins)

  private val allRoutes =
    (AuthRoutes.routes ++
    TournamentRoutes.routes ++
    ParticipationRoutes.routes ++
    ResultRoutes.routes ++
    StreamRoutes.routes ++
    GameRoutes.routes ++
    OpeningRoutes.routes ++
    BotRegistryRoutes.routes ++
    AnalyticsExportRoutes.routes) @@ Middleware.cors(corsConfig)

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
