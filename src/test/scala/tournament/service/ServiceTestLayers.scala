package tournament.service

import zio.*
import tournament.persistence.{InMemoryBotRegistryRepository, InMemoryIdentityRepository}

/** Shared test wiring for the bot registry, which is now backed by an auth
  * identity service (registry ids share the JWT subject space). */
object ServiceTestLayers:
  val botRegistry: ULayer[BotRegistryService] =
    (InMemoryBotRegistryRepository.layer ++
      (InMemoryIdentityRepository.layer >>> JwtAuthService.layer("test-secret"))) >>>
      BotRegistryServiceLive.layer
