package tournament.service

import zio.*
import zio.test.*
import tournament.domain.model.BotId
import tournament.domain.error.DomainError
import tournament.persistence.{InMemoryBotRegistryRepository, InMemoryIdentityRepository}

object BotRegistryServiceSpec extends ZIOSpecDefault:

  private val authLayer = InMemoryIdentityRepository.layer >>> JwtAuthService.layer("test-secret")
  private val layer = (InMemoryBotRegistryRepository.layer ++ authLayer) >>> BotRegistryServiceLive.layer

  def spec = suite("BotRegistryService")(
    test("register stores a bot with a generated id and trimmed name") {
      for
        svc <- ZIO.service[BotRegistryService]
        bot <- svc.register("  Fish  ", Some("http://bot"))
        fetched <- svc.get(bot.id)
      yield assertTrue(
        bot.name == "Fish",
        bot.endpoint.contains("http://bot"),
        fetched.contains(bot),
      )
    },
    test("register returns an auth-backed id and is idempotent by name") {
      for
        svc <- ZIO.service[BotRegistryService]
        a <- svc.register("Repeat", None)
        b <- svc.register("Repeat", None)
      yield assertTrue(a.id.value.startsWith("bot_"), a.id == b.id)
    },
    test("register treats a blank endpoint as absent") {
      for
        svc <- ZIO.service[BotRegistryService]
        bot <- svc.register("NoEndpoint", Some("   "))
      yield assertTrue(bot.endpoint.isEmpty)
    },
    test("register allows an omitted endpoint") {
      for
        svc <- ZIO.service[BotRegistryService]
        bot <- svc.register("Bare", None)
      yield assertTrue(bot.endpoint.isEmpty)
    },
    test("register rejects a blank name") {
      for
        svc <- ZIO.service[BotRegistryService]
        e <- svc.register("  ", None).flip
      yield assertTrue(e.isInstanceOf[DomainError.BadRequest])
    },
    test("list returns all registered bots") {
      for
        svc <- ZIO.service[BotRegistryService]
        _ <- svc.register("A", None)
        _ <- svc.register("B", None)
        all <- svc.list
      yield assertTrue(all.size >= 2)
    },
    test("get returns None for an unknown id") {
      for
        svc <- ZIO.service[BotRegistryService]
        found <- svc.get(BotId("nope"))
      yield assertTrue(found.isEmpty)
    },
    test("get is reachable through the service accessor") {
      for found <- BotRegistryService.get(BotId("still-nope"))
      yield assertTrue(found.isEmpty)
    },
    test("delete removes a registered bot") {
      for
        svc <- ZIO.service[BotRegistryService]
        bot <- svc.register("Doomed", None)
        _ <- svc.delete(bot.id)
        found <- svc.get(bot.id)
      yield assertTrue(found.isEmpty)
    },
    test("register stores and trims all engine metadata when provided") {
      for
        svc <- ZIO.service[BotRegistryService]
        bot <- svc.register(
          "FullMeta", Some("http://x"),
          family = Some(" fam "), strategyType = Some(" strat "),
          engineType = Some(" eng "), modelVersion = Some(" v1 "),
        )
      yield assertTrue(
        bot.family.contains("fam"),
        bot.strategyType.contains("strat"),
        bot.engineType.contains("eng"),
        bot.modelVersion.contains("v1"),
      )
    },
    test("register and get are reachable through the service accessors") {
      for
        bot     <- BotRegistryService.register("ViaCompanion", None)
        fetched <- BotRegistryService.get(bot.id)
      yield assertTrue(fetched.contains(bot))
    },
    test("list and delete are reachable through the service accessors") {
      for
        bot  <- BotRegistryService.register("CompanionLifecycle", None)
        all  <- BotRegistryService.list
        _    <- BotRegistryService.delete(bot.id)
        gone <- BotRegistryService.get(bot.id)
      yield assertTrue(all.exists(_.id == bot.id), gone.isEmpty)
    },
    test("re-registering the same name preserves existing metadata") {
      for
        svc  <- ZIO.service[BotRegistryService]
        orig <- svc.register("Stable", None, engineType = Some("heuristic"))
        again <- svc.register("Stable", None)
      yield assertTrue(
        orig.id == again.id,
        again.engineType.contains("heuristic"),
      )
    },
  ).provide(layer)
