package nowchess.tournament.service

import zio.*
import zio.test.*
import nowchess.tournament.domain.model.BotId
import nowchess.tournament.domain.error.DomainError
import nowchess.tournament.persistence.InMemoryBotRegistryRepository

object BotRegistryServiceSpec extends ZIOSpecDefault:

  private val layer = InMemoryBotRegistryRepository.layer >>> BotRegistryServiceLive.layer

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
  ).provide(layer)
