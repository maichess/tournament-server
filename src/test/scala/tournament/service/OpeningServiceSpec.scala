package tournament.service

import zio.*
import zio.test.*
import tournament.domain.opening.Opening
import tournament.domain.error.DomainError
import tournament.persistence.{InMemoryOpeningRepository, OpeningRepository}

object OpeningServiceSpec extends ZIOSpecDefault:

  private val layer = InMemoryOpeningRepository.layer >+> OpeningServiceLive.layer

  def spec = suite("OpeningService")(
    test("slug derives a lowerCamelCase key from a name") {
      assertTrue(OpeningService.slug("My Custom Opening") == "myCustomOpening")
    },
    test("register derives a key from the name") {
      for
        svc <- ZIO.service[OpeningService]
        o <- svc.register("Wild Gambit", "fen-string", None)
      yield assertTrue(o == Opening("wildGambit", "Wild Gambit", "fen-string"))
    },
    test("register honours an explicit key") {
      for
        svc <- ZIO.service[OpeningService]
        o <- svc.register("Something", "fen", Some("myKey"))
      yield assertTrue(o.key == "myKey")
    },
    test("list merges catalog and custom entries") {
      for
        svc <- ZIO.service[OpeningService]
        _ <- svc.register("Custom One", "fen-1", None)
        all <- svc.list
      yield assertTrue(
        all.exists(_.key == "vienna"),
        all.exists(_.key == "customOne"),
      )
    },
    test("list does not let a custom entry shadow a catalog key") {
      for
        repo <- ZIO.service[OpeningRepository]
        svc <- ZIO.service[OpeningService]
        _ <- repo.save(Opening("standard", "Hijacked", "evil-fen"))
        all <- svc.list
        standards = all.filter(_.key == "standard")
      yield assertTrue(
        standards.size == 1,
        standards.head.name == "Standard",
      )
    },
    test("resolve finds a catalog opening") {
      for
        svc <- ZIO.service[OpeningService]
        fen <- svc.resolve("french")
      yield assertTrue(fen.isDefined)
    },
    test("resolve finds a custom opening") {
      for
        svc <- ZIO.service[OpeningService]
        _ <- svc.register("Resolvable", "custom-fen", Some("resolvable"))
        fen <- svc.resolve("resolvable")
      yield assertTrue(fen.contains("custom-fen"))
    },
    test("resolve returns None for an unknown key") {
      for
        svc <- ZIO.service[OpeningService]
        fen <- svc.resolve("ghost")
      yield assertTrue(fen.isEmpty)
    },
    test("resolve is reachable through the service accessor") {
      for fen <- OpeningService.resolve("vienna")
      yield assertTrue(fen.isDefined)
    },
    test("register rejects a blank name") {
      for
        svc <- ZIO.service[OpeningService]
        e <- svc.register("   ", "fen", None).flip
      yield assertTrue(e.isInstanceOf[DomainError.BadRequest])
    },
    test("register rejects a blank fen") {
      for
        svc <- ZIO.service[OpeningService]
        e <- svc.register("Named", "   ", None).flip
      yield assertTrue(e.isInstanceOf[DomainError.BadRequest])
    },
    test("register rejects a name that yields an empty key") {
      for
        svc <- ZIO.service[OpeningService]
        e <- svc.register("!!!", "fen", None).flip
      yield assertTrue(e.isInstanceOf[DomainError.BadRequest])
    },
    test("register rejects a reserved catalog key") {
      for
        svc <- ZIO.service[OpeningService]
        e <- svc.register("Vienna", "fen", Some("vienna")).flip
      yield assertTrue(e.isInstanceOf[DomainError.Conflict])
    },
    test("register rejects a duplicate custom key") {
      for
        svc <- ZIO.service[OpeningService]
        _ <- svc.register("First", "fen", Some("dup"))
        e <- svc.register("Second", "fen", Some("dup")).flip
      yield assertTrue(e.isInstanceOf[DomainError.Conflict])
    },
  ).provide(layer)
