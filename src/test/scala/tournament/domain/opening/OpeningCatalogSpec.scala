package tournament.domain.opening

import zio.test.*

object OpeningCatalogSpec extends ZIOSpecDefault:

  def spec = suite("OpeningCatalog")(
    test("looks up a named opening by key") {
      val vienna = OpeningCatalog.byKey("vienna")
      assertTrue(vienna.isDefined) &&
      assertTrue(vienna.get.name == "Vienna Opening") &&
      assertTrue(vienna.get.fen.nonEmpty)
    },
    test("unknown key resolves to None") {
      assertTrue(OpeningCatalog.byKey("not-a-real-opening").isEmpty)
    },
    test("the catalog includes the standard position") {
      assertTrue(OpeningCatalog.byKey("standard").exists(_.fen == OpeningCatalog.standardFen))
    },
    test("every catalog entry has a key, name and fen") {
      assertTrue(OpeningCatalog.all.nonEmpty) &&
      assertTrue(OpeningCatalog.all.forall(o => o.key.nonEmpty && o.name.nonEmpty && o.fen.nonEmpty))
    },
  )
