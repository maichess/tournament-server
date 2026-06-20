package tournament.http

import zio.test.*
import zio.http.*

object CorsSpec extends ZIOSpecDefault:

  private def origin(s: String): Header.Origin =
    Header.Origin.parse(s).toOption.get

  def spec = suite("Cors")(
    test("an empty allowlist reflects any origin back"):
      val cfg = Cors.config(Set.empty)
      val o   = origin("https://anything.example")
      assertTrue(cfg.allowedOrigin(o).contains(Header.AccessControlAllowOrigin.Specific(o)))
    ,
    test("a non-empty allowlist permits a listed origin"):
      val o   = origin("https://play.nowchess.org")
      val cfg = Cors.config(Set(o.renderedValue))
      assertTrue(cfg.allowedOrigin(o).contains(Header.AccessControlAllowOrigin.Specific(o)))
    ,
    test("a non-empty allowlist rejects an unlisted origin"):
      val cfg = Cors.config(Set("https://play.nowchess.org"))
      assertTrue(cfg.allowedOrigin(origin("https://evil.example")).isEmpty)
  )
