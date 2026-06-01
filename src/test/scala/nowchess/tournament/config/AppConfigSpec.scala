package nowchess.tournament.config

import zio.test.*

object AppConfigSpec extends ZIOSpecDefault:
  def spec = suite("AppConfig")(
    test("default values"):
      val config = AppConfig()
      assertTrue(
        config.port == 8080,
        config.jwtSecret == "changeme",
      )
    ,
    test("custom values"):
      val config = AppConfig(port = 9090, jwtSecret = "mysecret")
      assertTrue(
        config.port == 9090,
        config.jwtSecret == "mysecret",
      )
    ,
  )
