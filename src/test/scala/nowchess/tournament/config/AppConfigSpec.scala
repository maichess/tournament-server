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
    test("fromEnv with empty map falls back to defaults"):
      val config = AppConfig.fromEnv(Map.empty)
      assertTrue(
        config.port == 8080,
        config.jwtSecret == "changeme",
      )
    ,
    test("fromEnv reads PORT and TOURNAMENT_JWT_SECRET"):
      val config = AppConfig.fromEnv(Map(
        "PORT" -> "9090",
        "TOURNAMENT_JWT_SECRET" -> "from-env",
      ))
      assertTrue(
        config.port == 9090,
        config.jwtSecret == "from-env",
      )
    ,
    test("fromEnv ignores non-numeric PORT"):
      val config = AppConfig.fromEnv(Map("PORT" -> "not-a-port"))
      assertTrue(config.port == 8080)
    ,
    test("default config has an empty CORS allowlist"):
      assertTrue(AppConfig().allowedOrigins.isEmpty)
    ,
    test("fromEnv with no ALLOWED_ORIGINS yields an empty allowlist"):
      assertTrue(AppConfig.fromEnv(Map.empty).allowedOrigins.isEmpty)
    ,
    test("fromEnv parses a comma-separated ALLOWED_ORIGINS, trimming blanks"):
      val config = AppConfig.fromEnv(Map("ALLOWED_ORIGINS" -> " https://a.com , https://b.com ,, "))
      assertTrue(config.allowedOrigins == Set("https://a.com", "https://b.com"))
    ,
  )
