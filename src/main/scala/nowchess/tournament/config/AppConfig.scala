package nowchess.tournament.config

final case class AppConfig(
  port: Int = 8080,
  jwtSecret: String = "changeme",
)
