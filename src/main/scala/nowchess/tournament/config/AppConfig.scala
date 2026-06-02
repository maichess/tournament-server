package nowchess.tournament.config

final case class AppConfig(
  port: Int = 8080,
  jwtSecret: String = "changeme",
)

object AppConfig:
  def fromEnv(env: Map[String, String]): AppConfig =
    AppConfig(
      port = env.get("PORT").flatMap(_.toIntOption).getOrElse(8080),
      jwtSecret = env.getOrElse("TOURNAMENT_JWT_SECRET", "changeme"),
    )
