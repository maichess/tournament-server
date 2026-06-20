package tournament.config

final case class AppConfig(
  port: Int = 8080,
  jwtSecret: String = "changeme",
  // Exact CORS origins to allow (e.g. "https://play.nowchess.org"). Empty means
  // "reflect any origin", a permissive dev default; set ALLOWED_ORIGINS in prod.
  allowedOrigins: Set[String] = Set.empty,
)

object AppConfig:
  def fromEnv(env: Map[String, String]): AppConfig =
    AppConfig(
      port = env.get("PORT").flatMap(_.toIntOption).getOrElse(8080),
      jwtSecret = env.getOrElse("TOURNAMENT_JWT_SECRET", "changeme"),
      allowedOrigins = parseOrigins(env.getOrElse("ALLOWED_ORIGINS", "")),
    )

  private def parseOrigins(raw: String): Set[String] =
    raw.split(",").iterator.map(_.trim).filter(_.nonEmpty).toSet
