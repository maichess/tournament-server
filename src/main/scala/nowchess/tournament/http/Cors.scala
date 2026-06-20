package nowchess.tournament.http

import zio.http.*

/** Builds the CORS policy from configuration. With an explicit allowlist only
  * those origins are echoed back; with an empty allowlist any origin is
  * reflected (a permissive default for local development). The server uses
  * bearer-token auth and sets no credentialed cookies, so reflecting an origin
  * never exposes authenticated data. */
object Cors:

  def config(allowedOrigins: Set[String]): Middleware.CorsConfig =
    Middleware.CorsConfig(
      allowedOrigin = origin =>
        if isAllowed(origin, allowedOrigins) then Some(Header.AccessControlAllowOrigin.Specific(origin))
        else None,
      allowedMethods = Header.AccessControlAllowMethods.All,
      allowedHeaders = Header.AccessControlAllowHeaders.All,
    )

  private def isAllowed(origin: Header.Origin, allowedOrigins: Set[String]): Boolean =
    allowedOrigins.isEmpty || allowedOrigins.contains(origin.renderedValue)
