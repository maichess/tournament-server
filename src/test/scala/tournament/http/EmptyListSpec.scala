package tournament.http

import zio.*
import zio.test.*
import zio.http.*
import zio.json.*
import tournament.http.routes.TournamentRoutes
import tournament.http.codec.JsonCodecs.given
import tournament.http.RouteTestHelpers.*

object EmptyListSpec extends ZIOSpecDefault:

  def spec = suite("Empty list coverage")(
    test("list tournaments with no tournaments returns empty arrays for all statuses") {
      val routes = TournamentRoutes.routes
      for
        response <- routes.runZIO(Request.get(URL(Path.root / "api" / "tournament")))
        body <- response.body.asString
      yield assertTrue(
        response.status == Status.Ok,
        body.contains("\"created\":[]"),
        body.contains("\"started\":[]"),
        body.contains("\"finished\":[]"),
      )
    },
  ).provide(allLayers)
