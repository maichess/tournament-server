package tournament.service

import zio.*
import zio.test.*
import tournament.domain.model.*
import tournament.domain.event.*
import tournament.domain.game.{GameStatus, GameClock}

object StreamServiceCompanionSpec extends ZIOSpecDefault:

  def spec = suite("StreamService companion")(
    test("publishTournament via companion") {
      for
        _ <- StreamService.subscribeTournament(TournamentId("t1"))
        _ <- StreamService.publishTournament(TournamentId("t1"), TournamentEvent.TournamentStarted)
      yield assertTrue(true)
    },
    test("publishGame via companion") {
      for
        _ <- StreamService.subscribeGame(GameId("g1"))
        _ <- StreamService.publishGame(GameId("g1"), GameEvent.GameEnd(None, GameStatus.Stalemate))
      yield assertTrue(true)
    },
  ).provide(StreamServiceLive.layer)
