package nowchess.tournament.service

import zio.*
import zio.test.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.event.*
import nowchess.tournament.domain.game.{GameStatus, GameClock}

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
