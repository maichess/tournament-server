package nowchess.tournament.service

import zio.*
import zio.test.*
import zio.stream.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.event.*

object StreamServiceSpec extends ZIOSpecDefault:

  private val tid = TournamentId("t1")
  private val gid = GameId("g1")

  def spec = suite("StreamService")(
    test("subscribe and receive tournament events") {
      (for
        svc <- ZIO.service[StreamService]
        stream <- svc.subscribeTournament(tid)
        fiber <- stream.take(2).runCollect.fork
        _ <- svc.publishTournament(tid, TournamentEvent.TournamentStarted)
        _ <- svc.publishTournament(tid, TournamentEvent.RoundStarted(1))
        events <- fiber.join
      yield
        assertTrue(events.size == 2) &&
        assertTrue(events(0) == TournamentEvent.TournamentStarted) &&
        assertTrue(events(1) == TournamentEvent.RoundStarted(1))
      ).provide(StreamServiceLive.layer)
    },
    test("subscribe and receive game events") {
      (for
        svc <- ZIO.service[StreamService]
        stream <- svc.subscribeGame(gid)
        fiber <- stream.take(1).runCollect.fork
        _ <- svc.publishGame(gid, GameEvent.GameEnd(Some(Color.White), nowchess.tournament.domain.game.GameStatus.Checkmate))
        events <- fiber.join
      yield assertTrue(events.size == 1)
      ).provide(StreamServiceLive.layer)
    },
    test("multiple subscribers receive same events") {
      (for
        svc <- ZIO.service[StreamService]
        s1 <- svc.subscribeTournament(tid)
        s2 <- svc.subscribeTournament(tid)
        f1 <- s1.take(1).runCollect.fork
        f2 <- s2.take(1).runCollect.fork
        _ <- svc.publishTournament(tid, TournamentEvent.TournamentStarted)
        e1 <- f1.join
        e2 <- f2.join
      yield
        assertTrue(e1.size == 1) &&
        assertTrue(e2.size == 1)
      ).provide(StreamServiceLive.layer)
    },
  )
