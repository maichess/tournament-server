package nowchess.tournament.service

import zio.*
import zio.test.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.tournament.*
import nowchess.tournament.domain.error.DomainError
import nowchess.tournament.persistence.{InMemoryTournamentRepository, InMemoryGameRepository}

object TournamentServiceSpec extends ZIOSpecDefault:

  private val bot1 = BotRef(BotId("b1"), "Bot1")
  private val bot2 = BotRef(BotId("b2"), "Bot2")
  private val director = UserId("director")

  private val form = CreateTournamentForm(
    name = "Test", nbRounds = 2, clockLimit = 300, clockIncrement = 3,
    rated = true, format = TournamentFormat.Swiss,
    startPosition = StartPosition.Standard, matchesPerPairing = 1, groupSize = None,
  )

  private val testLayer =
    InMemoryTournamentRepository.layer ++
    InMemoryGameRepository.layer ++
    StreamServiceLive.layer >>>
    TournamentServiceLive.layer

  def spec = suite("TournamentService")(
    test("create tournament") {
      (for
        svc <- ZIO.service[TournamentService]
        t <- svc.create(form, director)
      yield
        assertTrue(t.config.name == "Test") &&
        assertTrue(t.status == TournamentStatus.Created) &&
        assertTrue(t.director == director)
      ).provide(testLayer)
    },
    test("get existing tournament") {
      (for
        svc <- ZIO.service[TournamentService]
        t <- svc.create(form, director)
        fetched <- svc.get(t.id)
      yield assertTrue(fetched.id == t.id)
      ).provide(testLayer)
    },
    test("get non-existent tournament fails") {
      (for
        svc <- ZIO.service[TournamentService]
        result <- svc.get(TournamentId("nonexistent")).exit
      yield assertTrue(result.isFailure)
      ).provide(testLayer)
    },
    test("list tournaments by status") {
      (for
        svc <- ZIO.service[TournamentService]
        _ <- svc.create(form, director)
        _ <- svc.create(form, director)
        listed <- svc.list
      yield assertTrue(listed.getOrElse(TournamentStatus.Created, Vector.empty).size == 2)
      ).provide(testLayer)
    },
    test("join tournament") {
      (for
        svc <- ZIO.service[TournamentService]
        t <- svc.create(form, director)
        _ <- svc.join(t.id, bot1)
        updated <- svc.get(t.id)
      yield assertTrue(updated.participants.size == 1) &&
            assertTrue(updated.participants.head.id == bot1.id)
      ).provide(testLayer)
    },
    test("double join fails") {
      (for
        svc <- ZIO.service[TournamentService]
        t <- svc.create(form, director)
        _ <- svc.join(t.id, bot1)
        result <- svc.join(t.id, bot1).exit
      yield assertTrue(result.isFailure)
      ).provide(testLayer)
    },
    test("withdraw from tournament") {
      (for
        svc <- ZIO.service[TournamentService]
        t <- svc.create(form, director)
        _ <- svc.join(t.id, bot1)
        _ <- svc.withdraw(t.id, bot1.id)
        updated <- svc.get(t.id)
      yield assertTrue(updated.participants.isEmpty)
      ).provide(testLayer)
    },
    test("start tournament with 2 bots") {
      (for
        svc <- ZIO.service[TournamentService]
        t <- svc.create(form, director)
        _ <- svc.join(t.id, bot1)
        _ <- svc.join(t.id, bot2)
        started <- svc.start(t.id, director)
      yield
        assertTrue(started.status == TournamentStatus.Started) &&
        assertTrue(started.currentRound == 1) &&
        assertTrue(started.rounds.size == 1) &&
        assertTrue(started.rounds.head.pairings.size == 1)
      ).provide(testLayer)
    },
    test("start fails with less than 2 bots") {
      (for
        svc <- ZIO.service[TournamentService]
        t <- svc.create(form, director)
        _ <- svc.join(t.id, bot1)
        result <- svc.start(t.id, director).exit
      yield assertTrue(result.isFailure)
      ).provide(testLayer)
    },
    test("start fails if not director") {
      (for
        svc <- ZIO.service[TournamentService]
        t <- svc.create(form, director)
        _ <- svc.join(t.id, bot1)
        _ <- svc.join(t.id, bot2)
        result <- svc.start(t.id, UserId("other")).exit
      yield assertTrue(result.isFailure)
      ).provide(testLayer)
    },
    test("delete tournament") {
      (for
        svc <- ZIO.service[TournamentService]
        t <- svc.create(form, director)
        _ <- svc.delete(t.id, director)
        updated <- svc.get(t.id)
      yield assertTrue(updated.status == TournamentStatus.Finished)
      ).provide(testLayer)
    },
    test("delete fails if not director") {
      (for
        svc <- ZIO.service[TournamentService]
        t <- svc.create(form, director)
        result <- svc.delete(t.id, UserId("other")).exit
      yield assertTrue(result.isFailure)
      ).provide(testLayer)
    },
    test("delete fails if already started") {
      (for
        svc <- ZIO.service[TournamentService]
        t <- svc.create(form, director)
        _ <- svc.join(t.id, bot1)
        _ <- svc.join(t.id, bot2)
        _ <- svc.start(t.id, director)
        result <- svc.delete(t.id, director).exit
      yield assertTrue(result.isFailure)
      ).provide(testLayer)
    },
    test("start with elimination format") {
      val elimForm = form.copy(format = TournamentFormat.SingleElimination)
      (for
        svc <- ZIO.service[TournamentService]
        t <- svc.create(elimForm, director)
        _ <- svc.join(t.id, bot1)
        _ <- svc.join(t.id, bot2)
        started <- svc.start(t.id, director)
      yield assertTrue(started.rounds.head.pairings.size == 1)
      ).provide(testLayer)
    },
    test("start with league format") {
      val leagueForm = form.copy(format = TournamentFormat.League)
      (for
        svc <- ZIO.service[TournamentService]
        t <- svc.create(leagueForm, director)
        _ <- svc.join(t.id, bot1)
        _ <- svc.join(t.id, bot2)
        started <- svc.start(t.id, director)
      yield assertTrue(started.rounds.head.pairings.size == 1)
      ).provide(testLayer)
    },
    test("start with best-of-3") {
      val bo3Form = form.copy(matchesPerPairing = 3)
      (for
        svc <- ZIO.service[TournamentService]
        t <- svc.create(bo3Form, director)
        _ <- svc.join(t.id, bot1)
        _ <- svc.join(t.id, bot2)
        started <- svc.start(t.id, director)
      yield
        assertTrue(started.rounds.head.pairings.head.matches.size == 3)
      ).provide(testLayer)
    },
    test("start with custom FEN") {
      val fenForm = form.copy(startPosition = StartPosition.FromFen("8/8/8/8/8/8/8/4K2k w - - 0 1"))
      (for
        svc <- ZIO.service[TournamentService]
        t <- svc.create(fenForm, director)
        _ <- svc.join(t.id, bot1)
        _ <- svc.join(t.id, bot2)
        started <- svc.start(t.id, director)
      yield assertTrue(started.config.startPosition == StartPosition.FromFen("8/8/8/8/8/8/8/4K2k w - - 0 1"))
      ).provide(testLayer)
    },
  )
