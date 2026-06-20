package nowchess.tournament.domain.lifecycle

import zio.test.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.tournament.*
import nowchess.tournament.domain.round.Round
import nowchess.tournament.domain.error.DomainError
import java.time.Instant

object TournamentLifecycleSpec extends ZIOSpecDefault:

  private val now = Instant.parse("2026-01-01T00:00:00Z")

  private val bot1 = BotRef(BotId("bot1"), "Bot One")
  private val bot2 = BotRef(BotId("bot2"), "Bot Two")
  private val bot3 = BotRef(BotId("bot3"), "Bot Three")

  private val config = TournamentConfig(
    name = "Test Tournament",
    nbRounds = 3,
    clock = Clock(300, 3),
    rated = true,
    format = TournamentFormat.Swiss,
    startPosition = StartPosition.Standard,
    matchesPerPairing = 1,
  )

  private def freshTournament = Tournament(
    id = TournamentId("t1"),
    config = config,
    status = TournamentStatus.Created,
    participants = Vector.empty,
    rounds = Vector.empty,
    currentRound = 0,
    director = UserId("director1"),
    createdAt = now,
    startedAt = None,
    winner = None,
  )

  private def startedTournament = freshTournament.copy(
    status = TournamentStatus.Started,
    participants = Vector(bot1, bot2),
    startedAt = Some(now),
    currentRound = 1,
  )

  def spec = suite("TournamentLifecycle")(
    suite("join")(
      test("succeeds when tournament is created") {
        val result = TournamentLifecycle.join(freshTournament, bot1)
        assertTrue(result.isRight) &&
        assertTrue(result.toOption.get.participants == Vector(bot1))
      },
      test("fails when tournament is started") {
        val result = TournamentLifecycle.join(startedTournament, bot3)
        assertTrue(result == Left(DomainError.Conflict("tournament already started")))
      },
      test("fails on double join") {
        val t = freshTournament.copy(participants = Vector(bot1))
        val result = TournamentLifecycle.join(t, bot1)
        assertTrue(result == Left(DomainError.Conflict("bot already joined")))
      },
    ),
    suite("withdraw")(
      test("succeeds when bot is participant") {
        val t = freshTournament.copy(participants = Vector(bot1, bot2))
        val result = TournamentLifecycle.withdraw(t, bot1.id)
        assertTrue(result.isRight) &&
        assertTrue(result.toOption.get.participants == Vector(bot2))
      },
      test("fails when tournament is started") {
        val result = TournamentLifecycle.withdraw(startedTournament, bot1.id)
        assertTrue(result == Left(DomainError.Conflict("tournament already started")))
      },
      test("fails when bot is not participant") {
        val result = TournamentLifecycle.withdraw(freshTournament, bot1.id)
        assertTrue(result == Left(DomainError.Conflict("bot not in tournament")))
      },
    ),
    suite("start")(
      test("succeeds with 2+ participants") {
        val t = freshTournament.copy(participants = Vector(bot1, bot2))
        val result = TournamentLifecycle.start(t, now)
        assertTrue(result.isRight) &&
        assertTrue(result.toOption.get.status == TournamentStatus.Started) &&
        assertTrue(result.toOption.get.currentRound == 1) &&
        assertTrue(result.toOption.get.startedAt.contains(now))
      },
      test("fails with less than 2 participants") {
        val t = freshTournament.copy(participants = Vector(bot1))
        val result = TournamentLifecycle.start(t, now)
        assertTrue(result == Left(DomainError.Conflict("need at least 2 participants")))
      },
      test("fails when already started") {
        val result = TournamentLifecycle.start(startedTournament, now)
        assertTrue(result == Left(DomainError.Conflict("tournament already started")))
      },
    ),
    suite("addRound")(
      test("succeeds when started") {
        val round = Round(1, Vector.empty)
        val result = TournamentLifecycle.addRound(startedTournament, round)
        assertTrue(result.isRight) &&
        assertTrue(result.toOption.get.rounds.size == 1)
      },
      test("fails when not started") {
        val round = Round(1, Vector.empty)
        val result = TournamentLifecycle.addRound(freshTournament, round)
        assertTrue(result == Left(DomainError.Conflict("tournament not started")))
      },
    ),
    suite("advanceRound")(
      test("succeeds when not at final round") {
        val result = TournamentLifecycle.advanceRound(startedTournament)
        assertTrue(result.isRight) &&
        assertTrue(result.toOption.get.currentRound == 2)
      },
      test("fails at final round") {
        val t = startedTournament.copy(currentRound = 3)
        val result = TournamentLifecycle.advanceRound(t)
        assertTrue(result == Left(DomainError.Conflict("already at final round")))
      },
      test("fails when not started") {
        val result = TournamentLifecycle.advanceRound(freshTournament)
        assertTrue(result == Left(DomainError.Conflict("tournament not started")))
      },
    ),
    suite("updateRound")(
      test("succeeds when round exists") {
        val round = Round(1, Vector.empty)
        val t = startedTournament.copy(rounds = Vector(round))
        val updated = Round(1, Vector.empty)
        val result = TournamentLifecycle.updateRound(t, 1, updated)
        assertTrue(result.isRight)
      },
      test("fails when round not found") {
        val result = TournamentLifecycle.updateRound(startedTournament, 5, Round(5, Vector.empty))
        assertTrue(result == Left(DomainError.NotFound("round 5 not found")))
      },
      test("fails when not started") {
        val result = TournamentLifecycle.updateRound(freshTournament, 1, Round(1, Vector.empty))
        assertTrue(result == Left(DomainError.Conflict("tournament not started")))
      },
    ),
    suite("finish")(
      test("succeeds when started") {
        val result = TournamentLifecycle.finish(startedTournament, bot1, now)
        assertTrue(result.isRight) &&
        assertTrue(result.toOption.get.status == TournamentStatus.Finished) &&
        assertTrue(result.toOption.get.winner.contains(bot1)) &&
        assertTrue(result.toOption.get.finishedAt.contains(now))
      },
      test("fails when not started") {
        val result = TournamentLifecycle.finish(freshTournament, bot1, now)
        assertTrue(result == Left(DomainError.Conflict("tournament not started")))
      },
    ),
    suite("terminate")(
      test("succeeds when created") {
        val result = TournamentLifecycle.terminate(freshTournament)
        assertTrue(result.isRight) &&
        assertTrue(result.toOption.get.status == TournamentStatus.Finished)
      },
      test("fails when started") {
        val result = TournamentLifecycle.terminate(startedTournament)
        assertTrue(result == Left(DomainError.Conflict("can only terminate before start")))
      },
    ),
  )
