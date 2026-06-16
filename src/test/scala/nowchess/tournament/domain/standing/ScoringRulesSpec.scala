package nowchess.tournament.domain.standing

import zio.test.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.tournament.*
import nowchess.tournament.domain.round.*
import java.time.Instant

object ScoringRulesSpec extends ZIOSpecDefault:

  private val bot1 = BotRef(BotId("b1"), "Bot1")
  private val bot2 = BotRef(BotId("b2"), "Bot2")
  private val bot3 = BotRef(BotId("b3"), "Bot3")

  private val config = TournamentConfig(
    name = "Test", nbRounds = 2, clock = Clock(300, 0),
    rated = true, format = TournamentFormat.Swiss,
    startPosition = StartPosition.Standard, matchesPerPairing = 1,
  )

  private def mkTournament(participants: Vector[BotRef], rounds: Vector[Round]) =
    Tournament(
      id = TournamentId("t1"), config = config, status = TournamentStatus.Started,
      participants = participants, rounds = rounds, currentRound = rounds.size,
      director = UserId("d"), createdAt = Instant.now, startedAt = Some(Instant.now), winner = None,
    )

  private def mkPairing(white: BotRef, black: BotRef, outcome: Option[GameOutcome]) =
    Pairing(white, black,
      Vector(Match(GameId("g1"), white.id, outcome, None)),
      outcome,
    )

  def spec = suite("ScoringRules")(
    test("computes points for wins, draws, losses") {
      val rounds = Vector(
        Round(1, Vector(
          mkPairing(bot1, bot2, Some(GameOutcome.White)),
          mkPairing(bot3, BotRef(BotId("bye"), "Bye"), Some(GameOutcome.White)),
        )),
        Round(2, Vector(
          mkPairing(bot1, bot3, Some(GameOutcome.Draw)),
          mkPairing(bot2, BotRef(BotId("bye"), "Bye"), Some(GameOutcome.White)),
        )),
      )
      val t = mkTournament(Vector(bot1, bot2, bot3, BotRef(BotId("bye"), "Bye")), rounds)
      val results = ScoringRules.computeStandings(t)
      val r1 = results.find(_.bot.id == bot1.id).get
      val r2 = results.find(_.bot.id == bot2.id).get
      val r3 = results.find(_.bot.id == bot3.id).get
      assertTrue(r1.points == 1.5) &&
      assertTrue(r1.wins == 1) &&
      assertTrue(r1.draws == 1) &&
      assertTrue(r1.losses == 0) &&
      assertTrue(r2.points == 1.0) &&
      assertTrue(r2.wins == 1) &&
      assertTrue(r2.losses == 1) &&
      assertTrue(r3.points == 1.5)
    },
    test("ranks by points descending, then tiebreak") {
      val rounds = Vector(
        Round(1, Vector(mkPairing(bot1, bot2, Some(GameOutcome.White)))),
      )
      val t = mkTournament(Vector(bot1, bot2), rounds)
      val results = ScoringRules.computeStandings(t)
      assertTrue(results(0).bot.id == bot1.id) &&
      assertTrue(results(0).rank == 1) &&
      assertTrue(results(1).bot.id == bot2.id) &&
      assertTrue(results(1).rank == 2)
    },
    test("buchholz tiebreak sums opponent points") {
      val tb = ScoringRules.buchholzTieBreak(
        Vector(BotId("a"), BotId("b")),
        Map(BotId("a") -> 3.0, BotId("b") -> 2.5),
      )
      assertTrue(tb == 5.5)
    },
    test("buchholz handles unknown opponents") {
      val tb = ScoringRules.buchholzTieBreak(
        Vector(BotId("a"), BotId("unknown")),
        Map(BotId("a") -> 3.0),
      )
      assertTrue(tb == 3.0)
    },
    test("empty tournament has all participants at 0") {
      val t = mkTournament(Vector(bot1, bot2), Vector.empty)
      val results = ScoringRules.computeStandings(t)
      assertTrue(results.size == 2) &&
      assertTrue(results.forall(_.points == 0.0)) &&
      assertTrue(results.forall(_.nbGames == 0))
    },
    test("pairing with no outcome is skipped") {
      val rounds = Vector(Round(1, Vector(mkPairing(bot1, bot2, None))))
      val t = mkTournament(Vector(bot1, bot2), rounds)
      val results = ScoringRules.computeStandings(t)
      assertTrue(results.forall(_.points == 0.0))
    },
  )
