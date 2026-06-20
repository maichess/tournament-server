package tournament.domain.standing

import zio.test.*
import tournament.domain.model.*
import tournament.domain.tournament.*
import tournament.domain.round.*
import java.time.Instant

object ScoringRulesCoverageSpec extends ZIOSpecDefault:

  private val bot1 = BotRef(BotId("b1"), "Bot1")
  private val bot2 = BotRef(BotId("b2"), "Bot2")
  private val bot3 = BotRef(BotId("b3"), "Bot3")

  private def makeTournament(
    participants: Vector[BotRef],
    rounds: Vector[Round],
  ): Tournament =
    Tournament(
      id = TournamentId("t1"),
      config = TournamentConfig("Test", 1, Clock(300, 3), rated = true,
        TournamentFormat.Swiss, StartPosition.Standard, 1),
      status = TournamentStatus.Started,
      participants = participants,
      rounds = rounds,
      currentRound = 1,
      director = UserId("d"),
      createdAt = Instant.now(),
      startedAt = Some(Instant.now()),
      winner = None,
    )

  def spec = suite("ScoringRules coverage")(
    test("unknown bot fallback in computeStandings") {
      // Pairing references a bot NOT in participants list → BotRef(botId, "unknown") fallback
      val ghostBot = BotRef(BotId("ghost"), "Ghost")
      val round = Round(1, Vector(
        Pairing(ghostBot, bot1, Vector(Match(GameId("g1"), ghostBot.id, Some(GameOutcome.White), None)), Some(GameOutcome.White))
      ))
      // Only bot1 is a participant, ghostBot is not
      val tournament = makeTournament(Vector(bot1), Vector(round))
      val standings = ScoringRules.computeStandings(tournament)
      // ghost bot should appear with name "unknown"
      val ghostResult = standings.find(_.bot.id == BotId("ghost"))
      assertTrue(ghostResult.isDefined) &&
      assertTrue(ghostResult.get.bot.name == "unknown")
    },
    test("getOrElse BotStats fallback for bots not in initial map") {
      // Pairing references bots not in participants. The initial map only has participants.
      // So getOrElse(whiteId, BotStats(...)) triggers for bots not in participants.
      val round = Round(1, Vector(
        Pairing(bot1, bot2, Vector(Match(GameId("g1"), bot1.id, Some(GameOutcome.Black), None)), Some(GameOutcome.Black))
      ))
      // Neither bot is in participants
      val tournament = makeTournament(Vector.empty, Vector(round))
      val standings = ScoringRules.computeStandings(tournament)
      assertTrue(standings.length == 2) &&
      assertTrue(standings.exists(r => r.bot.id == BotId("b2") && r.wins == 1))
    },
    test("draw outcome in pairing") {
      val round = Round(1, Vector(
        Pairing(bot1, bot2, Vector(Match(GameId("g1"), bot1.id, Some(GameOutcome.Draw), None)), Some(GameOutcome.Draw))
      ))
      val tournament = makeTournament(Vector(bot1, bot2), Vector(round))
      val standings = ScoringRules.computeStandings(tournament)
      val b1 = standings.find(_.bot.id == BotId("b1")).get
      val b2 = standings.find(_.bot.id == BotId("b2")).get
      assertTrue(b1.draws == 1, b2.draws == 1, b1.points == 0.5, b2.points == 0.5)
    },
    test("pairing with no outcome is skipped") {
      val round = Round(1, Vector(
        Pairing(bot1, bot2, Vector(Match(GameId("g1"), bot1.id, None, None)), None)
      ))
      val tournament = makeTournament(Vector(bot1, bot2), Vector(round))
      val standings = ScoringRules.computeStandings(tournament)
      assertTrue(standings.forall(_.nbGames == 0))
    },
  )
