package tournament.service

import zio.test.*
import tournament.domain.model.*
import tournament.domain.game.{Game, GameClock, GameStatus}
import tournament.domain.round.{Round, Pairing, Match}
import tournament.domain.tournament.*
import tournament.domain.event.TournamentEvent
import java.time.Instant

object GameActivationSpec extends ZIOSpecDefault:

  private val bot1 = BotRef(BotId("b1"), "Bot1")
  private val bot2 = BotRef(BotId("b2"), "Bot2")

  private def tournament(currentRound: Int, rounds: Vector[Round]) = Tournament(
    id = TournamentId("t"),
    config = TournamentConfig("T", 1, Clock(300, 3), rated = true, TournamentFormat.Swiss, StartPosition.Standard, 1),
    status = TournamentStatus.Started,
    participants = Vector(bot1, bot2),
    rounds = rounds,
    currentRound = currentRound,
    director = UserId("d"),
    createdAt = Instant.EPOCH,
    startedAt = Some(Instant.EPOCH),
    winner = None,
  )

  private def game(id: String, status: GameStatus) = Game(
    id = GameId(id), tournamentId = TournamentId("t"), round = 1,
    white = bot1, black = bot2, moves = Vector.empty, status = status,
    turn = Color.White, winner = None, clock = GameClock(300, 300),
    startPosition = StartPosition.Standard, fen = "fen",
  )

  def spec = suite("GameActivation")(
    test("gameStartEvents announces white then black for one game"):
      assertTrue(GameActivation.gameStartEvents(3, GameId("gx"), BotId("b1"), BotId("b2")) == Vector(
        TournamentEvent.GameStart(3, GameId("gx"), Color.White, BotId("b1")),
        TournamentEvent.GameStart(3, GameId("gx"), Color.Black, BotId("b2")),
      ))
    ,
    test("activeGameStarts returns nothing when the current round is absent"):
      val t = tournament(currentRound = 5, rounds = Vector.empty)
      assertTrue(GameActivation.activeGameStarts(t, Vector.empty).isEmpty)
    ,
    test("activeGameStarts emits gameStart only for ongoing games, skipping pending ones"):
      val round = Round(1, Vector(Pairing(
        bot1, bot2,
        Vector(Match(GameId("g-on"), bot1.id, None, None), Match(GameId("g-pend"), bot1.id, None, None)),
        None,
      )))
      val t = tournament(currentRound = 1, rounds = Vector(round))
      val games = Vector(game("g-on", GameStatus.Ongoing), game("g-pend", GameStatus.Pending))
      val events = GameActivation.activeGameStarts(t, games)
      assertTrue(
        events == Vector(
          TournamentEvent.GameStart(1, GameId("g-on"), Color.White, BotId("b1")),
          TournamentEvent.GameStart(1, GameId("g-on"), Color.Black, BotId("b2")),
        ),
      )
  )
