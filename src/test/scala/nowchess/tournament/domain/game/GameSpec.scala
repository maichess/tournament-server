package nowchess.tournament.domain.game

import zio.test.*
import nowchess.tournament.domain.model.*

object GameSpec extends ZIOSpecDefault:

  private val bot1 = BotRef(BotId("b1"), "Bot1")
  private val bot2 = BotRef(BotId("b2"), "Bot2")

  private def mkGame(
    status: GameStatus = GameStatus.Ongoing,
    turn: Color = Color.White,
    winner: Option[Color] = None,
  ) = Game(
    id = GameId("g1"), tournamentId = TournamentId("t1"), round = 1,
    white = bot1, black = bot2, moves = Vector.empty,
    status = status, turn = turn, winner = winner,
    clock = GameClock(300.0, 300.0),
    startPosition = StartPosition.Standard,
    fen = StartPosition.standardFen,
  )

  def spec = suite("Game")(
    test("currentPlayer returns white when white's turn") {
      val g = mkGame(turn = Color.White)
      assertTrue(g.currentPlayer == bot1)
    },
    test("currentPlayer returns black when black's turn") {
      val g = mkGame(turn = Color.Black)
      assertTrue(g.currentPlayer == bot2)
    },
    test("toOutcome returns White on white win") {
      val g = mkGame(status = GameStatus.Checkmate, winner = Some(Color.White))
      assertTrue(g.toOutcome.contains(GameOutcome.White))
    },
    test("toOutcome returns Black on black win") {
      val g = mkGame(status = GameStatus.Checkmate, winner = Some(Color.Black))
      assertTrue(g.toOutcome.contains(GameOutcome.Black))
    },
    test("toOutcome returns Draw on terminal with no winner") {
      val g = mkGame(status = GameStatus.Stalemate, winner = None)
      assertTrue(g.toOutcome.contains(GameOutcome.Draw))
    },
    test("toOutcome returns None when ongoing") {
      val g = mkGame()
      assertTrue(g.toOutcome.isEmpty)
    },
    test("movesUci joins moves with space") {
      val g = mkGame().copy(moves = Vector("e2e4", "e7e5"))
      assertTrue(g.movesUci == "e2e4 e7e5")
    },
  )

object GameStatusSpec extends ZIOSpecDefault:
  def spec = suite("GameStatus")(
    test("Ongoing is not terminal") {
      assertTrue(!GameStatus.Ongoing.isTerminal)
    },
    test("Checkmate is terminal") {
      assertTrue(GameStatus.Checkmate.isTerminal)
    },
    test("Stalemate is terminal") {
      assertTrue(GameStatus.Stalemate.isTerminal)
    },
    test("Draw is terminal") {
      assertTrue(GameStatus.Draw.isTerminal)
    },
    test("Resigned is terminal") {
      assertTrue(GameStatus.Resigned.isTerminal)
    },
    test("Timeout is terminal") {
      assertTrue(GameStatus.Timeout.isTerminal)
    },
  )
