package nowchess.tournament.service

import zio.*
import nowchess.tournament.domain.model.{BotRef, Color, GameId}
import nowchess.tournament.domain.game.{Game, GameStatus, GameClock}
import nowchess.tournament.domain.round.Match
import nowchess.tournament.domain.tournament.{Tournament, TournamentFormat}
import nowchess.tournament.domain.pairing.*
import nowchess.tournament.persistence.GameRepository

/** Creates the games of a pairing from the tournament config (single position
  * repeated, or a multi-position opening book played with reversed colours) and
  * persists them, returning the `Match` rows that reference them. Shared by
  * round-one setup (TournamentService) and round progression (GameService) so
  * the colour/position policy lives in one place.
  */
object GameFactory:

  def selectAlgorithm(tournament: Tournament): PairingAlgorithm =
    tournament.config.format match
      case TournamentFormat.Swiss             => SwissPairing
      case TournamentFormat.SingleElimination => EliminationBracket
      case TournamentFormat.DoubleElimination => EliminationBracket
      case TournamentFormat.GroupStage(_)     => GroupStagePairing
      case TournamentFormat.League            => RoundRobinPairing
      case TournamentFormat.RandomKnockout    => RandomKnockoutPairing(tournament.seed)

  def createForPairing(
    tournament: Tournament,
    roundNum: Int,
    pair: (BotRef, BotRef),
    gameRepo: GameRepository,
  ): Task[((BotRef, BotRef), Vector[Match])] =
    create(tournament, roundNum, pair, gameRepo).map(matches => (pair, matches))

  def create(
    tournament: Tournament,
    roundNum: Int,
    pair: (BotRef, BotRef),
    gameRepo: GameRepository,
  ): Task[Vector[Match]] =
    val (first, second) = pair
    ZIO.foreach(tournament.config.pairingGameSpecs(first, second)): (white, black, startPos) =>
      val gameId = GameId(java.util.UUID.randomUUID().toString.take(8))
      val game = Game(
        id = gameId,
        tournamentId = tournament.id,
        round = roundNum,
        white = white,
        black = black,
        moves = Vector.empty,
        status = GameStatus.Pending,
        turn = Color.White,
        winner = None,
        clock = GameClock(tournament.config.clock.limit.toDouble, tournament.config.clock.limit.toDouble),
        startPosition = startPos,
        fen = startPos.toFen,
      )
      gameRepo.save(game).as(Match(gameId, white.id, None, None))
