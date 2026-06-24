package tournament.http.codec

import zio.json.*
import tournament.domain.model.*
import tournament.domain.opening.Opening
import tournament.domain.tournament.*
import tournament.domain.round.{Round, Pairing, Match as GameMatch}
import tournament.domain.standing.{Standing, Result}
import tournament.domain.game.{Game, GameStatus, GameClock}
import tournament.domain.event.{TournamentEvent, GameEvent}
import tournament.domain.error.DomainError
import java.time.Instant

object JsonCodecs:

  /** Encodes a JSON-enum value to its bare string form (no surrounding quotes). */
  def encodeAsString[A: JsonEncoder](a: A): String =
    summon[JsonEncoder[A]].encodeJson(a).toString.stripPrefix("\"").stripSuffix("\"")

  // --- Instant codec (ISO-8601 string) ---
  given JsonEncoder[Instant] = JsonEncoder[String].contramap(_.toString)

  // --- Opaque type codecs ---
  given JsonEncoder[TournamentId] = JsonEncoder[String].contramap(_.value)
  given JsonDecoder[TournamentId] = JsonDecoder[String].map(TournamentId(_))
  given JsonEncoder[BotId] = JsonEncoder[String].contramap(_.value)
  given JsonDecoder[BotId] = JsonDecoder[String].map(BotId(_))
  given JsonEncoder[UserId] = JsonEncoder[String].contramap(_.value)
  given JsonDecoder[UserId] = JsonDecoder[String].map(UserId(_))
  given JsonEncoder[GameId] = JsonEncoder[String].contramap(_.value)
  given JsonDecoder[GameId] = JsonDecoder[String].map(GameId(_))

  // --- Simple model codecs ---
  given JsonEncoder[Clock] = DeriveJsonEncoder.gen[Clock]
  given JsonDecoder[Clock] = DeriveJsonDecoder.gen[Clock]
  given JsonEncoder[Variant] = DeriveJsonEncoder.gen[Variant]
  given JsonDecoder[Variant] = DeriveJsonDecoder.gen[Variant]
  given JsonEncoder[BotRef] = DeriveJsonEncoder.gen[BotRef]
  given JsonDecoder[BotRef] = DeriveJsonDecoder.gen[BotRef]
  given JsonEncoder[Opening] = DeriveJsonEncoder.gen[Opening]
  given JsonDecoder[Opening] = DeriveJsonDecoder.gen[Opening]
  given JsonEncoder[RegisteredBot] = DeriveJsonEncoder.gen[RegisteredBot]
  given JsonDecoder[RegisteredBot] = DeriveJsonDecoder.gen[RegisteredBot]
  given JsonEncoder[GameClock] = DeriveJsonEncoder.gen[GameClock]
  given JsonDecoder[GameClock] = DeriveJsonDecoder.gen[GameClock]

  given JsonEncoder[Color] = JsonEncoder[String].contramap:
    case Color.White => "white"
    case Color.Black => "black"

  given JsonDecoder[Color] = JsonDecoder[String].mapOrFail:
    case "white" => Right(Color.White)
    case "black" => Right(Color.Black)
    case other   => Left(s"Invalid color: $other")

  given JsonEncoder[GameOutcome] = JsonEncoder[String].contramap:
    case GameOutcome.White => "white"
    case GameOutcome.Black => "black"
    case GameOutcome.Draw  => "draw"

  given JsonDecoder[GameOutcome] = JsonDecoder[String].mapOrFail:
    case "white" => Right(GameOutcome.White)
    case "black" => Right(GameOutcome.Black)
    case "draw"  => Right(GameOutcome.Draw)
    case other   => Left(s"Invalid outcome: $other")

  given JsonEncoder[GameStatus] = JsonEncoder[String].contramap:
    case GameStatus.Pending   => "pending"
    case GameStatus.Ongoing   => "ongoing"
    case GameStatus.Checkmate => "checkmate"
    case GameStatus.Stalemate => "stalemate"
    case GameStatus.Draw      => "draw"
    case GameStatus.Resigned  => "resigned"
    case GameStatus.Timeout   => "timeout"

  given JsonDecoder[GameStatus] = JsonDecoder[String].mapOrFail:
    case "pending"   => Right(GameStatus.Pending)
    case "ongoing"   => Right(GameStatus.Ongoing)
    case "checkmate" => Right(GameStatus.Checkmate)
    case "stalemate" => Right(GameStatus.Stalemate)
    case "draw"      => Right(GameStatus.Draw)
    case "resigned"  => Right(GameStatus.Resigned)
    case "timeout"   => Right(GameStatus.Timeout)
    case other       => Left(s"Invalid status: $other")

  given JsonEncoder[TournamentStatus] = JsonEncoder[String].contramap:
    case TournamentStatus.Created  => "created"
    case TournamentStatus.Started  => "started"
    case TournamentStatus.Finished => "finished"

  given JsonEncoder[TournamentFormat] = JsonEncoder[String].contramap:
    case TournamentFormat.Swiss            => "swiss"
    case TournamentFormat.SingleElimination => "singleElimination"
    case TournamentFormat.DoubleElimination => "doubleElimination"
    case TournamentFormat.GroupStage(_)     => "groupStage"
    case TournamentFormat.League           => "league"
    case TournamentFormat.RandomKnockout   => "randomKnockout"

  given JsonDecoder[TournamentFormat] = JsonDecoder[String].mapOrFail:
    case "swiss"             => Right(TournamentFormat.Swiss)
    case "singleElimination" => Right(TournamentFormat.SingleElimination)
    case "doubleElimination" => Right(TournamentFormat.DoubleElimination)
    case "groupStage"        => Right(TournamentFormat.GroupStage(4))
    case "league"            => Right(TournamentFormat.League)
    case "randomKnockout"    => Right(TournamentFormat.RandomKnockout)
    case other               => Left(s"Invalid format: $other")

  given JsonEncoder[StartPosition] = JsonEncoder[String].contramap:
    case StartPosition.Standard     => "standard"
    case StartPosition.FromFen(fen) => fen

  given JsonDecoder[StartPosition] = JsonDecoder[String].map(StartPosition.fromString)

  // --- Compound codecs ---
  given JsonEncoder[GameMatch] = DeriveJsonEncoder.gen[GameMatch]
  given JsonEncoder[Pairing] = DeriveJsonEncoder.gen[Pairing]
  given JsonEncoder[Round] = DeriveJsonEncoder.gen[Round]
  given JsonEncoder[Result] = DeriveJsonEncoder.gen[Result]
  given JsonEncoder[Standing] = DeriveJsonEncoder.gen[Standing]

  given JsonEncoder[TournamentConfig] = DeriveJsonEncoder.gen[TournamentConfig]

  // Tournament JSON (flattened for API response)
  given JsonEncoder[Tournament] = JsonEncoder[zio.json.ast.Json].contramap: t =>
    import zio.json.ast.Json.*
    Obj(
      "id" -> Str(t.id.value),
      "fullName" -> Str(t.config.name),
      "clock" -> Obj("limit" -> Num(t.config.clock.limit), "increment" -> Num(t.config.clock.increment)),
      "variant" -> Obj("key" -> Str("standard"), "name" -> Str("Standard")),
      "rated" -> Bool(t.config.rated),
      "nbPlayers" -> Num(t.participants.size),
      "nbRounds" -> Num(t.config.nbRounds),
      "format" -> Str(encodeAsString(t.config.format)),
      "matchesPerPairing" -> Num(t.config.matchesPerPairing),
      "startPosition" -> Str(t.config.startPosition match { case StartPosition.Standard => "standard"; case StartPosition.FromFen(f) => f }),
      "createdBy" -> Str(t.director.value),
      "status" -> Str(encodeAsString(t.status)),
      "round" -> Num(t.currentRound),
      "standing" -> Standing(1, ScoringRulesHelper.compute(t)).toJsonAST.getOrElse(Null),
      "winner" -> t.winner.map(b => b.toJsonAST.getOrElse(Null)).getOrElse(Null),
    )

  // Game state JSON
  given JsonEncoder[Game] = JsonEncoder[zio.json.ast.Json].contramap: g =>
    import zio.json.ast.Json.*
    Obj(
      "id" -> Str(g.id.value),
      "tournamentId" -> Str(g.tournamentId.value),
      "round" -> Num(g.round),
      "white" -> g.white.toJsonAST.getOrElse(Null),
      "black" -> g.black.toJsonAST.getOrElse(Null),
      "moves" -> Str(g.movesUci),
      "fen" -> Str(g.fen),
      "status" -> Str(encodeAsString(g.status)),
      "turn" -> Str(if g.turn == Color.White then "white" else "black"),
      "winner" -> g.winner.map(c => Str(if c == Color.White then "white" else "black")).getOrElse(Null),
      "clock" -> Obj("whiteTime" -> Num(g.clock.whiteTime), "blackTime" -> Num(g.clock.blackTime), "increment" -> Num(g.clock.increment)),
      "startPosition" -> Str(g.startPosition match { case StartPosition.Standard => "standard"; case StartPosition.FromFen(f) => f }),
      "startedAt" -> g.startedAt.map(i => Str(i.toString)).getOrElse(Null),
      "endedAt" -> g.endedAt.map(i => Str(i.toString)).getOrElse(Null),
    )

  // --- Event codecs ---
  given JsonEncoder[TournamentEvent] = JsonEncoder[zio.json.ast.Json].contramap:
    case TournamentEvent.TournamentStarted =>
      zio.json.ast.Json.Obj("type" -> zio.json.ast.Json.Str("tournamentStarted"))
    case TournamentEvent.RoundStarted(r) =>
      zio.json.ast.Json.Obj("type" -> zio.json.ast.Json.Str("roundStarted"), "round" -> zio.json.ast.Json.Num(r))
    case TournamentEvent.GameStart(r, gid, color, botId) =>
      zio.json.ast.Json.Obj(
        "type" -> zio.json.ast.Json.Str("gameStart"),
        "round" -> zio.json.ast.Json.Num(r),
        "gameId" -> zio.json.ast.Json.Str(gid.value),
        "color" -> zio.json.ast.Json.Str(if color == Color.White then "white" else "black"),
        "botId" -> zio.json.ast.Json.Str(botId.value),
      )
    case TournamentEvent.RoundFinished(r) =>
      zio.json.ast.Json.Obj("type" -> zio.json.ast.Json.Str("roundFinished"), "round" -> zio.json.ast.Json.Num(r))
    case TournamentEvent.TournamentFinished(w) =>
      zio.json.ast.Json.Obj("type" -> zio.json.ast.Json.Str("tournamentFinished"), "winner" -> w.toJsonAST.getOrElse(zio.json.ast.Json.Null))

  given JsonEncoder[GameEvent] = JsonEncoder[zio.json.ast.Json].contramap:
    case GameEvent.GameState(fen, moves, turn, clock, status, winner) =>
      import zio.json.ast.Json.*
      Obj(
        "type" -> Str("gameState"), "fen" -> Str(fen), "moves" -> Str(moves),
        "turn" -> Str(if turn == Color.White then "white" else "black"),
        "clock" -> Obj("whiteTime" -> Num(clock.whiteTime), "blackTime" -> Num(clock.blackTime), "increment" -> Num(clock.increment)),
        "status" -> Str(encodeAsString(status)),
        "winner" -> winner.map(c => Str(if c == Color.White then "white" else "black")).getOrElse(Null),
      )
    case GameEvent.MovePlayed(uci, fen, turn, clock) =>
      import zio.json.ast.Json.*
      Obj(
        "type" -> Str("move"), "uci" -> Str(uci), "fen" -> Str(fen),
        "turn" -> Str(if turn == Color.White then "white" else "black"),
        "clock" -> Obj("whiteTime" -> Num(clock.whiteTime), "blackTime" -> Num(clock.blackTime), "increment" -> Num(clock.increment)),
      )
    case GameEvent.GameEnd(winner, status) =>
      import zio.json.ast.Json.*
      Obj(
        "type" -> Str("gameEnd"),
        "winner" -> winner.map(c => Str(if c == Color.White then "white" else "black")).getOrElse(Null),
        "status" -> Str(encodeAsString(status)),
      )

  // --- Error codec ---
  given JsonEncoder[DomainError] = JsonEncoder[zio.json.ast.Json].contramap: e =>
    zio.json.ast.Json.Obj("error" -> zio.json.ast.Json.Str(e.message))

  // Ok response
  final case class OkResponse(ok: Boolean)
  given JsonEncoder[OkResponse] = DeriveJsonEncoder.gen[OkResponse]

  // Tournament list response
  final case class TournamentListResponse(
    created: Vector[Tournament],
    started: Vector[Tournament],
    finished: Vector[Tournament],
  )
  given JsonEncoder[TournamentListResponse] = DeriveJsonEncoder.gen[TournamentListResponse]

  // Round response
  final case class RoundResponse(round: Int, pairings: Vector[Pairing])
  given JsonEncoder[RoundResponse] = DeriveJsonEncoder.gen[RoundResponse]

  // Game export
  final case class GameExportJson(
    id: String, round: Int, white: BotRef, black: BotRef,
    winner: Option[String], moves: String,
  )
  given JsonEncoder[GameExportJson] = DeriveJsonEncoder.gen[GameExportJson]

  // Analytics export DTOs — no analytics computed here, only raw data assembly
  final case class AnalyticsExportGame(
    gameId: String,
    tournamentId: String,
    round: Int,
    whiteBotId: String,
    whiteBotName: String,
    whiteBotFamily: Option[String],
    whiteStrategyType: Option[String],
    whiteEngineType: Option[String],
    whiteModelVersion: Option[String],
    blackBotId: String,
    blackBotName: String,
    blackBotFamily: Option[String],
    blackStrategyType: Option[String],
    blackEngineType: Option[String],
    blackModelVersion: Option[String],
    winner: Option[String],
    winnerBotId: Option[String],
    terminationReason: String,
    totalPly: Int,
    moves: String,
    startedAt: Option[String],
    endedAt: Option[String],
    durationMillis: Option[Long],
  )
  given JsonEncoder[AnalyticsExportGame] = DeriveJsonEncoder.gen[AnalyticsExportGame]

  final case class AnalyticsExportStanding(
    tournamentId: String,
    botId: String,
    botName: String,
    botFamily: Option[String],
    strategyType: Option[String],
    engineType: Option[String],
    modelVersion: Option[String],
    rank: Int,
    points: Double,
    wins: Int,
    draws: Int,
    losses: Int,
    nbGames: Int,
    tieBreak: Double,
  )
  given JsonEncoder[AnalyticsExportStanding] = DeriveJsonEncoder.gen[AnalyticsExportStanding]

  final case class AnalyticsExportResponse(
    schemaVersion: String,
    tournamentId: String,
    format: String,
    clock: Clock,
    rated: Boolean,
    nbRounds: Int,
    startedAt: Option[String],
    finishedAt: Option[String],
    exportedAt: String,
    standings: Vector[AnalyticsExportStanding],
    games: Vector[AnalyticsExportGame],
  )
  given JsonEncoder[AnalyticsExportResponse] = DeriveJsonEncoder.gen[AnalyticsExportResponse]

private object ScoringRulesHelper:
  import tournament.domain.standing.{ScoringRules, Result}
  import tournament.domain.tournament.Tournament
  def compute(t: Tournament): Vector[Result] = ScoringRules.computeStandings(t)
