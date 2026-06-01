package nowchess.tournament.domain.game

import nowchess.tournament.domain.model.Color

object ChessRules:

  final case class Square(file: Int, rank: Int):
    def isValid: Boolean = file >= 0 && file < 8 && rank >= 0 && rank < 8

  enum PieceType:
    case King, Queen, Rook, Bishop, Knight, Pawn

  final case class Piece(pieceType: PieceType, color: Color)

  final case class Board(
    squares: Vector[Vector[Option[Piece]]],
    turn: Color,
    castling: CastlingRights,
    enPassant: Option[Square],
    halfmoveClock: Int,
    fullmoveNumber: Int,
  ):
    def get(sq: Square): Option[Piece] =
      if sq.isValid then squares(sq.rank)(sq.file) else None

    def set(sq: Square, piece: Option[Piece]): Board =
      copy(squares = squares.updated(sq.rank, squares(sq.rank).updated(sq.file, piece)))

    def isOccupied(sq: Square): Boolean = get(sq).isDefined

    def isOccupiedBy(sq: Square, color: Color): Boolean =
      get(sq).exists(_.color == color)

  final case class CastlingRights(
    whiteKingside: Boolean,
    whiteQueenside: Boolean,
    blackKingside: Boolean,
    blackQueenside: Boolean,
  )

  final case class Move(from: Square, to: Square, promotion: Option[PieceType])

  def parseFen(fen: String): Either[String, Board] =
    val parts = fen.split(' ')
    if parts.length < 4 then Left("Invalid FEN: too few fields")
    else
      val ranks = parts(0).split('/')
      if ranks.length != 8 then Left("Invalid FEN: expected 8 ranks")
      else
        val board = ranks.reverse.map: rank =>
          rank.foldLeft(Vector.empty[Option[Piece]]): (row, c) =>
            if c.isDigit then row ++ Vector.fill(c.asDigit)(None)
            else row :+ Some(charToPiece(c))

        if board.exists(_.length != 8) then Left("Invalid FEN: rank length mismatch")
        else
          val turn = if parts(1) == "w" then Color.White else Color.Black
          val castling = CastlingRights(
            whiteKingside = parts(2).contains('K'),
            whiteQueenside = parts(2).contains('Q'),
            blackKingside = parts(2).contains('k'),
            blackQueenside = parts(2).contains('q'),
          )
          val ep = if parts(3) == "-" then None else parseSquare(parts(3))
          val halfmove = if parts.length > 4 then parts(4).toIntOption.getOrElse(0) else 0
          val fullmove = if parts.length > 5 then parts(5).toIntOption.getOrElse(1) else 1
          Right(Board(board.toVector, turn, castling, ep, halfmove, fullmove))

  def boardToFen(board: Board): String =
    val ranks = board.squares.reverse.map: rank =>
      rank.foldLeft(("", 0)): (acc, sq) =>
        sq match
          case None => (acc._1, acc._2 + 1)
          case Some(p) =>
            val prefix = if acc._2 > 0 then acc._1 + acc._2.toString else acc._1
            (prefix + pieceToChar(p), 0)
      match
        case (s, 0) => s
        case (s, n) => s + n.toString
    val turnStr = if board.turn == Color.White then "w" else "b"
    val castleStr =
      val sb = new StringBuilder
      if board.castling.whiteKingside then sb.append('K')
      if board.castling.whiteQueenside then sb.append('Q')
      if board.castling.blackKingside then sb.append('k')
      if board.castling.blackQueenside then sb.append('q')
      if sb.isEmpty then "-" else sb.toString
    val epStr = board.enPassant.map(sq => s"${('a' + sq.file).toChar}${sq.rank + 1}").getOrElse("-")
    s"${ranks.mkString("/")} $turnStr $castleStr $epStr ${board.halfmoveClock} ${board.fullmoveNumber}"

  def parseUci(uci: String): Either[String, Move] =
    if uci.length < 4 || uci.length > 5 then Left(s"Invalid UCI: $uci")
    else
      for
        from <- parseSquare(uci.substring(0, 2)).toRight(s"Invalid from square: ${uci.substring(0, 2)}")
        to   <- parseSquare(uci.substring(2, 4)).toRight(s"Invalid to square: ${uci.substring(2, 4)}")
      yield
        val promo = if uci.length == 5 then uci(4) match
          case 'q' => Some(PieceType.Queen)
          case 'r' => Some(PieceType.Rook)
          case 'b' => Some(PieceType.Bishop)
          case 'n' => Some(PieceType.Knight)
          case _   => None
        else None
        Move(from, to, promo)

  def isLegalMove(board: Board, move: Move): Boolean =
    board.get(move.from) match
      case None => false
      case Some(piece) =>
        if piece.color != board.turn then false
        else
          val pseudoLegal = isPseudoLegalMove(board, piece, move)
          if !pseudoLegal then false
          else
            val after = applyMoveUnchecked(board, move)
            !isInCheck(after, board.turn)

  def applyMove(board: Board, move: Move): Either[String, Board] =
    if !isLegalMove(board, move) then Left("Illegal move")
    else Right(applyMoveUnchecked(board, move))

  def isInCheck(board: Board, color: Color): Boolean =
    findKing(board, color) match
      case None => false
      case Some(kingPos) => isAttackedBy(board, kingPos, color.opposite)

  def isCheckmate(board: Board): Boolean =
    isInCheck(board, board.turn) && !hasLegalMoves(board)

  def isStalemate(board: Board): Boolean =
    !isInCheck(board, board.turn) && !hasLegalMoves(board)

  def isDraw(board: Board): Boolean =
    isStalemate(board) || board.halfmoveClock >= 100 || isInsufficientMaterial(board)

  private def hasLegalMoves(board: Board): Boolean =
    allSquares.exists: sq =>
      board.get(sq).exists: piece =>
        piece.color == board.turn && generateMovesFor(board, sq, piece).exists(m => isLegalMove(board, m))

  private def generateMovesFor(board: Board, from: Square, piece: Piece): Vector[Move] =
    piece.pieceType match
      case PieceType.Pawn   => generatePawnMoves(board, from, piece.color)
      case PieceType.Knight => generateKnightMoves(from).map(to => Move(from, to, None))
      case PieceType.Bishop => generateSlidingMoves(board, from, bishopDirs).map(to => Move(from, to, None))
      case PieceType.Rook   => generateSlidingMoves(board, from, rookDirs).map(to => Move(from, to, None))
      case PieceType.Queen  => generateSlidingMoves(board, from, queenDirs).map(to => Move(from, to, None))
      case PieceType.King   => generateKingMoves(board, from, piece.color).map(to => Move(from, to, None))

  private def generatePawnMoves(board: Board, from: Square, color: Color): Vector[Move] =
    val dir = if color == Color.White then 1 else -1
    val startRank = if color == Color.White then 1 else 6
    val promoRank = if color == Color.White then 7 else 0
    val moves = Vector.newBuilder[Move]

    val oneStep = Square(from.file, from.rank + dir)
    if oneStep.isValid && !board.isOccupied(oneStep) then
      if oneStep.rank == promoRank then
        for p <- Vector(PieceType.Queen, PieceType.Rook, PieceType.Bishop, PieceType.Knight) do
          moves += Move(from, oneStep, Some(p))
      else
        moves += Move(from, oneStep, None)
        if from.rank == startRank then
          val twoStep = Square(from.file, from.rank + 2 * dir)
          if twoStep.isValid && !board.isOccupied(twoStep) then
            moves += Move(from, twoStep, None)

    for df <- Vector(-1, 1) do
      val cap = Square(from.file + df, from.rank + dir)
      if cap.isValid then
        val isCapture = board.isOccupiedBy(cap, color.opposite)
        val isEp = board.enPassant.contains(cap)
        if isCapture || isEp then
          if cap.rank == promoRank then
            for p <- Vector(PieceType.Queen, PieceType.Rook, PieceType.Bishop, PieceType.Knight) do
              moves += Move(from, cap, Some(p))
          else
            moves += Move(from, cap, None)

    moves.result()

  private val knightOffsets = Vector((-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1))

  private def generateKnightMoves(from: Square): Vector[Square] =
    knightOffsets.map((df, dr) => Square(from.file + df, from.rank + dr)).filter(_.isValid)

  private val bishopDirs = Vector((-1,-1),(-1,1),(1,-1),(1,1))
  private val rookDirs = Vector((-1,0),(1,0),(0,-1),(0,1))
  private val queenDirs = bishopDirs ++ rookDirs

  private def generateSlidingMoves(board: Board, from: Square, dirs: Vector[(Int,Int)]): Vector[Square] =
    dirs.flatMap: (df, dr) =>
      Iterator.iterate(Square(from.file + df, from.rank + dr))(sq => Square(sq.file + df, sq.rank + dr))
        .takeWhile(_.isValid)
        .foldLeft((Vector.empty[Square], false)): (acc, sq) =>
          if acc._2 then acc
          else if board.isOccupied(sq) then
            if board.isOccupiedBy(sq, board.turn) then (acc._1, true)
            else (acc._1 :+ sq, true) // opponent piece: capture and stop
          else (acc._1 :+ sq, false)
        ._1

  private def generateKingMoves(board: Board, from: Square, color: Color): Vector[Square] =
    val normal = queenDirs.map((df, dr) => Square(from.file + df, from.rank + dr))
      .filter(sq => sq.isValid && !board.isOccupiedBy(sq, color))
    val castling = Vector.newBuilder[Square]
    val rank = if color == Color.White then 0 else 7
    if from == Square(4, rank) then
      if canCastleKingside(board, color) then castling += Square(6, rank)
      if canCastleQueenside(board, color) then castling += Square(2, rank)
    normal ++ castling.result()

  private def canCastleKingside(board: Board, color: Color): Boolean =
    val rank = if color == Color.White then 0 else 7
    val hasRight = if color == Color.White then board.castling.whiteKingside else board.castling.blackKingside
    hasRight &&
      !board.isOccupied(Square(5, rank)) &&
      !board.isOccupied(Square(6, rank)) &&
      !isAttackedBy(board, Square(4, rank), color.opposite) &&
      !isAttackedBy(board, Square(5, rank), color.opposite) &&
      !isAttackedBy(board, Square(6, rank), color.opposite)

  private def canCastleQueenside(board: Board, color: Color): Boolean =
    val rank = if color == Color.White then 0 else 7
    val hasRight = if color == Color.White then board.castling.whiteQueenside else board.castling.blackQueenside
    hasRight &&
      !board.isOccupied(Square(1, rank)) &&
      !board.isOccupied(Square(2, rank)) &&
      !board.isOccupied(Square(3, rank)) &&
      !isAttackedBy(board, Square(4, rank), color.opposite) &&
      !isAttackedBy(board, Square(2, rank), color.opposite) &&
      !isAttackedBy(board, Square(3, rank), color.opposite)

  private def isPseudoLegalMove(board: Board, piece: Piece, move: Move): Boolean =
    if board.isOccupiedBy(move.to, piece.color) then false
    else piece.pieceType match
      case PieceType.Pawn   => isPseudoLegalPawnMove(board, piece.color, move)
      case PieceType.Knight =>
        val df = Math.abs(move.to.file - move.from.file)
        val dr = Math.abs(move.to.rank - move.from.rank)
        (df == 1 && dr == 2) || (df == 2 && dr == 1)
      case PieceType.Bishop => isDiagonalClear(board, move.from, move.to)
      case PieceType.Rook   => isStraightClear(board, move.from, move.to)
      case PieceType.Queen  => isDiagonalClear(board, move.from, move.to) || isStraightClear(board, move.from, move.to)
      case PieceType.King   => isPseudoLegalKingMove(board, piece.color, move)

  private def isPseudoLegalPawnMove(board: Board, color: Color, move: Move): Boolean =
    val dir = if color == Color.White then 1 else -1
    val startRank = if color == Color.White then 1 else 6
    val df = move.to.file - move.from.file
    val dr = move.to.rank - move.from.rank
    if df == 0 && dr == dir && !board.isOccupied(move.to) then true
    else if df == 0 && dr == 2 * dir && move.from.rank == startRank &&
            !board.isOccupied(Square(move.from.file, move.from.rank + dir)) &&
            !board.isOccupied(move.to) then true
    else if Math.abs(df) == 1 && dr == dir then
      board.isOccupiedBy(move.to, color.opposite) || board.enPassant.contains(move.to)
    else false

  private def isPseudoLegalKingMove(board: Board, color: Color, move: Move): Boolean =
    val df = Math.abs(move.to.file - move.from.file)
    val dr = Math.abs(move.to.rank - move.from.rank)
    if df <= 1 && dr <= 1 then true
    else
      val rank = if color == Color.White then 0 else 7
      if move.from == Square(4, rank) && move.to == Square(6, rank) then canCastleKingside(board, color)
      else if move.from == Square(4, rank) && move.to == Square(2, rank) then canCastleQueenside(board, color)
      else false

  private def isDiagonalClear(board: Board, from: Square, to: Square): Boolean =
    val df = to.file - from.file
    val dr = to.rank - from.rank
    if Math.abs(df) != Math.abs(dr) || df == 0 then false
    else
      val stepF = if df > 0 then 1 else -1
      val stepR = if dr > 0 then 1 else -1
      (1 until Math.abs(df)).forall: i =>
        !board.isOccupied(Square(from.file + i * stepF, from.rank + i * stepR))

  private def isStraightClear(board: Board, from: Square, to: Square): Boolean =
    val df = to.file - from.file
    val dr = to.rank - from.rank
    if (df != 0 && dr != 0) || (df == 0 && dr == 0) then false
    else
      val stepF = if df > 0 then 1 else if df < 0 then -1 else 0
      val stepR = if dr > 0 then 1 else if dr < 0 then -1 else 0
      val steps = Math.max(Math.abs(df), Math.abs(dr))
      (1 until steps).forall: i =>
        !board.isOccupied(Square(from.file + i * stepF, from.rank + i * stepR))

  private def isAttackedBy(board: Board, sq: Square, byColor: Color): Boolean =
    isAttackedByPawn(board, sq, byColor) ||
    isAttackedByKnight(board, sq, byColor) ||
    isAttackedBySliding(board, sq, byColor, bishopDirs, Set(PieceType.Bishop, PieceType.Queen)) ||
    isAttackedBySliding(board, sq, byColor, rookDirs, Set(PieceType.Rook, PieceType.Queen)) ||
    isAttackedByKing(board, sq, byColor)

  private def isAttackedByPawn(board: Board, sq: Square, byColor: Color): Boolean =
    val dir = if byColor == Color.White then 1 else -1
    Vector(-1, 1).exists: df =>
      val from = Square(sq.file + df, sq.rank - dir)
      from.isValid && board.get(from).exists(p => p.color == byColor && p.pieceType == PieceType.Pawn)

  private def isAttackedByKnight(board: Board, sq: Square, byColor: Color): Boolean =
    knightOffsets.exists: (df, dr) =>
      val from = Square(sq.file + df, sq.rank + dr)
      from.isValid && board.get(from).exists(p => p.color == byColor && p.pieceType == PieceType.Knight)

  private def isAttackedBySliding(board: Board, sq: Square, byColor: Color, dirs: Vector[(Int,Int)], types: Set[PieceType]): Boolean =
    dirs.exists: (df, dr) =>
      Iterator.iterate(Square(sq.file + df, sq.rank + dr))(s => Square(s.file + df, s.rank + dr))
        .takeWhile(_.isValid)
        .foldLeft(Option.empty[Boolean]): (found, s) =>
          found.orElse:
            board.get(s) match
              case None => None
              case Some(p) if p.color == byColor && types.contains(p.pieceType) => Some(true)
              case _ => Some(false)
        .getOrElse(false)

  private def isAttackedByKing(board: Board, sq: Square, byColor: Color): Boolean =
    queenDirs.exists: (df, dr) =>
      val s = Square(sq.file + df, sq.rank + dr)
      s.isValid && board.get(s).exists(p => p.color == byColor && p.pieceType == PieceType.King)

  private def findKing(board: Board, color: Color): Option[Square] =
    allSquares.find(sq => board.get(sq).exists(p => p.color == color && p.pieceType == PieceType.King))

  private def applyMoveUnchecked(board: Board, move: Move): Board =
    val piece = board.get(move.from).get
    val captured = board.get(move.to)

    var b = board.set(move.from, None)

    val movedPiece = move.promotion match
      case Some(pt) => piece.copy(pieceType = pt)
      case None     => piece

    b = b.set(move.to, Some(movedPiece))

    // en passant capture
    if piece.pieceType == PieceType.Pawn && board.enPassant.contains(move.to) then
      val capturedRank = if piece.color == Color.White then move.to.rank - 1 else move.to.rank + 1
      b = b.set(Square(move.to.file, capturedRank), None)

    // castling rook movement
    if piece.pieceType == PieceType.King then
      val df = move.to.file - move.from.file
      if Math.abs(df) == 2 then
        val rank = move.from.rank
        if df > 0 then // kingside
          b = b.set(Square(7, rank), None).set(Square(5, rank), board.get(Square(7, rank)))
        else // queenside
          b = b.set(Square(0, rank), None).set(Square(3, rank), board.get(Square(0, rank)))

    // update en passant
    val newEp = if piece.pieceType == PieceType.Pawn && Math.abs(move.to.rank - move.from.rank) == 2 then
      Some(Square(move.from.file, (move.from.rank + move.to.rank) / 2))
    else None

    // update castling rights
    val newCastling = updateCastlingRights(board.castling, move, piece)

    // update halfmove clock
    val newHalfmove = if piece.pieceType == PieceType.Pawn || captured.isDefined then 0 else board.halfmoveClock + 1

    // update fullmove
    val newFullmove = if board.turn == Color.Black then board.fullmoveNumber + 1 else board.fullmoveNumber

    b.copy(
      turn = board.turn.opposite,
      castling = newCastling,
      enPassant = newEp,
      halfmoveClock = newHalfmove,
      fullmoveNumber = newFullmove,
    )

  private def updateCastlingRights(rights: CastlingRights, move: Move, piece: Piece): CastlingRights =
    var r = rights
    if piece.pieceType == PieceType.King then
      if piece.color == Color.White then r = r.copy(whiteKingside = false, whiteQueenside = false)
      else r = r.copy(blackKingside = false, blackQueenside = false)
    if piece.pieceType == PieceType.Rook then
      if move.from == Square(0, 0) then r = r.copy(whiteQueenside = false)
      if move.from == Square(7, 0) then r = r.copy(whiteKingside = false)
      if move.from == Square(0, 7) then r = r.copy(blackQueenside = false)
      if move.from == Square(7, 7) then r = r.copy(blackKingside = false)
    // also revoke if rook is captured
    if move.to == Square(0, 0) then r = r.copy(whiteQueenside = false)
    if move.to == Square(7, 0) then r = r.copy(whiteKingside = false)
    if move.to == Square(0, 7) then r = r.copy(blackQueenside = false)
    if move.to == Square(7, 7) then r = r.copy(blackKingside = false)
    r

  private def isInsufficientMaterial(board: Board): Boolean =
    val pieces = allSquares.flatMap(board.get)
    val nonKings = pieces.filterNot(_.pieceType == PieceType.King)
    nonKings.isEmpty ||
    (nonKings.size == 1 && nonKings.head.pieceType == PieceType.Bishop) ||
    (nonKings.size == 1 && nonKings.head.pieceType == PieceType.Knight)

  private def parseSquare(s: String): Option[Square] =
    if s.length != 2 then None
    else
      val file = s(0) - 'a'
      val rank = s(1) - '1'
      val sq = Square(file, rank)
      if sq.isValid then Some(sq) else None

  private def charToPiece(c: Char): Piece =
    val color = if c.isUpper then Color.White else Color.Black
    val pt = c.toLower match
      case 'k' => PieceType.King
      case 'q' => PieceType.Queen
      case 'r' => PieceType.Rook
      case 'b' => PieceType.Bishop
      case 'n' => PieceType.Knight
      case 'p' => PieceType.Pawn
      case _   => PieceType.Pawn
    Piece(pt, color)

  private def pieceToChar(p: Piece): Char =
    val c = p.pieceType match
      case PieceType.King   => 'k'
      case PieceType.Queen  => 'q'
      case PieceType.Rook   => 'r'
      case PieceType.Bishop => 'b'
      case PieceType.Knight => 'n'
      case PieceType.Pawn   => 'p'
    if p.color == Color.White then c.toUpper else c

  private val allSquares: Vector[Square] =
    (for r <- 0 until 8; f <- 0 until 8 yield Square(f, r)).toVector
