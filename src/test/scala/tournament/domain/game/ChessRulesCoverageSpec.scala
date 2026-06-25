package tournament.domain.game

import zio.test.*
import tournament.domain.model.Color
import tournament.domain.game.ChessRules.*

object ChessRulesCoverageSpec extends ZIOSpecDefault:

  def spec = suite("ChessRules coverage")(
    suite("FEN edge cases")(
      test("FEN with rank length mismatch (too many pieces)") {
        // Rank "PPPPPPPPPP" = 10 squares, not 8
        assertTrue(parseFen("PPPPPPPPPP/8/8/8/8/8/8/4K3 w - - 0 1").isLeft)
      },
      test("FEN with only 4 parts (no halfmove/fullmove)") {
        val result = parseFen("4k3/8/8/8/8/8/8/4K3 w - -")
        assertTrue(result.isRight) && {
          val b = result.toOption.get
          assertTrue(b.halfmoveClock == 0, b.fullmoveNumber == 1)
        }
      },
      test("FEN with 5 parts (no fullmove)") {
        val result = parseFen("4k3/8/8/8/8/8/8/4K3 w - - 10")
        assertTrue(result.isRight) && {
          val b = result.toOption.get
          assertTrue(b.halfmoveClock == 10, b.fullmoveNumber == 1)
        }
      },
      test("FEN with non-numeric halfmove") {
        val result = parseFen("4k3/8/8/8/8/8/8/4K3 w - - abc 1")
        assertTrue(result.isRight) && {
          assertTrue(result.toOption.get.halfmoveClock == 0)
        }
      },
      test("FEN with non-numeric fullmove") {
        val result = parseFen("4k3/8/8/8/8/8/8/4K3 w - - 0 xyz")
        assertTrue(result.isRight) && {
          assertTrue(result.toOption.get.fullmoveNumber == 1)
        }
      },
      test("FEN with unknown piece char uses pawn fallback") {
        // 'x' is not a valid piece char, charToPiece falls back to Pawn
        val result = parseFen("4k3/8/8/8/8/8/8/x3K3 w - - 0 1")
        assertTrue(result.isRight) && {
          val piece = result.toOption.get.get(Square(0, 0))
          assertTrue(piece.exists(_.pieceType == PieceType.Pawn))
        }
      },
    ),
    suite("parseUci edge cases")(
      test("UCI with invalid 'to' square") {
        assertTrue(parseUci("e2z4").isLeft)
      },
      test("UCI with unknown promotion char is rejected") {
        assertTrue(parseUci("e7e8x").isLeft)
      },
      test("uppercase promotion char is accepted") {
        val result = parseUci("e7e8Q")
        assertTrue(result.toOption.flatMap(_.promotion).contains(PieceType.Queen))
      },
      test("each promotion piece parses") {
        assertTrue(parseUci("e7e8q").toOption.flatMap(_.promotion).contains(PieceType.Queen)) &&
        assertTrue(parseUci("e7e8r").toOption.flatMap(_.promotion).contains(PieceType.Rook)) &&
        assertTrue(parseUci("e7e8b").toOption.flatMap(_.promotion).contains(PieceType.Bishop)) &&
        assertTrue(parseUci("e7e8n").toOption.flatMap(_.promotion).contains(PieceType.Knight))
      },
    ),
    suite("promotion legality")(
      test("pawn reaching last rank requires a promotion piece") {
        val board = parseFen("4k3/P7/8/8/8/8/8/4K3 w - - 0 1").toOption.get
        val noPromo = Move(Square(0, 6), Square(0, 7), None)
        val promo   = Move(Square(0, 6), Square(0, 7), Some(PieceType.Queen))
        assertTrue(!isLegalMove(board, noPromo)) &&
        assertTrue(isLegalMove(board, promo))
      },
      test("promotion applied to non-last-rank move is illegal") {
        val board = parseFen("4k3/8/8/8/8/8/4P3/4K3 w - - 0 1").toOption.get
        val bogus = Move(Square(4, 1), Square(4, 3), Some(PieceType.Queen))
        assertTrue(!isLegalMove(board, bogus))
      },
      test("forward promotion produces the promoted piece in the FEN") {
        val board  = parseFen("4k3/P7/8/8/8/8/8/4K3 w - - 0 1").toOption.get
        val move   = parseUci("a7a8q").toOption.get
        val result = applyMove(board, move).map(boardToFen)
        assertTrue(result.exists(_.startsWith("Q3k3/")))
      },
    ),
    suite("Board access")(
      test("Board.get returns None for out-of-bounds square") {
        val board = parseFen("4k3/8/8/8/8/8/8/4K3 w - - 0 1").toOption.get
        assertTrue(board.get(Square(-1, 0)).isEmpty) &&
        assertTrue(board.get(Square(0, 8)).isEmpty)
      },
    ),
    suite("isInCheck edge cases")(
      test("isInCheck returns false when no king present") {
        // Board with no white king
        val fen = "4k3/8/8/8/8/8/8/4Q3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(!isInCheck(board, Color.White))
      },
    ),
    suite("stalemate/checkmate with promotion moves")(
      test("position where pawn forward promotion is a legal move preventing stalemate") {
        // White: king on a1, pawn on h7. Black: king on c1.
        // White to move. Pawn can promote. Not stalemate.
        val fen = "4k3/7P/8/8/8/8/8/K7 w - - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(!isStalemate(board))
      },
      test("position where capture promotion is available") {
        // White: king on a1, pawn on g7. Black: king on e8, rook on h8.
        // White pawn can capture rook and promote.
        val fen = "4k2r/6P1/8/8/8/8/8/K7 w - - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(6, 6), Square(7, 7), Some(PieceType.Queen))
        assertTrue(isLegalMove(board, move))
      },
      test("stalemate check generates castling moves") {
        // White: king on e1 with castling rights, rooks present. Not stalemate.
        val fen = "4k3/8/8/8/8/8/8/R3K2R w KQ - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(!isStalemate(board))
      },
      test("capture promotion on both diagonals for black pawn") {
        // Black pawn on b2, white pieces on a1 and c1. Black to move.
        val fen = "4k3/8/8/8/8/8/1p6/R1B1K3 b - - 0 1"
        val board = parseFen(fen).toOption.get
        // Can capture a1 rook with promotion
        val capLeft = Move(Square(1, 1), Square(0, 0), Some(PieceType.Queen))
        // Can capture c1 bishop with promotion
        val capRight = Move(Square(1, 1), Square(2, 0), Some(PieceType.Queen))
        assertTrue(isLegalMove(board, capLeft)) &&
        assertTrue(isLegalMove(board, capRight))
      },
    ),
    suite("sliding captures in move generation")(
      test("rook captures opponent piece along file") {
        // White rook on a1, black pawn on a5. Rook can capture.
        val fen = "4k3/8/8/p7/8/8/8/R3K3 w Q - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(0, 0), Square(0, 4), None) // Ra1xa5
        assertTrue(isLegalMove(board, move))
      },
      test("bishop captures opponent piece on diagonal") {
        val fen = "4k3/8/8/8/8/8/1p6/B3K3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(0, 0), Square(1, 1), None) // Ba1xb2
        assertTrue(isLegalMove(board, move))
      },
    ),
    suite("isStraightClear edge case")(
      test("same square straight move is rejected") {
        val board = parseFen("4k3/8/8/8/8/8/8/R3K3 w Q - 0 1").toOption.get
        val move = Move(Square(0, 0), Square(0, 0), None) // Ra1-a1
        assertTrue(!isLegalMove(board, move))
      },
    ),
    suite("parseSquare edge case")(
      test("FEN with en passant on invalid square") {
        // Invalid en passant square "z9" - parseSquare returns None
        val result = parseFen("4k3/8/8/8/8/8/8/4K3 w - z9 0 1")
        assertTrue(result.isRight) && {
          assertTrue(result.toOption.get.enPassant.isEmpty)
        }
      },
    ),
    suite("isPseudoLegalKingMove castling branch")(
      test("king cannot castle to invalid square") {
        val board = parseFen("4k3/8/8/8/8/8/8/R3K2R w KQ - 0 1").toOption.get
        // King tries to move to a3 - not normal king move and not castling
        val move = Move(Square(4, 0), Square(0, 2), None)
        assertTrue(!isLegalMove(board, move))
      },
    ),
    suite("hasLegalMoves internal generation coverage")(
      test("checkmate position exercises pawn forward promotion generation") {
        // White king on a1 in check from black pawn on b2. c3 pawn protects b2.
        // b3 pawn protects a2. King has no escape. Pawn on h7 generates promotion moves
        // but none resolve check. hasLegalMoves iterates all white pieces including pawn.
        val fen = "4k1n1/7P/8/8/8/1pp5/pp6/K7 w - - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(isCheckmate(board))
      },
      test("checkmate with pawn non-promotion capture in generation") {
        // Same trapped king. White pawn on c2 can capture b3 (non-promo capture)
        // but that doesn't resolve the check from b2. Exercises non-promo capture path.
        val fen = "4k3/8/8/8/8/1pp5/ppP5/K7 w - - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(isCheckmate(board))
      },
      test("not checkmate when sliding capture resolves check") {
        // White king trapped on a1, in check from b2 pawn (protected by c3).
        // White rook on b1 can capture b2 to resolve check — exercises sliding capture
        // generation in generateSlidingMoves (line 201).
        val fen = "4k3/8/8/8/8/1pp5/pp6/KR6 w - - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(!isCheckmate(board)) // Rxb2 resolves check
      },
      test("castling generation in hasLegalMoves - kingside") {
        // No white pieces before e1 in iteration order. King on e1 with K rights.
        // hasLegalMoves reaches king, generateKingMoves includes castling squares.
        val fen = "4k3/8/8/8/8/8/8/4K2R w K - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(!isStalemate(board)) // has legal moves including castling
      },
      test("castling generation in hasLegalMoves - queenside") {
        // King on e1 with Q rights but no rook on a1 (to avoid short-circuit by rook).
        val fen = "4k3/8/8/8/8/8/8/4K3 w Q - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(!isStalemate(board))
      },
    ),
    suite("parseSquare length check")(
      test("FEN with en passant field of wrong length") {
        val result = parseFen("4k3/8/8/8/8/8/8/4K3 w - abc 0 1")
        assertTrue(result.isRight) && {
          assertTrue(result.toOption.get.enPassant.isEmpty)
        }
      },
    ),
  )
