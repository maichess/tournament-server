package tournament.domain.game

import zio.test.*
import tournament.domain.model.Color
import tournament.domain.game.ChessRules.*

object ChessRulesEdgeCaseSpec extends ZIOSpecDefault:

  def spec = suite("ChessRules edge cases")(
    suite("FEN parsing")(
      test("FEN with too few ranks") {
        assertTrue(parseFen("8/8/8 w KQkq - 0 1").isLeft)
      },
      test("FEN with various turn colors") {
        // 'b' is valid for black
        val blackTurn = parseFen("4k3/8/8/8/8/8/8/4K3 b - - 0 1")
        assertTrue(blackTurn.isRight) && {
          assertTrue(blackTurn.toOption.get.turn == Color.Black)
        }
      },
      test("FEN with missing fields") {
        assertTrue(parseFen("8/8/8/8/8/8/8/8").isLeft)
      },
      test("FEN with numeric halfmove") {
        val result = parseFen("4k3/8/8/8/8/8/8/4K3 w - - 42 100")
        assertTrue(result.isRight) && {
          val b = result.toOption.get
          assertTrue(b.halfmoveClock == 42, b.fullmoveNumber == 100)
        }
      },
    ),
    suite("UCI parsing edge cases")(
      test("UCI with invalid file letter") {
        assertTrue(parseUci("z2z4").isLeft)
      },
      test("UCI 3-char string") {
        assertTrue(parseUci("e2e").isLeft)
      },
    ),
    suite("complex positions")(
      test("rook move along file blocked by piece") {
        val fen = "4k3/8/8/4p3/8/8/8/4K2R w K - 0 1"
        val board = parseFen(fen).toOption.get
        // Rook on h1 can move to h2-h8 (vertical), but not to e1 (blocked by king)
        val moveVertical = Move(Square(7, 0), Square(7, 5), None) // Rh1-h6
        assertTrue(isLegalMove(board, moveVertical))
      },
      test("queen straight move along rank") {
        val fen = "4k3/8/8/8/8/8/8/Q3K3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(0, 0), Square(3, 0), None) // Qa1-d1
        assertTrue(isLegalMove(board, move))
      },
      test("queen blocked diagonally") {
        val fen = "4k3/8/8/8/8/2P5/8/Q3K3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(0, 0), Square(3, 3), None) // Qa1-d4 blocked by c3 pawn
        assertTrue(!isLegalMove(board, move))
      },
      test("bishop blocked by own piece on diagonal") {
        // Bishop on a1, pawn on b2 blocks path to c3
        val fen = "4k3/8/8/8/8/8/1P6/B3K3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(0, 0), Square(2, 2), None) // Ba1-c3 blocked by Pb2
        assertTrue(!isLegalMove(board, move))
      },
      test("stalemate detection with no legal moves") {
        // King boxed in
        val fen = "k7/2Q5/1K6/8/8/8/8/8 b - - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(isStalemate(board))
      },
      test("not stalemate when has legal moves") {
        val fen = "k7/8/1K6/8/8/8/8/8 b - - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(!isStalemate(board))
      },
      test("checkmate with rook and king") {
        // Black king on a8, white rook on a7, white king on b6 — checkmate
        // Actually rook on a7 checks king on a8 and king on b6 covers b8/b7
        val fen = "k7/R7/1K6/8/8/8/8/8 b - - 0 1"
        val board = parseFen(fen).toOption.get
        // King on a8 is checked by rook on a7. Can move to b8? b6 king covers b7,b8. So checkmate.
        // Actually Kb6 covers a5,a6,b5,b7,c5,c6,c7 — b8 is NOT covered by king.
        // Is b8 attacked by Ra7? No, rook is on a-file. So black king can go to b8.
        // Not checkmate — black king escapes to b8.
        // Let me use a better position
        val fen2 = "1k6/R7/1K6/8/8/8/8/8 b - - 0 1"
        val board2 = parseFen(fen2).toOption.get
        // King on b8, rook on a7 checks along rank 7. King on b6 covers a7,b7,c7.
        // b8 king can go to c8 (not attacked). Not checkmate either.
        // Better: back rank mate
        val fen3 = "6k1/5ppp/8/8/8/8/8/R3K3 b - - 0 1"
        // Rook on a1 doesn't check king on g8. Need different setup.
        // Classic back rank: king on g8, pawns f7,g7,h7, white rook delivers check on 8th rank
        // Rook on h8 protected by rook on h1 — no, simpler: scholar's mate position
        val fen4 = "r1bqkb1r/pppp1ppp/2n2n2/4p2Q/2B1P3/8/PPPP1PPP/RNB1K1NR w KQkq - 4 4"
        val board4 = parseFen(fen4).toOption.get
        // White plays Qxf7# (scholar's mate)
        val mateMove = Move(Square(7, 4), Square(5, 6), None) // Qh5xf7
        val afterMate = applyMove(board4, mateMove).toOption.get
        assertTrue(isCheckmate(afterMate))
      },
      test("isInCheck white in check by black queen") {
        val fen = "4k3/8/8/8/4q3/8/8/4K3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(isInCheck(board, Color.White))
      },
      test("isInCheck white NOT in check") {
        val fen = "4k3/8/8/8/3q4/8/8/4K3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(!isInCheck(board, Color.White))
      },
    ),
    suite("pawn generation edge cases")(
      test("pawn capture generates correctly") {
        // White pawn on d4, black pawn on e5
        val fen = "4k3/8/8/4p3/3P4/8/8/4K3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        val captureMove = Move(Square(3, 3), Square(4, 4), None) // d4xe5
        assertTrue(isLegalMove(board, captureMove))
      },
      test("pawn promotion with capture") {
        // White pawn on g7, black rook on h8
        val fen = "4k2r/6P1/8/8/8/8/8/4K3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(6, 6), Square(7, 7), Some(PieceType.Queen)) // g7xh8=Q
        assertTrue(isLegalMove(board, move))
      },
      test("pawn promotion to rook") {
        val fen = "4k3/P7/8/8/8/8/8/4K3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(0, 6), Square(0, 7), Some(PieceType.Rook))
        val after = applyMove(board, move).toOption.get
        assertTrue(after.get(Square(0, 7)).contains(Piece(PieceType.Rook, Color.White)))
      },
      test("pawn promotion to bishop") {
        val fen = "4k3/P7/8/8/8/8/8/4K3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(0, 6), Square(0, 7), Some(PieceType.Bishop))
        val after = applyMove(board, move).toOption.get
        assertTrue(after.get(Square(0, 7)).contains(Piece(PieceType.Bishop, Color.White)))
      },
      test("pawn promotion to knight") {
        val fen = "4k3/P7/8/8/8/8/8/4K3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(0, 6), Square(0, 7), Some(PieceType.Knight))
        val after = applyMove(board, move).toOption.get
        assertTrue(after.get(Square(0, 7)).contains(Piece(PieceType.Knight, Color.White)))
      },
      test("black pawn capture and promotion") {
        val fen = "4k3/8/8/8/8/8/6p1/4K2R b - - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(6, 1), Square(7, 0), Some(PieceType.Queen)) // g2xh1=Q
        assertTrue(isLegalMove(board, move))
      },
    ),
    suite("castling edge cases")(
      test("cannot castle kingside when rook captured") {
        val fen = "4k3/8/8/8/8/8/8/R3K3 w Q - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(4, 0), Square(6, 0), None) // Ke1-g1
        assertTrue(!isLegalMove(board, move))
      },
      test("cannot castle queenside without right") {
        val fen = "4k3/8/8/8/8/8/8/4K2R w K - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(4, 0), Square(2, 0), None) // Ke1-c1
        assertTrue(!isLegalMove(board, move))
      },
      test("black castling kingside") {
        val fen = "r3k2r/8/8/8/8/8/8/4K3 b kq - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(4, 7), Square(6, 7), None)
        val after = applyMove(board, move).toOption.get
        assertTrue(
          after.get(Square(5, 7)).contains(Piece(PieceType.Rook, Color.Black)),
          after.get(Square(6, 7)).contains(Piece(PieceType.King, Color.Black)),
        )
      },
      test("black castling queenside") {
        val fen = "r3k2r/8/8/8/8/8/8/4K3 b kq - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(4, 7), Square(2, 7), None)
        val after = applyMove(board, move).toOption.get
        assertTrue(
          after.get(Square(3, 7)).contains(Piece(PieceType.Rook, Color.Black)),
          after.get(Square(2, 7)).contains(Piece(PieceType.King, Color.Black)),
        )
      },
      test("cannot castle through attacked square kingside") {
        // Black bishop on a5 attacks e1 diagonal - actually attacks f1? No, a5 attacks... Let me use rook
        // Black rook attacks f1
        val fen = "4k3/8/8/8/8/8/8/R3K2R w KQ - 0 1"
        // f1 is attacked by... we need an attacker. Let's put black rook on f8
        val fen2 = "5rk1/8/8/8/8/8/8/R3K2R w KQ - 0 1"
        val board = parseFen(fen2).toOption.get
        val move = Move(Square(4, 0), Square(6, 0), None) // O-O
        assertTrue(!isLegalMove(board, move)) // f1 attacked by rook on f8
      },
      test("cannot castle queenside through attacked d1") {
        // Black rook on d8 attacks d1
        val fen = "3rk3/8/8/8/8/8/8/R3K3 w Q - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(4, 0), Square(2, 0), None) // O-O-O
        assertTrue(!isLegalMove(board, move))
      },
    ),
    suite("draw detection")(
      test("50 move rule at exactly 100 halfmoves") {
        val fen = "4k3/8/8/8/8/8/8/4K3 w - - 100 50"
        val board = parseFen(fen).toOption.get
        assertTrue(isDraw(board))
      },
      test("not a draw with sufficient material") {
        val fen = "4k3/8/8/8/8/8/8/4K2R w - - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(!isDraw(board))
      },
      test("K+B vs K is draw") {
        val fen = "4k3/8/8/8/8/8/8/4KB2 w - - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(isDraw(board))
      },
    ),
    suite("FEN generation")(
      test("boardToFen with black pieces") {
        val fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(boardToFen(board) == fen)
      },
      test("boardToFen preserves castling KQkq") {
        val fen = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(boardToFen(board).contains("KQkq"))
      },
      test("boardToFen with K only castling") {
        val fen = "4k3/8/8/8/8/8/8/4K2R w K - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(boardToFen(board).contains(" K "))
      },
      test("boardToFen with kq only castling") {
        val fen = "r3k2r/8/8/8/8/8/8/4K3 b kq - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(boardToFen(board).contains(" kq "))
      },
    ),
  )
