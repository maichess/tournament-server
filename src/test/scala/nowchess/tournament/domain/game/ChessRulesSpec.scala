package nowchess.tournament.domain.game

import zio.test.*
import nowchess.tournament.domain.model.Color
import nowchess.tournament.domain.game.ChessRules.*

object ChessRulesSpec extends ZIOSpecDefault:

  private val startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

  def spec = suite("ChessRules")(
    suite("parseFen")(
      test("parses standard starting position") {
        val result = parseFen(startFen)
        assertTrue(result.isRight) && {
          val board = result.toOption.get
          assertTrue(board.turn == Color.White) &&
          assertTrue(board.castling.whiteKingside) &&
          assertTrue(board.castling.blackQueenside) &&
          assertTrue(board.get(Square(0, 0)).contains(Piece(PieceType.Rook, Color.White))) &&
          assertTrue(board.get(Square(4, 0)).contains(Piece(PieceType.King, Color.White))) &&
          assertTrue(board.get(Square(4, 7)).contains(Piece(PieceType.King, Color.Black)))
        }
      },
      test("rejects invalid FEN") {
        assertTrue(parseFen("invalid").isLeft) &&
        assertTrue(parseFen("8/8/8 w KQkq -").isLeft)
      },
    ),
    suite("boardToFen")(
      test("roundtrips standard position") {
        val board = parseFen(startFen).toOption.get
        val fen = boardToFen(board)
        assertTrue(fen == startFen)
      },
    ),
    suite("parseUci")(
      test("parses regular move") {
        val move = parseUci("e2e4")
        assertTrue(move == Right(Move(Square(4, 1), Square(4, 3), None)))
      },
      test("parses promotion") {
        val move = parseUci("e7e8q")
        assertTrue(move == Right(Move(Square(4, 6), Square(4, 7), Some(PieceType.Queen))))
      },
      test("rejects invalid UCI") {
        assertTrue(parseUci("xx").isLeft) &&
        assertTrue(parseUci("").isLeft)
      },
    ),
    suite("isLegalMove")(
      test("e2e4 is legal from start") {
        val board = parseFen(startFen).toOption.get
        val move = Move(Square(4, 1), Square(4, 3), None)
        assertTrue(isLegalMove(board, move))
      },
      test("e2e5 is illegal from start") {
        val board = parseFen(startFen).toOption.get
        val move = Move(Square(4, 1), Square(4, 4), None)
        assertTrue(!isLegalMove(board, move))
      },
      test("cannot move opponent piece") {
        val board = parseFen(startFen).toOption.get
        val move = Move(Square(4, 6), Square(4, 5), None)
        assertTrue(!isLegalMove(board, move))
      },
      test("cannot move from empty square") {
        val board = parseFen(startFen).toOption.get
        val move = Move(Square(4, 3), Square(4, 4), None)
        assertTrue(!isLegalMove(board, move))
      },
    ),
    suite("applyMove")(
      test("applies e2e4") {
        val board = parseFen(startFen).toOption.get
        val move = Move(Square(4, 1), Square(4, 3), None)
        val result = applyMove(board, move)
        assertTrue(result.isRight) && {
          val after = result.toOption.get
          assertTrue(after.turn == Color.Black) &&
          assertTrue(after.get(Square(4, 3)).contains(Piece(PieceType.Pawn, Color.White))) &&
          assertTrue(after.get(Square(4, 1)).isEmpty) &&
          assertTrue(after.enPassant.contains(Square(4, 2)))
        }
      },
      test("rejects illegal move") {
        val board = parseFen(startFen).toOption.get
        val move = Move(Square(4, 1), Square(4, 4), None)
        assertTrue(applyMove(board, move).isLeft)
      },
    ),
    suite("checkmate and stalemate")(
      test("detects checkmate (fool's mate)") {
        val board = parseFen(startFen).toOption.get
        val moves = Vector("f2f3", "e7e5", "g2g4", "d8h4")
        val finalBoard = moves.foldLeft(board): (b, uci) =>
          val m = parseUci(uci).toOption.get
          applyMove(b, m).toOption.get
        assertTrue(isCheckmate(finalBoard)) &&
        assertTrue(!isStalemate(finalBoard))
      },
      test("detects stalemate") {
        // Black king on a8, white queen on b6, white king on c8 — black to move, stalemate
        val fen = "k7/8/1Q6/8/8/8/8/2K5 b - - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(isStalemate(board)) &&
        assertTrue(!isCheckmate(board))
      },
    ),
    suite("isInCheck")(
      test("detects check") {
        // White queen checks black king
        val fen = "k7/8/8/8/8/8/8/4QK2 b - - 0 1"
        val board = parseFen(fen).toOption.get
        // The queen on e1 doesn't directly check k on a8 through diagonal/file
        // Let me use a simpler case
        val fen2 = "k7/1Q6/8/8/8/8/8/4K3 b - - 0 1"
        val board2 = parseFen(fen2).toOption.get
        assertTrue(isInCheck(board2, Color.Black))
      },
      test("no check in standard position") {
        val board = parseFen(startFen).toOption.get
        assertTrue(!isInCheck(board, Color.White)) &&
        assertTrue(!isInCheck(board, Color.Black))
      },
    ),
    suite("isDraw")(
      test("insufficient material K vs K") {
        val fen = "k7/8/8/8/8/8/8/K7 w - - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(isDraw(board))
      },
      test("insufficient material K+B vs K") {
        val fen = "k7/8/8/8/8/8/B7/K7 w - - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(isDraw(board))
      },
      test("insufficient material K+N vs K") {
        val fen = "k7/8/8/8/8/8/N7/K7 w - - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(isDraw(board))
      },
      test("50 move rule") {
        val fen = "k7/8/8/8/8/8/8/4K2R w - - 100 50"
        val board = parseFen(fen).toOption.get
        assertTrue(isDraw(board))
      },
    ),
    suite("castling")(
      test("kingside castling is legal when path is clear") {
        val fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(4, 0), Square(6, 0), None)
        assertTrue(isLegalMove(board, move))
      },
      test("queenside castling is legal when path is clear") {
        val fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(4, 0), Square(2, 0), None)
        assertTrue(isLegalMove(board, move))
      },
      test("castling moves rook correctly") {
        val fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(4, 0), Square(6, 0), None)
        val after = applyMove(board, move).toOption.get
        assertTrue(after.get(Square(5, 0)).contains(Piece(PieceType.Rook, Color.White))) &&
        assertTrue(after.get(Square(6, 0)).contains(Piece(PieceType.King, Color.White))) &&
        assertTrue(after.get(Square(7, 0)).isEmpty)
      },
      test("cannot castle through check") {
        // Enemy rook attacking f1
        val fen = "r3k2r/pppppppp/8/8/8/8/PPPPP1PP/R3K2R b KQkq - 0 1"
        // Actually let's set up white castling blocked by black rook on f-file
        val fen2 = "4k3/8/8/8/8/8/8/R3K2r w Q - 0 1"
        val board = parseFen(fen2).toOption.get
        // Black rook on h1 attacks along rank 1, so f1 is not attacked
        // Let me use a bishop attacking f1
        val fen3 = "4k3/8/8/8/8/4b3/8/R3K2R w KQ - 0 1"
        val board3 = parseFen(fen3).toOption.get
        val move = Move(Square(4, 0), Square(6, 0), None)
        assertTrue(!isLegalMove(board3, move))
      },
    ),
    suite("en passant")(
      test("en passant capture is legal") {
        val fen = "rnbqkbnr/pppp1ppp/8/4pP2/8/8/PPPPP1PP/RNBQKBNR w KQkq e6 0 3"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(5, 4), Square(4, 5), None)
        assertTrue(isLegalMove(board, move))
      },
      test("en passant removes captured pawn") {
        val fen = "rnbqkbnr/pppp1ppp/8/4pP2/8/8/PPPPP1PP/RNBQKBNR w KQkq e6 0 3"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(5, 4), Square(4, 5), None)
        val after = applyMove(board, move).toOption.get
        assertTrue(after.get(Square(4, 4)).isEmpty) &&
        assertTrue(after.get(Square(4, 5)).contains(Piece(PieceType.Pawn, Color.White)))
      },
    ),
    suite("promotion")(
      test("pawn promotion to queen") {
        val fen = "4k3/P7/8/8/8/8/8/4K3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(0, 6), Square(0, 7), Some(PieceType.Queen))
        val after = applyMove(board, move).toOption.get
        assertTrue(after.get(Square(0, 7)).contains(Piece(PieceType.Queen, Color.White)))
      },
    ),
    suite("knight moves")(
      test("knight L-shape is legal") {
        val fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(1, 0), Square(2, 2), None) // Nb1-c3
        assertTrue(isLegalMove(board, move))
      },
    ),
    suite("rook moves")(
      test("rook move along file") {
        val fen = "4k3/8/8/8/8/8/8/R3K3 w Q - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(0, 0), Square(0, 5), None) // Ra1-a6
        assertTrue(isLegalMove(board, move))
      },
      test("rook move along rank") {
        val fen = "4k3/8/8/8/8/8/8/R3K3 w Q - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(0, 0), Square(3, 0), None) // Ra1-d1
        assertTrue(isLegalMove(board, move))
      },
      test("rook blocked by own piece") {
        val fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(0, 0), Square(0, 2), None) // Ra1-a3 blocked by pawn
        assertTrue(!isLegalMove(board, move))
      },
    ),
    suite("queen moves")(
      test("queen straight move") {
        val fen = "4k3/8/8/8/8/8/8/3QK3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(3, 0), Square(3, 5), None) // Qd1-d6
        assertTrue(isLegalMove(board, move))
      },
      test("queen diagonal move") {
        val fen = "4k3/8/8/8/8/8/8/3QK3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(3, 0), Square(0, 3), None) // Qd1-a4
        assertTrue(isLegalMove(board, move))
      },
    ),
    suite("bishop moves")(
      test("bishop diagonal is legal") {
        val fen = "4k3/8/8/8/8/8/8/2B1K3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(2, 0), Square(5, 3), None) // Bc1-f4
        assertTrue(isLegalMove(board, move))
      },
    ),
    suite("castling rights updates")(
      test("moving rook from a1 removes white queenside castling") {
        // No pawns blocking the rook
        val fen = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(0, 0), Square(0, 3), None) // Ra1-a4
        val after = applyMove(board, move).toOption.get
        assertTrue(!after.castling.whiteQueenside) &&
        assertTrue(after.castling.whiteKingside)
      },
      test("moving rook from h1 removes white kingside castling") {
        val fen = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(7, 0), Square(7, 3), None) // Rh1-h4
        val after = applyMove(board, move).toOption.get
        assertTrue(!after.castling.whiteKingside) &&
        assertTrue(after.castling.whiteQueenside)
      },
      test("moving black rook from a8 removes black queenside") {
        val fen = "r3k2r/8/8/8/8/8/8/R3K2R b KQkq - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(0, 7), Square(0, 4), None) // ra8-a5
        val after = applyMove(board, move).toOption.get
        assertTrue(!after.castling.blackQueenside) &&
        assertTrue(after.castling.blackKingside)
      },
      test("moving black rook from h8 removes black kingside") {
        val fen = "r3k2r/8/8/8/8/8/8/R3K2R b KQkq - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(7, 7), Square(7, 4), None) // rh8-h5
        val after = applyMove(board, move).toOption.get
        assertTrue(!after.castling.blackKingside) &&
        assertTrue(after.castling.blackQueenside)
      },
      test("capturing rook on a1 revokes white queenside") {
        // Black bishop captures white rook on a1
        val fen = "4k3/8/8/8/8/8/1b6/R3K3 b Q - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(1, 1), Square(0, 0), None) // Bb2xa1
        val after = applyMove(board, move).toOption.get
        assertTrue(!after.castling.whiteQueenside)
      },
      test("capturing rook on h1 revokes white kingside") {
        val fen = "4k3/8/8/8/8/8/6b1/4K2R b K - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(6, 1), Square(7, 0), None) // Bg2xh1
        val after = applyMove(board, move).toOption.get
        assertTrue(!after.castling.whiteKingside)
      },
      test("capturing rook on a8 revokes black queenside") {
        val fen = "r3k3/1B6/8/8/8/8/8/4K3 w q - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(1, 6), Square(0, 7), None) // Bb7xa8
        val after = applyMove(board, move).toOption.get
        assertTrue(!after.castling.blackQueenside)
      },
      test("capturing rook on h8 revokes black kingside") {
        val fen = "4k2r/6B1/8/8/8/8/8/4K3 w k - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(6, 6), Square(7, 7), None) // Bg7xh8
        val after = applyMove(board, move).toOption.get
        assertTrue(!after.castling.blackKingside)
      },
      test("black king move removes both castling rights") {
        val fen = "r3k2r/8/8/8/8/8/8/R3K2R b KQkq - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(4, 7), Square(4, 6), None) // Ke8-e7
        val after = applyMove(board, move).toOption.get
        assertTrue(!after.castling.blackKingside) &&
        assertTrue(!after.castling.blackQueenside)
      },
      test("queenside castling moves rook") {
        val fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(4, 0), Square(2, 0), None) // O-O-O
        val after = applyMove(board, move).toOption.get
        assertTrue(after.get(Square(3, 0)).contains(Piece(PieceType.Rook, Color.White))) &&
        assertTrue(after.get(Square(0, 0)).isEmpty)
      },
    ),
    suite("pawn moves")(
      test("black pawn moves forward") {
        val fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(4, 6), Square(4, 5), None) // e7-e6
        assertTrue(isLegalMove(board, move))
      },
      test("black pawn double advance") {
        val fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(4, 6), Square(4, 4), None) // e7-e5
        assertTrue(isLegalMove(board, move))
      },
      test("pawn capture diagonal") {
        val fen = "4k3/8/8/3p4/4P3/8/8/4K3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(4, 3), Square(3, 4), None) // e4xd5
        assertTrue(isLegalMove(board, move))
      },
      test("black en passant") {
        val fen = "4k3/8/8/8/3pP3/8/8/4K3 b - e3 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(3, 3), Square(4, 2), None) // d4xe3 en passant
        assertTrue(isLegalMove(board, move))
      },
      test("black pawn promotion") {
        val fen = "4k3/8/8/8/8/8/1p6/4K3 b - - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(1, 1), Square(1, 0), Some(PieceType.Queen))
        val after = applyMove(board, move).toOption.get
        assertTrue(after.get(Square(1, 0)).contains(Piece(PieceType.Queen, Color.Black)))
      },
    ),
    suite("boardToFen edge cases")(
      test("FEN with empty ranks") {
        val fen = "4k3/8/8/8/8/8/8/4K3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        val result = boardToFen(board)
        assertTrue(result == fen)
      },
      test("FEN with en passant") {
        val fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
        val board = parseFen(fen).toOption.get
        val result = boardToFen(board)
        assertTrue(result == fen)
      },
      test("FEN with partial castling") {
        val fen = "4k3/8/8/8/8/8/8/R3K3 w Q - 0 1"
        val board = parseFen(fen).toOption.get
        val result = boardToFen(board)
        assertTrue(result == fen)
      },
      test("FEN with no castling") {
        val fen = "4k3/8/8/8/8/8/8/4K3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        val result = boardToFen(board)
        assertTrue(result == fen)
      },
      test("FEN black to move") {
        val fen = "4k3/8/8/8/8/8/8/4K3 b - - 5 10"
        val board = parseFen(fen).toOption.get
        val result = boardToFen(board)
        assertTrue(result == fen)
      },
    ),
    suite("king moves")(
      test("king cannot castle when in check") {
        val fen = "4k3/8/8/8/4r3/8/8/R3K2R w KQ - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(4, 0), Square(6, 0), None)
        assertTrue(!isLegalMove(board, move))
      },
      test("black kingside castling") {
        val fen = "r3k2r/pppppppp/8/8/4P3/8/PPPP1PPP/R3K2R b kq - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(4, 7), Square(6, 7), None)
        assertTrue(isLegalMove(board, move))
      },
      test("black queenside castling") {
        val fen = "r3k2r/pppppppp/8/8/4P3/8/PPPP1PPP/R3K2R b kq - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(4, 7), Square(2, 7), None)
        assertTrue(isLegalMove(board, move))
      },
      test("king single square move") {
        val fen = "4k3/8/8/8/8/8/8/4K3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(4, 0), Square(5, 1), None) // Ke1-f2
        assertTrue(isLegalMove(board, move))
      },
      test("king cannot move into check") {
        val fen = "4k3/8/8/8/8/8/8/r3K3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(4, 0), Square(3, 0), None) // Ke1-d1 into rook attack
        assertTrue(!isLegalMove(board, move))
      },
    ),
    suite("isInCheck edge cases")(
      test("check by rook") {
        val fen = "4k3/8/8/8/8/8/8/r3K3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(isInCheck(board, Color.White))
      },
      test("check by bishop") {
        // Bishop on b4 checks king on e1 via diagonal
        val fen = "4k3/8/8/8/1b6/8/8/4K3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(isInCheck(board, Color.White))
      },
      test("check by knight") {
        val fen = "4k3/8/8/8/8/3n4/8/4K3 w - - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(isInCheck(board, Color.White))
      },
    ),
    suite("parseFen edge cases")(
      test("FEN with black to move") {
        val fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(board.turn == Color.Black)
      },
      test("FEN with only kingside castling") {
        val fen = "4k3/8/8/8/8/8/8/4K2R w K - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(board.castling.whiteKingside) &&
        assertTrue(!board.castling.whiteQueenside)
      },
      test("FEN with only queenside castling") {
        val fen = "4k3/8/8/8/8/8/8/R3K3 w Q - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(!board.castling.whiteKingside) &&
        assertTrue(board.castling.whiteQueenside)
      },
      test("FEN with black castling only") {
        val fen = "r3k2r/8/8/8/8/8/8/4K3 b kq - 0 1"
        val board = parseFen(fen).toOption.get
        assertTrue(board.castling.blackKingside) &&
        assertTrue(board.castling.blackQueenside) &&
        assertTrue(!board.castling.whiteKingside)
      },
    ),
    suite("move validation edge cases")(
      test("cannot move to own piece square") {
        val fen = "4k3/8/8/8/8/8/8/R3K3 w Q - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(0, 0), Square(4, 0), None) // Ra1 to e1 (own king)
        assertTrue(!isLegalMove(board, move))
      },
      test("invalid square move is illegal") {
        val board = parseFen(startFen).toOption.get
        val move = Move(Square(4, 1), Square(4, 1), None) // same square
        assertTrue(!isLegalMove(board, move))
      },
    ),
    suite("parseUci edge cases")(
      test("promotion to rook") {
        val move = parseUci("e7e8r")
        assertTrue(move.isRight) && {
          val m = move.toOption.get
          assertTrue(m.promotion.contains(PieceType.Rook))
        }
      },
      test("promotion to bishop") {
        val move = parseUci("e7e8b")
        assertTrue(move.isRight) && {
          val m = move.toOption.get
          assertTrue(m.promotion.contains(PieceType.Bishop))
        }
      },
      test("promotion to knight") {
        val move = parseUci("e7e8n")
        assertTrue(move.isRight) && {
          val m = move.toOption.get
          assertTrue(m.promotion.contains(PieceType.Knight))
        }
      },
    ),
    suite("halfmove and fullmove clock")(
      test("halfmove resets on capture") {
        val fen = "4k3/8/8/3p4/4P3/8/8/4K3 w - - 10 5"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(4, 3), Square(3, 4), None) // e4xd5
        val after = applyMove(board, move).toOption.get
        assertTrue(after.halfmoveClock == 0)
      },
      test("fullmove increments after black move") {
        val fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(4, 6), Square(4, 4), None) // e7-e5
        val after = applyMove(board, move).toOption.get
        assertTrue(after.fullmoveNumber == 2)
      },
      test("halfmove increments on quiet move") {
        val fen = "4k3/8/8/8/8/8/8/4K3 w - - 5 10"
        val board = parseFen(fen).toOption.get
        val move = Move(Square(4, 0), Square(5, 0), None) // Ke1-f1
        val after = applyMove(board, move).toOption.get
        assertTrue(after.halfmoveClock == 6)
      },
    ),
  )
