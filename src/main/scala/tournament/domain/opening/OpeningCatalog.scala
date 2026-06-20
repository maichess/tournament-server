package tournament.domain.opening

/** Built-in library of named starting positions. FENs describe the position
  * after the opening's defining moves, with the side to move and full-move
  * counter set accordingly.
  */
object OpeningCatalog:

  val standardFen: String = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

  val all: Vector[Opening] = Vector(
    Opening("standard", "Standard", standardFen),
    // 1. e4 e5 2. Nc3 — Vienna Game
    Opening("vienna", "Vienna Opening",
      "rnbqkbnr/pppp1ppp/8/4p3/4P3/2N5/PPPP1PPP/R1BQKBNR b KQkq - 1 2"),
    // 1. e4 e5 2. Nf3 Nc6 3. Bc4 — Italian Game
    Opening("italian", "Italian Game",
      "r1bqkbnr/pppp1ppp/2n5/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3"),
    // 1. e4 e5 2. Nf3 Nc6 3. Bb5 — Ruy Lopez
    Opening("ruyLopez", "Ruy Lopez",
      "r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3"),
    // 1. e4 c5 — Sicilian Defence
    Opening("sicilian", "Sicilian Defence",
      "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"),
    // 1. e4 e6 — French Defence
    Opening("french", "French Defence",
      "rnbqkbnr/pppp1ppp/4p3/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"),
    // 1. d4 Nf6 2. c4 g6 3. Nc3 d5 — Grünfeld Defence
    Opening("queensGambit", "Queen's Gambit",
      "rnbqkbnr/ppp1pppp/8/3p4/2PP4/8/PP2PPPP/RNBQKBNR b KQkq - 0 2"),
    // 1. d4 Nf6 2. c4 e6 3. Nc3 Bb4 — Nimzo-Indian Defence
    Opening("nimzoIndian", "Nimzo-Indian Defence",
      "rnbqk2r/pppp1ppp/4pn2/8/1bPP4/2N5/PP2PPPP/R1BQKBNR w KQkq - 2 4"),
  )

  def byKey(key: String): Option[Opening] =
    all.find(_.key == key)
