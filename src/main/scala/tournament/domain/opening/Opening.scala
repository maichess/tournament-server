package tournament.domain.opening

/** A named starting position: a stable `key`, a human-readable `name`, and the
  * FEN to seed games with. Used both for the built-in catalog and for custom
  * user-registered positions.
  */
final case class Opening(key: String, name: String, fen: String)
