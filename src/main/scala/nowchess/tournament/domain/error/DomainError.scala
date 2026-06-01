package nowchess.tournament.domain.error

sealed abstract class DomainError(val message: String) extends Exception(message)

object DomainError:
  final case class NotFound(msg: String)     extends DomainError(msg)
  final case class Conflict(msg: String)     extends DomainError(msg)
  final case class Forbidden(msg: String)    extends DomainError(msg)
  final case class BadRequest(msg: String)   extends DomainError(msg)
  final case class Unauthorized(msg: String) extends DomainError(msg)
