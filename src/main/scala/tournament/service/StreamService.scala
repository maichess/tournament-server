package tournament.service

import zio.*
import zio.stream.*
import tournament.domain.model.{TournamentId, GameId}
import tournament.domain.event.{TournamentEvent, GameEvent}

trait StreamService:
  def subscribeTournament(id: TournamentId): UIO[ZStream[Any, Nothing, TournamentEvent]]
  def publishTournament(id: TournamentId, event: TournamentEvent): UIO[Unit]
  def subscribeGame(id: GameId): UIO[ZStream[Any, Nothing, GameEvent]]
  def publishGame(id: GameId, event: GameEvent): UIO[Unit]

object StreamService:
  def subscribeTournament(id: TournamentId): URIO[StreamService, ZStream[Any, Nothing, TournamentEvent]] =
    ZIO.serviceWithZIO(_.subscribeTournament(id))
  def publishTournament(id: TournamentId, event: TournamentEvent): URIO[StreamService, Unit] =
    ZIO.serviceWithZIO(_.publishTournament(id, event))
  def subscribeGame(id: GameId): URIO[StreamService, ZStream[Any, Nothing, GameEvent]] =
    ZIO.serviceWithZIO(_.subscribeGame(id))
  def publishGame(id: GameId, event: GameEvent): URIO[StreamService, Unit] =
    ZIO.serviceWithZIO(_.publishGame(id, event))

final class StreamServiceLive(
  tournamentQueues: Ref[Map[TournamentId, Vector[Queue[TournamentEvent]]]],
  gameQueues: Ref[Map[GameId, Vector[Queue[GameEvent]]]],
) extends StreamService:

  override def subscribeTournament(id: TournamentId): UIO[ZStream[Any, Nothing, TournamentEvent]] =
    for
      queue <- Queue.unbounded[TournamentEvent]
      _     <- tournamentQueues.update(m => m.updated(id, m.getOrElse(id, Vector.empty) :+ queue))
    yield ZStream.fromQueue(queue)

  override def publishTournament(id: TournamentId, event: TournamentEvent): UIO[Unit] =
    tournamentQueues.get.flatMap: m =>
      ZIO.foreachDiscard(m.getOrElse(id, Vector.empty))(_.offer(event))

  override def subscribeGame(id: GameId): UIO[ZStream[Any, Nothing, GameEvent]] =
    for
      queue <- Queue.unbounded[GameEvent]
      _     <- gameQueues.update(m => m.updated(id, m.getOrElse(id, Vector.empty) :+ queue))
    yield ZStream.fromQueue(queue)

  override def publishGame(id: GameId, event: GameEvent): UIO[Unit] =
    gameQueues.get.flatMap: m =>
      ZIO.foreachDiscard(m.getOrElse(id, Vector.empty))(_.offer(event))

object StreamServiceLive:
  val layer: ULayer[StreamService] =
    ZLayer:
      for
        tq <- Ref.make(Map.empty[TournamentId, Vector[Queue[TournamentEvent]]])
        gq <- Ref.make(Map.empty[GameId, Vector[Queue[GameEvent]]])
      yield StreamServiceLive(tq, gq)
