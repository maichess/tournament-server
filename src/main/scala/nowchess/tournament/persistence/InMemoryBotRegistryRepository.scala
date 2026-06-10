package nowchess.tournament.persistence

import zio.*
import nowchess.tournament.domain.model.{BotId, RegisteredBot}

final class InMemoryBotRegistryRepository(ref: Ref[Map[BotId, RegisteredBot]]) extends BotRegistryRepository:

  override def save(bot: RegisteredBot): Task[Unit] =
    ref.update(_.updated(bot.id, bot))

  override def get(id: BotId): Task[Option[RegisteredBot]] =
    ref.get.map(_.get(id))

  override def list: Task[Vector[RegisteredBot]] =
    ref.get.map(_.values.toVector)

  override def delete(id: BotId): Task[Unit] =
    ref.update(_ - id)

object InMemoryBotRegistryRepository:
  val layer: ULayer[BotRegistryRepository] =
    ZLayer:
      Ref.make(Map.empty[BotId, RegisteredBot]).map(new InMemoryBotRegistryRepository(_))
