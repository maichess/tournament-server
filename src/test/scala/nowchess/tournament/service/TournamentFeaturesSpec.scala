package nowchess.tournament.service

import zio.*
import zio.test.*
import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.tournament.*
import nowchess.tournament.domain.game.{GameStatus, Game}
import nowchess.tournament.domain.error.DomainError
import nowchess.tournament.persistence.*

object TournamentFeaturesSpec extends ZIOSpecDefault:

  private val director = UserId("director")
  private val other = UserId("intruder")

  private val layer =
    (InMemoryTournamentRepository.layer ++
     InMemoryGameRepository.layer ++
     StreamServiceLive.layer ++
     (InMemoryOpeningRepository.layer >>> OpeningServiceLive.layer) ++
     ServiceTestLayers.botRegistry) >+>
    (TournamentServiceLive.layer ++ GameServiceLive.layer)

  private def form(
    format: TournamentFormat = TournamentFormat.Swiss,
    nbRounds: Int = 1,
    opening: Option[String] = None,
    bots: Option[String] = None,
    maxConcurrentGames: Option[Int] = None,
    openings: Option[String] = None,
  ) = CreateTournamentForm(
    name = "T", nbRounds = nbRounds, clockLimit = 300, clockIncrement = 3,
    rated = true, format = format, startPosition = StartPosition.Standard,
    matchesPerPairing = 1, groupSize = None,
    opening = opening, bots = bots, maxConcurrentGames = maxConcurrentGames,
    openings = openings,
  )

  private def bot(n: Int) = BotRef(BotId(s"b$n"), s"Bot$n")

  private def foolsMate(gsvc: GameService, gameRepo: GameRepository, gameId: GameId) =
    for
      g <- gameRepo.get(gameId).map(_.get)
      _ <- gsvc.makeMove(gameId, "f2f3", g.white.id)
      _ <- gsvc.makeMove(gameId, "e7e5", g.black.id)
      _ <- gsvc.makeMove(gameId, "g2g4", g.white.id)
      _ <- gsvc.makeMove(gameId, "d8h4", g.black.id)
    yield ()

  def spec = suite("TournamentService features")(

    test("create resolves an opening key to a FEN start position") {
      for
        tsvc <- ZIO.service[TournamentService]
        t <- tsvc.create(form(opening = Some("vienna")), director)
      yield assertTrue(t.config.startPosition match
        case StartPosition.FromFen(fen) => fen.nonEmpty
        case _ => false)
    },
    test("create rejects an unknown opening") {
      for
        tsvc <- ZIO.service[TournamentService]
        e <- tsvc.create(form(opening = Some("ghost")), director).flip
      yield assertTrue(e.isInstanceOf[DomainError.BadRequest])
    },
    test("create seeds participants from the bot registry") {
      for
        tsvc <- ZIO.service[TournamentService]
        reg <- ZIO.service[BotRegistryService]
        a <- reg.register("Alpha", None)
        b <- reg.register("Beta", None)
        t <- tsvc.create(form(bots = Some(s"${a.id.value}, ${b.id.value}")), director)
      yield assertTrue(t.participants.map(_.id).toSet == Set(a.id, b.id))
    },
    test("create rejects an unknown registered bot") {
      for
        tsvc <- ZIO.service[TournamentService]
        e <- tsvc.create(form(bots = Some("nobody")), director).flip
      yield assertTrue(e.isInstanceOf[DomainError.BadRequest])
    },
    test("create stores the max concurrent games cap") {
      for
        tsvc <- ZIO.service[TournamentService]
        t <- tsvc.create(form(maxConcurrentGames = Some(2)), director)
      yield assertTrue(t.config.maxConcurrentGames.contains(2))
    },

    test("create with an openings book sets startPositions and doubles matchesPerPairing") {
      for
        tsvc <- ZIO.service[TournamentService]
        t <- tsvc.create(form(openings = Some("vienna, italian")), director)
      yield assertTrue(
        t.config.startPositions.size == 2,
        t.config.matchesPerPairing == 4,
      )
    },
    test("create rejects an unknown opening in the book") {
      for
        tsvc <- ZIO.service[TournamentService]
        e <- tsvc.create(form(openings = Some("vienna,ghost")), director).flip
      yield assertTrue(e.isInstanceOf[DomainError.BadRequest])
    },
    test("start with an openings book plays each position twice with reversed colours") {
      for
        tsvc <- ZIO.service[TournamentService]
        gameRepo <- ZIO.service[GameRepository]
        t <- tsvc.create(form(openings = Some("vienna,italian")), director)
        _ <- tsvc.join(t.id, bot(1))
        _ <- tsvc.join(t.id, bot(2))
        _ <- tsvc.start(t.id, director)
        games <- gameRepo.findByTournament(t.id)
      yield assertTrue(
        games.size == 4,
        games.count(_.white.id == bot(1).id) == 2,
        games.count(_.white.id == bot(2).id) == 2,
        games.map(_.fen).distinct.size == 2,
      )
    },

    test("addRegisteredBot adds a registered bot to the tournament") {
      for
        tsvc <- ZIO.service[TournamentService]
        reg <- ZIO.service[BotRegistryService]
        b <- reg.register("Joiner", None)
        t <- tsvc.create(form(), director)
        _ <- tsvc.addRegisteredBot(t.id, b.id, director)
        updated <- tsvc.get(t.id)
      yield assertTrue(updated.participants.map(_.id).contains(b.id))
    },
    test("addRegisteredBot is forbidden for non-directors") {
      for
        tsvc <- ZIO.service[TournamentService]
        reg <- ZIO.service[BotRegistryService]
        b <- reg.register("Joiner2", None)
        t <- tsvc.create(form(), director)
        e <- tsvc.addRegisteredBot(t.id, b.id, other).flip
      yield assertTrue(e.isInstanceOf[DomainError.Forbidden])
    },
    test("addRegisteredBot fails for an unknown bot") {
      for
        tsvc <- ZIO.service[TournamentService]
        t <- tsvc.create(form(), director)
        e <- tsvc.addRegisteredBot(t.id, BotId("ghost"), director).flip
      yield assertTrue(e.isInstanceOf[DomainError.NotFound])
    },

    test("maxConcurrentGames=1 keeps all but one game pending, then activates on completion") {
      for
        tsvc <- ZIO.service[TournamentService]
        gsvc <- ZIO.service[GameService]
        gameRepo <- ZIO.service[GameRepository]
        t <- tsvc.create(form(maxConcurrentGames = Some(1)), director)
        _ <- ZIO.foreach(1 to 4)(n => tsvc.join(t.id, bot(n)))
        started <- tsvc.start(t.id, director)
        afterStart <- gameRepo.findByTournament(t.id)
        ongoing = afterStart.filter(_.status == GameStatus.Ongoing)
        pending = afterStart.filter(_.status == GameStatus.Pending)
        // moving in a pending game is rejected
        movePending <- gsvc.makeMove(pending.head.id, "e2e4", pending.head.white.id).flip
        // finishing the active game should activate the queued one
        _ <- foolsMate(gsvc, gameRepo, ongoing.head.id)
        afterFirst <- gameRepo.findByTournament(t.id)
      yield assertTrue(
        ongoing.size == 1,
        pending.size == 1,
        movePending.isInstanceOf[DomainError.Conflict],
        afterFirst.count(_.status == GameStatus.Ongoing) == 1,
        afterFirst.count(_.status == GameStatus.Pending) == 0,
      )
    },

    test("random knockout advances winners into a second round") {
      for
        tsvc <- ZIO.service[TournamentService]
        gsvc <- ZIO.service[GameService]
        gameRepo <- ZIO.service[GameRepository]
        t <- tsvc.create(form(format = TournamentFormat.RandomKnockout, nbRounds = 2), director)
        _ <- ZIO.foreach(1 to 4)(n => tsvc.join(t.id, bot(n)))
        started <- tsvc.start(t.id, director)
        round1Games = started.rounds.head.pairings.map(_.matches.head.gameId)
        _ <- ZIO.foreach(round1Games)(gid => foolsMate(gsvc, gameRepo, gid))
        advanced <- tsvc.get(t.id)
      yield assertTrue(
        started.rounds.head.pairings.size == 2,
        advanced.currentRound == 2,
        advanced.rounds.exists(_.number == 2),
      )
    },
  ).provide(layer)
