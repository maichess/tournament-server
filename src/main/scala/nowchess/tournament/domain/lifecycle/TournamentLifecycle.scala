package nowchess.tournament.domain.lifecycle

import nowchess.tournament.domain.model.*
import nowchess.tournament.domain.tournament.*
import nowchess.tournament.domain.round.*
import nowchess.tournament.domain.error.DomainError
import java.time.Instant

object TournamentLifecycle:

  def join(tournament: Tournament, bot: BotRef): Either[DomainError, Tournament] =
    if tournament.status != TournamentStatus.Created then
      Left(DomainError.Conflict("tournament already started"))
    else if tournament.participants.exists(_.id == bot.id) then
      Left(DomainError.Conflict("bot already joined"))
    else
      Right(tournament.copy(participants = tournament.participants :+ bot))

  def withdraw(tournament: Tournament, botId: BotId): Either[DomainError, Tournament] =
    if tournament.status != TournamentStatus.Created then
      Left(DomainError.Conflict("tournament already started"))
    else if !tournament.participants.exists(_.id == botId) then
      Left(DomainError.Conflict("bot not in tournament"))
    else
      Right(tournament.copy(participants = tournament.participants.filterNot(_.id == botId)))

  def start(tournament: Tournament, now: Instant): Either[DomainError, Tournament] =
    if tournament.status != TournamentStatus.Created then
      Left(DomainError.Conflict("tournament already started"))
    else if tournament.participants.size < 2 then
      Left(DomainError.Conflict("need at least 2 participants"))
    else
      Right(tournament.copy(
        status = TournamentStatus.Started,
        startedAt = Some(now),
        currentRound = 1,
      ))

  def addRound(tournament: Tournament, round: Round): Either[DomainError, Tournament] =
    if tournament.status != TournamentStatus.Started then
      Left(DomainError.Conflict("tournament not started"))
    else
      Right(tournament.copy(rounds = tournament.rounds :+ round))

  def advanceRound(tournament: Tournament): Either[DomainError, Tournament] =
    if tournament.status != TournamentStatus.Started then
      Left(DomainError.Conflict("tournament not started"))
    else if tournament.currentRound >= tournament.config.nbRounds then
      Left(DomainError.Conflict("already at final round"))
    else
      Right(tournament.copy(currentRound = tournament.currentRound + 1))

  def updateRound(tournament: Tournament, roundNumber: Int, updatedRound: Round): Either[DomainError, Tournament] =
    if tournament.status != TournamentStatus.Started then
      Left(DomainError.Conflict("tournament not started"))
    else
      val idx = tournament.rounds.indexWhere(_.number == roundNumber)
      if idx < 0 then Left(DomainError.NotFound(s"round $roundNumber not found"))
      else Right(tournament.copy(rounds = tournament.rounds.updated(idx, updatedRound)))

  def finish(tournament: Tournament, winner: BotRef): Either[DomainError, Tournament] =
    if tournament.status != TournamentStatus.Started then
      Left(DomainError.Conflict("tournament not started"))
    else
      Right(tournament.copy(
        status = TournamentStatus.Finished,
        winner = Some(winner),
      ))

  def terminate(tournament: Tournament): Either[DomainError, Tournament] =
    if tournament.status != TournamentStatus.Created then
      Left(DomainError.Conflict("can only terminate before start"))
    else
      Right(tournament.copy(status = TournamentStatus.Finished))
