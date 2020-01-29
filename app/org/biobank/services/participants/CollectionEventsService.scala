package org.biobank.services.participants

import akka.actor._
import akka.pattern.ask
import com.google.inject.ImplementedBy
import javax.inject.{Inject, Named}
import org.biobank._
import org.biobank.domain.access._
import org.biobank.domain.participants._
import org.biobank.domain.studies._
import org.biobank.domain.users.UserId
import org.biobank.dto.participants.CollectionEventDto
import org.biobank.infrastructure.AscendingOrder
import org.biobank.infrastructure.commands.CollectionEventCommands._
import org.biobank.infrastructure.events.CollectionEventEvents._
import org.biobank.services._
import org.biobank.services.access.AccessService
import org.slf4j.{Logger, LoggerFactory}
import scala.concurrent.ExecutionContext
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._

@ImplementedBy(classOf[CollectionEventsServiceImpl])
trait CollectionEventsService extends BbwebService {

  def get(requestUserId: UserId, collectionEventId: CollectionEventId): ServiceValidation[CollectionEventDto]

  def getByVisitNumber(
      requestUserId: UserId,
      participantId: ParticipantId,
      visitNumber:   Int
    ): ServiceValidation[CollectionEventDto]

  def list(
      requestUserId: UserId,
      participantId: ParticipantId,
      pagedQuery:    PagedQuery
    ): FutureValidation[PagedResults[CollectionEventDto]]

  def collectionEventToDto(
      requestUserId: UserId,
      event:         CollectionEvent
    ): ServiceValidation[CollectionEventDto]

  def processCommand(cmd: CollectionEventCommand): FutureValidation[CollectionEventDto]

  def processRemoveCommand(cmd: RemoveCollectionEventCmd): FutureValidation[Boolean]

  def snapshotRequest(requestUserId: UserId): ServiceValidation[Unit]

}

@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
class CollectionEventsServiceImpl @Inject()(
    @Named("collectionEventsProcessor") val processor: ActorRef,
    val accessService:                                 AccessService,
    val studyRepository:                               StudyRepository,
    val participantRepository:                         ParticipantRepository,
    val collectionEventRepository:                     CollectionEventRepository,
    val collectionEventTypeRepository:                 CollectionEventTypeRepository
  )(
    implicit
    val executionContext: ExecutionContext)
    extends CollectionEventsService with AccessChecksSerivce with ServicePermissionChecks {

  val log: Logger = LoggerFactory.getLogger(this.getClass)

  def get(
      requestUserId:     UserId,
      collectionEventId: CollectionEventId
    ): ServiceValidation[CollectionEventDto] =
    for {
      cevent <- collectionEventRepository.getByKey(collectionEventId)
      dto    <- collectionEventToDto(requestUserId, cevent)
    } yield dto

  def getByVisitNumber(
      requestUserId: UserId,
      participantId: ParticipantId,
      visitNumber:   Int
    ): ServiceValidation[CollectionEventDto] =
    for {
      cevent <- collectionEventRepository.withVisitNumber(participantId, visitNumber)
      dto    <- collectionEventToDto(requestUserId, cevent)
    } yield dto

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def list(
      requestUserId: UserId,
      participantId: ParticipantId,
      query:         PagedQuery
    ): FutureValidation[PagedResults[CollectionEventDto]] =
    FutureValidation {
      whenParticipantPermitted(requestUserId, participantId) { participant =>
        val allCevents = collectionEventRepository.allForParticipant(participantId).toSet
        val sortStr =
          if (query.sort.expression.isEmpty) new SortString("visitNumber")
          else query.sort

        for {
          cevents   <- CollectionEventFilter.filterCollectionEvents(allCevents, query.filter)
          validPage <- query.validPage(cevents.size)
          sortExpressions <- {
            QuerySortParser(sortStr).toSuccessNel(
              ServiceError(s"could not parse sort expression: ${query.sort}")
            )
          }
          firstSort <- {
            sortExpressions.headOption.toSuccessNel(ServiceError("at least one sort expression is required"))
          }
          sortFunc <- {
            CollectionEvent.sort2Compare
              .get(firstSort.name).toSuccessNel(ServiceError(s"invalid sort field: ${firstSort.name}"))
          }
          sorted <- cevents.toSeq.sortWith(sortFunc).successNel[String]
          dtos <- {
            cevents
              .map(event => collectionEventToDto(requestUserId, event, participant))
              .toList.sequenceU.map(_.toSeq)
          }
          results <- {
            val sorted =
              if (firstSort.order == AscendingOrder) dtos
              else dtos.reverse

            PagedResults.create(sorted, query.page, query.limit)
          }
        } yield results
      }
    }

  def processCommand(cmd: CollectionEventCommand): FutureValidation[CollectionEventDto] = {
    val validCommand = cmd match {
      case c: RemoveCollectionEventCmd =>
        ServiceError(s"invalid service call: $cmd, use processRemoveCommand").failureNel[DisabledStudy]
      case c => c.successNel[String]
    }

    validCommand
      .fold(err => FutureValidation(err.failure[CollectionEventDto]),
            c =>
              whenParticipantPermittedAsync(cmd) { () =>
                for {
                  event <- FutureValidation(
                            ask(processor, cmd).mapTo[ServiceValidation[CollectionEventEvent]]
                          )
                  cevent <- FutureValidation(collectionEventRepository.getByKey(CollectionEventId(event.id)))
                  dto    <- FutureValidation(collectionEventToDto(UserId(cmd.sessionUserId), cevent))
                } yield dto
              })
  }

  def processRemoveCommand(cmd: RemoveCollectionEventCmd): FutureValidation[Boolean] =
    whenParticipantPermittedAsync(cmd) { () =>
      FutureValidation(ask(processor, cmd).mapTo[ServiceValidation[CollectionEventEvent]])
        .map(_ => true)
    }

  private def whenParticipantPermitted[T](
      requestUserId: UserId,
      participantId: ParticipantId
    )(block:         Participant => ServiceValidation[T]
    ): ServiceValidation[T] =
    for {
      participant <- participantRepository.getByKey(participantId)
      study       <- studyRepository.getByKey(participant.studyId)
      result <- whenPermittedAndIsMember(requestUserId,
                                         PermissionId.CollectionEventRead,
                                         Some(study.id),
                                         None)(() => block(participant))
    } yield result

  private def whenParticipantPermittedAsync[T](
      cmd:   CollectionEventCommand
    )(block: () => FutureValidation[T]
    ): FutureValidation[T] = {
    val validParticipantId = cmd match {
      case c: AddCollectionEventCmd => ParticipantId(c.participantId).successNel[String]
      case c: CollectionEventModifyCommand =>
        collectionEventRepository.getByKey(CollectionEventId(c.id)).map(c => c.participantId)
    }

    val permission = cmd match {
      case c: AddCollectionEventCmd    => PermissionId.CollectionEventCreate
      case c: RemoveCollectionEventCmd => PermissionId.CollectionEventDelete
      case c => PermissionId.CollectionEventUpdate
    }

    val validStudy = for {
      participantId <- validParticipantId
      participant   <- participantRepository.getByKey(participantId)
      study         <- studyRepository.getByKey(participant.studyId)
    } yield study

    validStudy.fold(
      err => FutureValidation(err.failure[T]),
      study =>
        whenPermittedAndIsMemberAsync(UserId(cmd.sessionUserId), permission, Some(study.id), None)(block)
    )
  }

  private def collectionEventToDto(
      requestUserId: UserId,
      event:         CollectionEvent,
      participant:   Participant
    ): ServiceValidation[CollectionEventDto] =
    for {
      ceventType <- collectionEventTypeRepository.getByKey(event.collectionEventTypeId)
      permitted <- whenPermittedAndIsMember(requestUserId,
                                            PermissionId.CollectionEventRead,
                                            Some(ceventType.studyId),
                                            None) { () =>
                    ().successNel[String]
                  }
    } yield CollectionEventDto(event, participant, ceventType)

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def collectionEventToDto(
      requestUserId: UserId,
      event:         CollectionEvent
    ): ServiceValidation[CollectionEventDto] =
    participantRepository.getByKey(event.participantId) flatMap { participant =>
      collectionEventToDto(requestUserId, event, participant)
    }

}
