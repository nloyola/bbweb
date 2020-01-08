package org.biobank.services.participants

import akka.actor._
import akka.pattern.ask
import com.google.inject.ImplementedBy
import javax.inject.{Inject, Named}
import org.biobank._
import org.biobank.domain.Slug
import org.biobank.domain.access._
import org.biobank.domain.centres.CentreRepository
import org.biobank.domain.participants._
import org.biobank.domain.studies._
import org.biobank.domain.users.UserId
import org.biobank.dto.{CentreLocationInfo, CollectionEventDto, SpecimenDto}
import org.biobank.infrastructure.AscendingOrder
import org.biobank.infrastructure.commands.SpecimenCommands._
import org.biobank.infrastructure.events.SpecimenEvents._
import org.biobank.services._
import org.biobank.services.access.AccessService
import org.slf4j.{Logger, LoggerFactory}
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._

@ImplementedBy(classOf[SpecimensServiceImpl])
trait SpecimensService extends BbwebService {

  def get(requestUserId: UserId, id: SpecimenId): ServiceValidation[SpecimenDto]

  def getBySlug(requestUserId: UserId, slug: Slug): ServiceValidation[SpecimenDto]

  def getByInventoryId(requestUserId: UserId, inventoryId: String): ServiceValidation[Specimen]

  def getByInventoryIds(requestUserId: UserId, inventoryIds: String*): ServiceValidation[List[Specimen]]

  def list(
      requestUserId:     UserId,
      collectionEventId: CollectionEventId,
      query:             PagedQuery
    ): FutureValidation[PagedResults[SpecimenDto]]

  def listBySlug(
      requestUserId: UserId,
      eventSlug:     Slug,
      query:         PagedQuery
    ): FutureValidation[PagedResults[SpecimenDto]]

  def processCommand(cmd: SpecimenCommand): FutureValidation[CollectionEventDto]

  def processRemoveCommand(cmd: SpecimenCommand): FutureValidation[Boolean]

  def snapshotRequest(requestUserId: UserId): ServiceValidation[Unit]

}

@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
class SpecimensServiceImpl @Inject()(
    @Named("specimensProcessor") val processor: ActorRef,
    val accessService:                          AccessService,
    val eventsService:                          CollectionEventsService,
    val studyRepository:                        StudyRepository,
    val participantRepository:                  ParticipantRepository,
    val collectionEventRepository:              CollectionEventRepository,
    val collectionEventTypeRepository:          CollectionEventTypeRepository,
    val ceventSpecimenRepository:               CeventSpecimenRepository,
    val specimenRepository:                     SpecimenRepository,
    val centreRepository:                       CentreRepository
  )(
    implicit
    val executionContext: BbwebExecutionContext)
    extends SpecimensService with AccessChecksSerivce with ServicePermissionChecks {

  val log: Logger = LoggerFactory.getLogger(this.getClass)

  def get(requestUserId: UserId, id: SpecimenId): ServiceValidation[SpecimenDto] =
    for {
      ceventSpecimen <- ceventSpecimenRepository.withSpecimenId(id)
      permitted      <- whenSpecimenPermitted(requestUserId, ceventSpecimen.ceventId)(_ => ().successNel[String])
      specimen       <- specimenRepository.getByKey(id)
      dto            <- specimenToDto(specimen)
    } yield dto

  def getBySlug(requestUserId: UserId, slug: Slug): ServiceValidation[SpecimenDto] =
    for {
      specimen       <- specimenRepository.getBySlug(slug)
      ceventSpecimen <- ceventSpecimenRepository.withSpecimenId(specimen.id)
      permitted      <- whenSpecimenPermitted(requestUserId, ceventSpecimen.ceventId)(_ => ().successNel[String])
      dto            <- specimenToDto(specimen)
    } yield dto

  def getByInventoryId(requestUserId: UserId, inventoryId: String): ServiceValidation[Specimen] =
    for {
      specimen       <- specimenRepository.getByInventoryId(inventoryId)
      ceventSpecimen <- ceventSpecimenRepository.withSpecimenId(specimen.id)
      permitted      <- whenSpecimenPermitted(requestUserId, ceventSpecimen.ceventId)(_ => ().successNel[String])
    } yield specimen

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def getByInventoryIds(requestUserId: UserId, inventoryIds: String*): ServiceValidation[List[Specimen]] = {
    inventoryIds.map(inventoryId => getByInventoryId(requestUserId, inventoryId)).toList.sequenceU
  }

  def list(
      requestUserId:     UserId,
      collectionEventId: CollectionEventId,
      query:             PagedQuery
    ): FutureValidation[PagedResults[SpecimenDto]] =
    FutureValidation {
      filterSpecimens(requestUserId, collectionEventId, query);
    }

  def listBySlug(
      requestUserId: UserId,
      eventSlug:     Slug,
      query:         PagedQuery
    ): FutureValidation[PagedResults[SpecimenDto]] =
    FutureValidation {
      for {
        event     <- collectionEventRepository.getBySlug(eventSlug)
        specimens <- filterSpecimens(requestUserId, event.id, query)
      } yield specimens
    }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def filterSpecimens(
      requestUserId: UserId,
      eventId:       CollectionEventId,
      query:         PagedQuery
    ): ServiceValidation[PagedResults[SpecimenDto]] =
    for {
      permitted <- whenSpecimenPermitted(requestUserId, eventId)(_ => ().successNel[String])
      specimens <- sortSpecimens(eventId, query.sort)
      validPage <- query.validPage(specimens.size)
      dtos      <- specimens.map(specimenToDto).toList.sequenceU
      results   <- PagedResults.create(dtos, query.page, query.limit)
    } yield results

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def sortSpecimens(
      ceventId: CollectionEventId,
      sort:     SortString
    ): ServiceValidation[List[Specimen]] = {
    val sortStr =
      if (sort.expression.isEmpty) new SortString("inventoryId")
      else sort

    for {
      specimens <- {
        ceventSpecimenRepository
          .withCeventId(ceventId)
          .map { cs =>
            specimenRepository.getByKey(cs.specimenId)
          }
          .toList
          .sequenceU
      }
      sortExpressions <- {
        QuerySortParser(sortStr).toSuccessNel(ServiceError(s"could not parse sort expression: ${sort}"))
      }
      sortFunc <- {
        Specimen.sort2Compare
          .get(sortExpressions(0).name).toSuccessNel(
            ServiceError(s"invalid sort field: ${sortExpressions(0).name}")
          )
      }
    } yield {
      val result = specimens.sortWith(sortFunc)
      if (sortExpressions(0).order == AscendingOrder) result
      else result.reverse
    }
  }

  def processCommand(cmd: SpecimenCommand): FutureValidation[CollectionEventDto] = {
    val validCommand = cmd match {
      case c: RemoveSpecimenCmd =>
        ServiceError(s"invalid service call: $cmd, use processRemoveCommand").failureNel[CollectionEvent]
      case c => c.successNel[String]
    }

    validCommand
      .fold(err => FutureValidation(err.failure[CollectionEventDto]),
                      _ =>
                        whenSpecimenPermittedAsync(cmd) { () =>
                            for {
                  event <- FutureValidation(ask(processor, cmd).mapTo[ServiceValidation[SpecimenEvent]])
                  cevent <- FutureValidation(
                             collectionEventRepository
                                         .getByKey(CollectionEventId(event.getAdded.getCollectionEventId))
                           )
                  dto <- FutureValidation(
                          eventsService.collectionEventToDto(UserId(cmd.sessionUserId), cevent)
                        )
                            } yield dto
                        })
  }

  def processRemoveCommand(cmd: SpecimenCommand): FutureValidation[Boolean] = {
    whenSpecimenPermittedAsync(cmd) { () =>
      FutureValidation(ask(processor, cmd).mapTo[ServiceValidation[SpecimenEvent]]).map(_ => true)
      }
    }

  //
  // Invokes function "block" if user that invoked this service has the permission and membership
  // to do so.
  //
  private def whenSpecimenPermitted[T](
      requestUserId: UserId,
      ceventId:      CollectionEventId
    )(block:         CollectionEvent => ServiceValidation[T]
    ): ServiceValidation[T] =
    for {
      cevent     <- collectionEventRepository.getByKey(ceventId)
      ceventType <- collectionEventTypeRepository.getByKey(cevent.collectionEventTypeId)
      study      <- studyRepository.getByKey(ceventType.studyId)
      result <- whenPermittedAndIsMember(requestUserId,
                                         PermissionId.CollectionEventRead,
                                         Some(study.id),
                                         None)(() => block(cevent))
    } yield result

  //
  // Invokes function "block" if user that issued the command has the permission and membership
  // to do so.
  //
  private def whenSpecimenPermittedAsync[T](
      cmd:   SpecimenCommand
    )(block: () => FutureValidation[T]
    ): FutureValidation[T] = {

    val validCeventId = cmd match {
      case c: SpecimenModifyCommand =>
        collectionEventRepository.getByKey(CollectionEventId(c.collectionEventId)).map(c => c.id)
      case c: SpecimenCommand => CollectionEventId(c.collectionEventId).successNel[String]
    }

    val permission = cmd match {
      case c: AddSpecimensCmd   => PermissionId.SpecimenCreate
      case c: RemoveSpecimenCmd => PermissionId.SpecimenDelete
      case c => PermissionId.SpecimenUpdate
    }

    val validStudy = for {
      ceventId   <- validCeventId
      cevent     <- collectionEventRepository.getByKey(ceventId)
      ceventType <- collectionEventTypeRepository.getByKey(cevent.collectionEventTypeId)
      study      <- studyRepository.getByKey(ceventType.studyId)
    } yield study

    validStudy.fold(
      err => FutureValidation(err.failure[T]),
      study =>
        whenPermittedAndIsMemberAsync(UserId(cmd.sessionUserId), permission, Some(study.id), None)(block)
    )
  }

  private def specimenToDto(specimen: Specimen): ServiceValidation[SpecimenDto] =
    for {
      ceventSpecimen     <- ceventSpecimenRepository.withSpecimenId(specimen.id)
      cevent             <- collectionEventRepository.getByKey(ceventSpecimen.ceventId)
      ceventType         <- collectionEventTypeRepository.getByKey(cevent.collectionEventTypeId)
      specimenDefinition <- ceventType.specimenDefinition(specimen.specimenDefinitionId)
      originCentre       <- centreRepository.getByLocationId(specimen.originLocationId)
      originLocation     <- originCentre.locationWithId(specimen.originLocationId)
      centre             <- centreRepository.getByLocationId(specimen.locationId)
      location           <- centre.locationWithId(specimen.locationId)
      study              <- studyRepository.getEnabled(ceventType.studyId)
      participant        <- participantRepository.getByKey(cevent.participantId)
    } yield {
      val originLocationInfo = CentreLocationInfo(originCentre, originLocation)
      val locationInfo       = CentreLocationInfo(centre, location)
      specimen.createDto(cevent,
                         ceventType,
                         specimenDefinition,
                         originLocationInfo,
                         locationInfo,
                         study,
                         participant)
    }

}
