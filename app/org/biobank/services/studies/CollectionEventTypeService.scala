package org.biobank.services.studies

import akka.actor.ActorRef
import akka.pattern.ask
import com.google.inject.ImplementedBy
import javax.inject.{Inject, Named}
import org.biobank._
import org.biobank.domain.Slug
import org.biobank.domain.access._
import org.biobank.domain.studies._
import org.biobank.domain.participants.CollectionEventRepository
import org.biobank.domain.users.UserId
import org.biobank.dto.{CollectedSpecimenDefinitionNames, NamedEntityInfoDto}
import org.biobank.infrastructure.AscendingOrder
import org.biobank.infrastructure.commands.CollectionEventTypeCommands._
import org.biobank.infrastructure.events.CollectionEventTypeEvents._
import org.biobank.services._
import org.biobank.services.access.AccessService
import org.slf4j.{Logger, LoggerFactory}
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._

/**
 * This is the CollectionEventType Aggregate Application Service.
 *
 * Handles the commands to configure Collection Event Types. the commands are forwarded to the
 * CollectionEventType Aggregate Processor.
 *
 */
@ImplementedBy(classOf[CollectionEventTypeServiceImpl])
trait CollectionEventTypeService extends BbwebService {

  def eventTypeWithId(
      requestUserId: UserId,
      studyId:       StudyId,
      eventTypeId:   CollectionEventTypeId
    ): ServiceValidation[CollectionEventType]

  def eventTypeBySlug(
      requestUserId: UserId,
      studySlug:     Slug,
      eventTypeSlug: Slug
    ): ServiceValidation[CollectionEventType]

  def eventTypeInUse(requestUserId: UserId, slug: Slug): ServiceValidation[Boolean]

  def list(
      requestUserId: UserId,
      studyId:       StudyId,
      pagedQuery:    PagedQuery
    ): FutureValidation[PagedResults[CollectionEventType]]

  def listByStudySlug(
      requestUserId: UserId,
      studySlug:     Slug,
      pagedQuery:    PagedQuery
    ): FutureValidation[PagedResults[CollectionEventType]]

  def specimenDefinitionsForStudy(
      requestUserId: UserId,
      studySlug:     Slug
    ): FutureValidation[Set[CollectedSpecimenDefinitionNames]]

  def listNamesByStudySlug(
      requestUserId: UserId,
      studySlug:     Slug,
      query:         FilterAndSortQuery
    ): FutureValidation[Seq[NamedEntityInfoDto]]

  def listNamesByStudyId(
      requestUserId: UserId,
      studyId:       StudyId,
      query:         FilterAndSortQuery
    ): FutureValidation[Seq[NamedEntityInfoDto]]

  def processCommand(cmd: CollectionEventTypeCommand): FutureValidation[CollectionEventType]

  def processRemoveCommand(cmd: RemoveCollectionEventTypeCmd): FutureValidation[Boolean]

  def snapshotRequest(requestUserId: UserId): ServiceValidation[Unit]

}

@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
class CollectionEventTypeServiceImpl @Inject()(
    @Named("collectionEventType") val processor: ActorRef,
    val accessService:                           AccessService,
    val eventTypeRepository:                     CollectionEventTypeRepository,
    val studiesService:                          StudiesService,
    val eventRepository:                         CollectionEventRepository
  )(
    implicit
    val executionContext: BbwebExecutionContext)
    extends CollectionEventTypeService with AccessChecksSerivce with ServicePermissionChecks {

  val log: Logger = LoggerFactory.getLogger(this.getClass)

  def eventTypeWithId(
      requestUserId: UserId,
      studyId:       StudyId,
      eventTypeId:   CollectionEventTypeId
    ): ServiceValidation[CollectionEventType] =
    whenPermittedAndIsMember(requestUserId, PermissionId.StudyRead, Some(studyId), None) { () =>
      withStudy(requestUserId, studyId) { study =>
        eventTypeRepository.withId(study.id, eventTypeId)
      }
    }

  def eventTypeBySlug(
      requestUserId: UserId,
      studySlug:     Slug,
      eventTypeSlug: Slug
    ): ServiceValidation[CollectionEventType] =
    studiesService.getStudyBySlug(requestUserId, studySlug).flatMap { study =>
      eventTypeRepository.getBySlug(eventTypeSlug)
    }

  def eventTypeInUse(requestUserId: UserId, slug: Slug): ServiceValidation[Boolean] =
    eventTypeRepository.getBySlug(slug).flatMap { ceventType =>
      whenPermittedAndIsMember(requestUserId, PermissionId.StudyRead, Some(ceventType.studyId), None) { () =>
        eventRepository.collectionEventTypeInUse(ceventType.id).successNel[String]
      }
    }

  def list(
      requestUserId: UserId,
      studyId:       StudyId,
      query:         PagedQuery
    ): FutureValidation[PagedResults[CollectionEventType]] =
    FutureValidation {
      whenPermittedAndIsMember(requestUserId, PermissionId.StudyRead, Some(studyId), None) { () =>
        queryInternal(requestUserId, studyId, query.filter, query.sort).flatMap { types =>
          PagedResults.create(types, query.page, query.limit)
        }
      }
    }

  def listByStudySlug(
      requestUserId: UserId,
      studySlug:     Slug,
      query:         PagedQuery
    ): FutureValidation[PagedResults[CollectionEventType]] =
    FutureValidation {
      for {
        study     <- studiesService.getStudyBySlug(requestUserId, studySlug)
        types     <- queryInternal(requestUserId, study.id, query.filter, query.sort)
        validPage <- query.validPage(types.size)
        results   <- PagedResults.create(types, query.page, query.limit)
      } yield results
    }

  def specimenDefinitionsForStudy(
      requestUserId: UserId,
      studySlug:     Slug
    ): FutureValidation[Set[CollectedSpecimenDefinitionNames]] =
    FutureValidation {
      for {
        study      <- studiesService.getStudyBySlug(requestUserId, studySlug)
        eventTypes <- queryInternal(requestUserId, study.id, new FilterString(""), new SortString(""))
      } yield {
        eventTypes
          .filter { !_.specimenDefinitions.isEmpty }
          .map(et => CollectedSpecimenDefinitionNames(et))
          .toSet
      }
    }

  def listNamesByStudyId(
      requestUserId: UserId,
      studyId:       StudyId,
      query:         FilterAndSortQuery
    ): FutureValidation[Seq[NamedEntityInfoDto]] =
    FutureValidation {
      for {
        study <- studiesService.getStudy(requestUserId, studyId)
        types <- queryInternal(requestUserId, study.id, query.filter, query.sort)
      } yield {
        types.map { t =>
          NamedEntityInfoDto(t.id.id, t.slug, t.name)
        }
      }
    }

  def listNamesByStudySlug(
      requestUserId: UserId,
      studySlug:     Slug,
      query:         FilterAndSortQuery
    ): FutureValidation[Seq[NamedEntityInfoDto]] =
    FutureValidation {
      for {
        study <- studiesService.getStudyBySlug(requestUserId, studySlug)
        types <- queryInternal(requestUserId, study.id, query.filter, query.sort)
      } yield {
        types.map { t =>
          NamedEntityInfoDto(t.id.id, t.slug, t.name)
        }
      }
    }

  def processCommand(cmd: CollectionEventTypeCommand): FutureValidation[CollectionEventType] = {
    val v = for {
      validCommand <- cmd match {
                       case c: RemoveCollectionEventTypeCmd =>
                         ServiceError(s"invalid service call: $cmd, use processRemoveCommand")
                           .failureNel[DisabledStudy]
                       case c => c.successNel[String]
                     }
      study <- studiesService.getDisabledStudy(UserId(cmd.sessionUserId), StudyId(cmd.studyId))
    } yield study

    v.fold(err => FutureValidation(err.failure[CollectionEventType]),
           study =>
             whenPermittedAndIsMemberAsync(UserId(cmd.sessionUserId),
                                           PermissionId.StudyUpdate,
                                           Some(study.id),
                                           None) { () =>
               for {
                 event <- FutureValidation(
                           ask(processor, cmd).mapTo[ServiceValidation[CollectionEventTypeEvent]]
                         )
                 result <- FutureValidation(eventTypeRepository.getByKey(CollectionEventTypeId(event.id)))
               } yield result
             })
  }

  def processRemoveCommand(cmd: RemoveCollectionEventTypeCmd): FutureValidation[Boolean] =
    studiesService
      .getDisabledStudy(UserId(cmd.sessionUserId), StudyId(cmd.studyId)).fold(
        err => FutureValidation(err.failure[Boolean]),
        study =>
          whenPermittedAndIsMemberAsync(UserId(cmd.sessionUserId),
                                        PermissionId.StudyUpdate,
                                        Some(study.id),
                                        None) { () =>
            FutureValidation(ask(processor, cmd).mapTo[ServiceValidation[CollectionEventTypeEvent]])
              .map(event => true)
          }
      )

  private def queryInternal(
      requestUserId: UserId,
      studyId:       StudyId,
      filter:        FilterString,
      sort:          SortString
    ): ServiceValidation[Seq[CollectionEventType]] = {
    val sortStr =
      if (sort.expression.isEmpty) new SortString("name")
      else sort

    for {
      study       <- studiesService.getStudy(requestUserId, studyId)
      ceventTypes <- getEventTypes(studyId, filter)
      sortExpressions <- {
        QuerySortParser(sortStr).toSuccessNel(ServiceError(s"could not parse sort expression: $sort"))
      }
      firstSort <- {
        sortExpressions.headOption.toSuccessNel(ServiceError("at least one sort expression is required"))
      }
      sortFunc <- {
        CollectionEventType.sort2Compare
          .get(firstSort.name).toSuccessNel(ServiceError(s"invalid sort field: ${firstSort.name}"))
      }
    } yield {
      val result = ceventTypes.toSeq.sortWith(sortFunc)

      if (firstSort.order == AscendingOrder) result
      else result.reverse
    }
  }

  private def withStudy[T](
      sessionUserId: UserId,
      studyId:       StudyId
    )(fn:            Study => ServiceValidation[T]
    ): ServiceValidation[T] =
    for {
      study  <- studiesService.getStudy(sessionUserId, studyId)
      result <- fn(study)
    } yield result

  private def getEventTypes(
      studyId: StudyId,
      filter:  FilterString
    ): ServiceValidation[Set[CollectionEventType]] = {
    val allCeventTypes = eventTypeRepository.allForStudy(studyId).toSet
    CollectionEventTypeFilter.filterCollectionEventTypes(allCeventTypes, filter)
  }
}
