package org.biobank.services.centres

import akka.actor.ActorRef
import akka.pattern.ask
import com.google.inject.ImplementedBy
import javax.inject.{Inject, Named}
import org.biobank.domain.Slug
import org.biobank.domain.access.PermissionId
import org.biobank.domain.centres.{CentreId, CentreRepository}
import org.biobank.domain.containers._
import org.biobank.domain.users.UserId
import org.biobank.dto._
import org.biobank.infrastructure.AscendingOrder
import org.biobank.infrastructure.commands.ContainerTypeCommands._
import org.biobank.infrastructure.events.ContainerTypeEvents._
import org.biobank.services._
import org.biobank.services.access.AccessService
import org.slf4j.{Logger, LoggerFactory}
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._

@ImplementedBy(classOf[ContainerTypesServiceImpl])
trait ContainerTypesService extends BbwebService {

  def getBySlug(requestUserId: UserId, slug: Slug): Future[ServiceValidation[ContainerTypeDto]]

  def search(
      requestUserId: UserId,
      centreId:      CentreId,
      query:         PagedQuery
    ): Future[ServiceValidation[PagedResults[ContainerTypeDto]]]

  def processCommand(cmd: ContainerTypeCommand): Future[ServiceValidation[ContainerTypeDto]]

  def processRemoveCommand(cmd: RemoveContainerTypeCmd): Future[ServiceValidation[Boolean]]

  def snapshotRequest(requestUserId: UserId): ServiceValidation[Unit]

}

class ContainerTypesServiceImpl @Inject()(
    @Named("containerTypesProcessor") val processor: ActorRef,
    val accessService:                               AccessService,
    val centresService:                              CentresService,
    val centreRepository:                            CentreRepository,
    val containerTypeRepository:                     ContainerTypeRepository,
    val schemaRepository:                            ContainerSchemaRepository)
    extends ContainerTypesService with AccessChecksSerivce with ServicePermissionChecks {

  val log: Logger = LoggerFactory.getLogger(this.getClass)

  def getBySlug(requestUserId: UserId, slug: Slug): Future[ServiceValidation[ContainerTypeDto]] =
    Future {
      whenContainerTypePermitted(requestUserId, slug) { containerType =>
        containerTypeToDto(containerType)
      }
    }

  /** All [[domain.containers.ContainerType ContainerTypes]] for a [domain.centres.Centre Centre], and all
   * shared [[domain.containers.ContainerType ContainerTypes]] for other [domain.centres.Centre Centres].
   */
  def search(
      requestUserId: UserId,
      centreId:      CentreId,
      query:         PagedQuery
    ): Future[ServiceValidation[PagedResults[ContainerTypeDto]]] =
    Future {
      for {
        centre <- centresService.getCentre(requestUserId, centreId)
        allContainerTypes = containerTypeRepository.allForCentre(centre.id)
        containerTypes <- filterContainerTypesInternal(allContainerTypes, query.filter, query.sort)
        validPage      <- query.validPage(containerTypes.size)
        dtos           <- containerTypes.map(ct => containerTypeToDto(ct)).toList.sequenceU
        results        <- PagedResults.create(dtos, query.page, query.limit)
      } yield results
    }

  def processCommand(cmd: ContainerTypeCommand): Future[ServiceValidation[ContainerTypeDto]] = {
    val validCentre = cmd match {
      case c: ContainerTypeAddCommand => centreRepository.getByKey(CentreId(c.centreId))
      case c: ContainerTypeModifyCommand =>
        for {
          containerType <- containerTypeRepository.getByKey(ContainerTypeId(c.id))
          centre        <- centreRepository.getByKey(containerType.centreId)
        } yield centre

    }

    val permission = cmd match {
      case c: ContainerTypeAddCommand => PermissionId.ContainerCreate
      case _ => PermissionId.ContainerUpdate
    }

    val requestUserId = UserId(cmd.sessionUserId)

    validCentre
      .fold(err => Future.successful((err.failure[ContainerTypeDto])),
            centre =>
              whenPermittedAndIsMemberAsync(requestUserId, permission, None, Some(centre.id)) { () =>
                ask(processor, cmd).mapTo[ServiceValidation[ContainerTypeEvent]].map {
                  validation =>
                    // need to retrieve the centre attached to the returned containerType, since there is a
                    // possibility it was updated to a new centre
                    for {
                      event               <- validation
                      containerType       <- containerTypeRepository.getByKey(ContainerTypeId(event.id))
                      schema              <- schemaRepository.getByKey(containerType.schemaId)
                      containerTypeCentre <- centreRepository.getByKey(containerType.centreId)
                    } yield ContainerTypeDto(containerType, containerTypeCentre, schema)
                }
              })
  }

  def processRemoveCommand(cmd: RemoveContainerTypeCmd): Future[ServiceValidation[Boolean]] = {
    val validCentre = for {
      containerType <- containerTypeRepository.getByKey(ContainerTypeId(cmd.id))
      centre        <- centreRepository.getByKey(containerType.centreId)
    } yield centre

    validCentre
      .fold(err => Future.successful((err.failure[Boolean])),
            centre =>
              whenPermittedAndIsMemberAsync(UserId(cmd.sessionUserId),
                                            PermissionId.ContainerDelete,
                                            None,
                                            Some(centre.id)) { () =>
                ask(processor, cmd)
                  .mapTo[ServiceValidation[ContainerTypeEvent]]
                  .map { _.map(event => true) }
              })
  }

  private def whenContainerTypePermitted[T](
      requestUserId:     UserId,
      containerTypeSlug: Slug
    )(block:             ContainerType => ServiceValidation[T]
    ): ServiceValidation[T] = {
    for {
      containerType <- containerTypeRepository.getBySlug(containerTypeSlug)
      centre        <- centreRepository.getByKey(containerType.centreId)
      result <- whenPermittedAndIsMember(requestUserId, PermissionId.ContainerRead, None, Some(centre.id))(
                 () => block(containerType)
               )
    } yield result
  }

  private def filterContainerTypesInternal(
      unfilteredContainerTypes: Set[ContainerType],
      filter:                   FilterString,
      sort:                     SortString
    ): ServiceValidation[Seq[ContainerType]] = {
    val sortStr =
      if (sort.expression.isEmpty) new SortString("name")
      else sort

    for {
      containerTypes <- ContainerTypeFilter.filterContainerTypes(unfilteredContainerTypes, filter)
      sortExpressions <- {
        QuerySortParser(sortStr).toSuccessNel(ServiceError(s"could not parse sort expression: $sort"))
      }
      sortFunc <- {
        ContainerType.sort2Compare
          .get(sortExpressions(0).name).toSuccessNel(
            ServiceError(s"invalid sort field: ${sortExpressions(0).name}")
          )
      }
    } yield {
      val result = containerTypes.toSeq.sortWith(sortFunc)
      if (sortExpressions(0).order == AscendingOrder) result
      else result.reverse
    }
  }

  private def containerTypeToDto(containerType: ContainerType): ServiceValidation[ContainerTypeDto] =
    for {
      schema <- schemaRepository.getByKey(containerType.schemaId)
      centre <- centreRepository.getByKey(containerType.centreId)
    } yield ContainerTypeDto(containerType, centre, schema)
}
