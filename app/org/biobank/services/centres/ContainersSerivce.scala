package org.biobank.services.centres

import akka.actor.ActorRef
import akka.pattern.ask
import com.google.inject.ImplementedBy
import java.time.format.DateTimeFormatter
import javax.inject.{Inject, Named}
import org.biobank.domain.access.PermissionId
import org.biobank.domain.centres.{CentreId, CentreRepository}
import org.biobank.domain.containers._
import org.biobank.domain.users.UserId
import org.biobank.dto._
import org.biobank.infrastructure.AscendingOrder
import org.biobank.infrastructure.commands.ContainerCommands._
import org.biobank.infrastructure.events.ContainerEvents._
import org.biobank.services._
import org.biobank.services.access.AccessService
import org.slf4j.{Logger, LoggerFactory}
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._

@ImplementedBy(classOf[ContainersServiceImpl])
trait ContainersService extends BbwebService {

  def getContainer(requestUserId: UserId, id: ContainerId): ServiceValidation[ContainerDto]

  /** All Top [[domain.containers.Container Containers]] for a [domain.centres.Centre Centre]. */
  def getContainers(
      requestUserId: UserId,
      centreId:      CentreId,
      query:         PagedQuery
    ): Future[ServiceValidation[PagedResults[ContainerDto]]]

  def processCommand(cmd: ContainerCommand): Future[ServiceValidation[ContainerDto]]
}

class ContainersServiceImpl @Inject()(
    @Named("containersProcessor") val processor: ActorRef,
    val accessService:                           AccessService,
    val centresService:                          CentresService,
    val centreRepository:                        CentreRepository,
    val containerTypeRepository:                 ContainerTypeRepository,
    val containerRepository:                     ContainerRepository,
    val containerSchemaRepository:               ContainerSchemaRepository)
    extends ContainersService with AccessChecksSerivce with ServicePermissionChecks {

  val log: Logger = LoggerFactory.getLogger(this.getClass)

  def getContainer(requestUserId: UserId, id: ContainerId): ServiceValidation[ContainerDto] =
    whenContainerPermitted(requestUserId, id)(container => containerToDto(container))

  def getContainers(
      requestUserId: UserId,
      centreId:      CentreId,
      query:         PagedQuery
    ): Future[ServiceValidation[PagedResults[ContainerDto]]] =
    Future {
      for {
        centre <- centresService.getCentre(requestUserId, centreId)
        rootContainers = containerRepository.rootContainers(centreId).toSeq
        containers <- filterContainersInternal(rootContainers, query.filter, query.sort)
        validPage  <- query.validPage(containers.size)
        dtos       <- containers.map(containerToDto).toList.sequenceU.map(_.toSeq)
        result     <- PagedResults.create(dtos, query.page, query.limit)
      } yield result
    }

  def processCommand(cmd: ContainerCommand): Future[ServiceValidation[ContainerDto]] = {
    val validCentreId = cmd match {
      case c: AddRootContainerCmd =>
        for {
          centre <- centreRepository.getByKey(CentreId(c.centreId))
        } yield centre.id
      case c: AddSubContainerCommand => getContainerCentreId(ContainerId(c.parentId))
      case c: ContainerModifyCommand => getContainerCentreId(ContainerId(c.id))
    }

    val permission = cmd match {
      case c: AddContainerCommand => PermissionId.ContainerCreate
      case c => PermissionId.ContainerUpdate
    }

    val requestUserId = UserId(cmd.sessionUserId)

    validCentreId
      .fold(err => Future.successful((err.failure[ContainerDto])),
            centreId =>
              whenPermittedAndIsMemberAsync(requestUserId, permission, None, Some(centreId)) { () =>
                ask(processor, cmd).mapTo[ServiceValidation[ContainerEvent]].map { validation =>
                  for {
                    event     <- validation
                    container <- containerRepository.getByKey(ContainerId(event.id))
                    dto       <- containerToDto(container)
                  } yield dto
                }
              })
  }

  //
  // Invokes function "block" if user that invoked this service has the permission and membership
  // to do so.
  //
  private def whenContainerPermitted[T](
      requestUserId: UserId,
      containerId:   ContainerId
    )(block:         Container => ServiceValidation[T]
    ): ServiceValidation[T] =
    for {
      container <- containerRepository.getByKey(containerId)
      centreId  <- getContainerCentreId(containerId)
      permitted <- {
        whenPermittedAndIsMember(requestUserId, PermissionId.ContainerRead, None, Some(centreId))(
          () => block(container)
        )
      }
    } yield permitted

  private def filterContainersInternal(
      unfiltered: Seq[Container],
      filter:     FilterString,
      sort:       SortString
    ): ServiceValidation[Seq[Container]] = {
    val sortStr =
      if (sort.expression.isEmpty) new SortString("name")
      else sort

    for {
      containers <- ContainerFilter.filterContainers(unfiltered.toSet, filter)
      sortExpressions <- {
        QuerySortParser(sortStr).toSuccessNel(ServiceError(s"could not parse sort expression: $sort"))
      }
      sortFunc <- {
        Container.sort2Compare
          .get(sortExpressions(0).name)
          .toSuccessNel(ServiceError(s"invalid sort field: ${sortExpressions(0).name}"))
      }
    } yield {
      val result = containers.toSeq.sortWith(sortFunc)
      if (sortExpressions(0).order == AscendingOrder) result
      else result.reverse
    }
  }

  private def containerToDto(container: Container): ServiceValidation[ContainerDto] =
    containerTypeRepository.getByKey(container.containerTypeId).flatMap { containerType =>
      val containerTypeInfo = EntityInfoDto(containerType.id.id, containerType.slug, containerType.name)

      container match {
        case c: RootContainer =>
          for {
            centre       <- centreRepository.getByKey(c.centreId)
            locationName <- centre.locationName(c.locationId)
          } yield {
            val centreLocationInfo = CentreLocationInfo(centre.id.id, c.locationId.id, locationName)
            val constraintsDto     = c.constraints.flatMap(containerConstraintsToDto(_).toOption)
            RootContainerDto(id        = c.id.id,
                             version   = c.version,
                             timeAdded = c.timeAdded.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                             timeModified =
                               c.timeModified.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                             slug               = c.slug,
                             label              = c.label,
                             inventoryId        = c.inventoryId,
                             enabled            = c.enabled,
                             containerType      = containerTypeInfo,
                             centreLocationInfo = centreLocationInfo,
                             temperature        = c.temperature,
                             constraints        = constraintsDto)
          }

        case c: ChildContainer =>
          for {
            parent <- containerRepository.getByKey(c.parentId)
            schema <- containerSchemaRepository.getByKey(containerType.schemaId)
          } yield {
            val parentInfo  = ContainerInfoDto(parent.id.id, parent.slug.id, parent.getLabel)
            val positionDto = ContainerSchemaPositionDto(c.position, schema)

            c match {
              case c: StorageContainer =>
                val constraintsDto = c.constraints.flatMap(containerConstraintsToDto(_).toOption)

                StorageContainerDto(id        = c.id.id,
                                    version   = c.version,
                                    timeAdded = c.timeAdded.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                    timeModified =
                                      c.timeModified.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                                    slug          = c.slug,
                                    inventoryId   = c.inventoryId,
                                    enabled       = c.enabled,
                                    containerType = containerTypeInfo,
                                    parent        = parentInfo,
                                    position      = positionDto,
                                    constraints   = constraintsDto)

              case c: SpecimenContainer =>
                SpecimenContainerDto(id        = c.id.id,
                                     version   = c.version,
                                     timeAdded = c.timeAdded.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                     timeModified =
                                       c.timeModified.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                                     slug          = c.slug,
                                     inventoryId   = c.inventoryId,
                                     containerType = containerTypeInfo,
                                     parent        = parentInfo,
                                     position      = positionDto)
            }
          }
      }
    }

  private def containerConstraintsToDto(
      constraints: ContainerConstraints
    ): ServiceValidation[ContainerConstraintsDto] =
    centreRepository.getByKey(constraints.centreId).map { centre =>
      ContainerConstraintsDto(constraints, centre)
    }

  private def getContainerCentreId(containerId: ContainerId): ServiceValidation[CentreId] =
    containerRepository.getRootContainer(containerId).map(_.centreId)

}
