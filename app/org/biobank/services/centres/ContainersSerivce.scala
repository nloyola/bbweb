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

  /** All Top [[domain.containers.Container Containers]] for a [domain.centres.Centre Centre]. */
  def getContainers(
      requestUserId: UserId,
      centreId: CentreId,
      query: PagedQuery
    ): Future[ServiceValidation[PagedResults[ContainerDto]]]

  def processCommand(cmd: ContainerCommand): Future[ServiceValidation[ContainerDto]]
}

class ContainersServiceImpl @Inject()(
    @Named("containersProcessor") val processor: ActorRef,
    val accessService: AccessService,
    val centresService: CentresService,
    val centreRepository: CentreRepository,
    val containerTypeRepository: ContainerTypeRepository,
    val containerRepository: ContainerRepository)
    extends ContainersService
    with AccessChecksSerivce
    with ServicePermissionChecks {

  val log: Logger = LoggerFactory.getLogger(this.getClass)

  def getContainers(
      requestUserId: UserId,
      centreId: CentreId,
      query: PagedQuery
    ): Future[ServiceValidation[PagedResults[ContainerDto]]] = {
    Future {
      for {
        centre <- centresService.getCentre(requestUserId, centreId)
        rootContainers = containerRepository.rootContainers(centreId).toSeq
        containers <- filterContainersInternal(rootContainers, query.filter, query.sort)
        validPage <- query.validPage(containers.size)
        dtos <- containers.map(containerToDto(requestUserId, _)).toList.sequenceU.map(_.toSeq)
        result <- PagedResults.create(dtos, query.page, query.limit)
      } yield result
    }
  }

  def processCommand(cmd: ContainerCommand): Future[ServiceValidation[ContainerDto]] = {
    val validCentreId = cmd match {
      case c: AddRootContainerCmd =>
        for {
          centre <- centreRepository.getByKey(CentreId(c.centreId))
        } yield centre.id
      case c: AddSubContainerCommand => containerRepository.getSubContainerCentre(ContainerId(c.parentId))
      case c: ContainerModifyCommand => containerRepository.getSubContainerCentre(ContainerId(c.id))
    }

    val permission = cmd match {
      case c: AddContainerCommand => PermissionId.ContainerCreate
      case c                      => PermissionId.ContainerUpdate
    }

    val requestUserId = UserId(cmd.sessionUserId)

    validCentreId.fold(err => Future.successful((err.failure[ContainerDto])),
                       centreId =>
                         whenPermittedAndIsMemberAsync(requestUserId, permission, None, Some(centreId)) {
                           () =>
                             ask(processor, cmd).mapTo[ServiceValidation[ContainerEvent]].map { validation =>
                               for {
                                 event <- validation
                                 container <- containerRepository.getByKey(ContainerId(event.id))
                                 dto <- containerToDto(requestUserId, container)
                               } yield dto
                             }
                         })
  }

  private def filterContainersInternal(
      unfiltered: Seq[Container],
      filter: FilterString,
      sort: SortString
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

  private def containerToDto(requestUserId: UserId, container: Container): ServiceValidation[ContainerDto] = {
    container match {
      case c: StorageContainer =>
        for {
          containerType <- containerTypeRepository.getByKey(c.containerTypeId)
          //centre          <- c.constraints.flatMap(_.centreId.map(centreRepository.getByKey))
        } yield {
          val parentInfo = c.parentId.flatMap { containerId =>
            containerRepository
              .getByKey(containerId)
              .map((pc: Container) => EntityInfoDto(pc.id.id, pc.slug, pc.label))
              .toOption
          }
          val containerTypeInfo = EntityInfoDto(containerType.id.id, containerType.slug, containerType.name)
          StorageContainerDto(id = c.id.id,
                              version = c.version,
                              timeAdded = c.timeAdded.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                              timeModified =
                                c.timeModified.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                              slug = c.slug,
                              inventoryId = c.inventoryId,
                              label = c.label,
                              enabled = c.enabled,
                              sharedProperties = c.sharedProperties,
                              containerType = containerTypeInfo,
                              parent = parentInfo,
                              position = c.position,
                              constraints = None)
        }
      case c: SpecimenContainer =>
        ServiceError("not implemented yet").failureNel[ContainerDto]
    }
  }

}
