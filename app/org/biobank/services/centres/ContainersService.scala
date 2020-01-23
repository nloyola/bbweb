package org.biobank.services.centres

import akka.actor.ActorRef
import akka.pattern.ask
import com.google.inject.ImplementedBy
import javax.inject.{Inject, Named}
import org.biobank._
import org.biobank.domain.Slug
import org.biobank.domain.access.PermissionId
import org.biobank.domain.centres.{CentreId, CentreRepository}
import org.biobank.domain.containers._
import org.biobank.domain.users.UserId
import org.biobank.dto.containers._
import org.biobank.infrastructure.AscendingOrder
import org.biobank.infrastructure.commands.ContainerCommands._
import org.biobank.infrastructure.events.ContainerEvents._
import org.biobank.services._
import org.biobank.services.access.AccessService
import org.slf4j.{Logger, LoggerFactory}
import scala.concurrent._
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._

@ImplementedBy(classOf[ContainersServiceImpl])
trait ContainersService extends BbwebService {

  def getBySlug(requestUserId: UserId, slug: Slug): FutureValidation[ContainerDto]

  /** Searches for [[domain.containers.RootContainer RootContainers]] for a [domain.centres.Centre Centre]. */
  def search(
      requestUserId: UserId,
      centreId:      CentreId,
      query:         PagedQuery
    ): FutureValidation[PagedResults[ContainerDto]]

  def getChildrenBySlug(requestUserId: UserId, slug: Slug): FutureValidation[ContainerChildrenInfo]

  def processCommand(cmd: ContainerCommand): FutureValidation[ContainerDto]

  def processRemoveCommand(cmd: RemoveContainerCmd): FutureValidation[Boolean]

  def snapshotRequest(requestUserId: UserId): ServiceValidation[Unit]
}

@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
class ContainersServiceImpl @Inject()(
    @Named("containersProcessor") val processor: ActorRef,
    val accessService:                           AccessService,
    val centresService:                          CentresService,
    val centreRepository:                        CentreRepository,
    val containerTypeRepository:                 ContainerTypeRepository,
    val containerRepository:                     ContainerRepository,
    val containerSchemaRepository:               ContainerSchemaRepository
  )(
    implicit
    val executionContext: ExecutionContext)
    extends ContainersService with AccessChecksSerivce with ServicePermissionChecks {

  val log: Logger = LoggerFactory.getLogger(this.getClass)

  def getBySlug(requestUserId: UserId, slug: Slug): FutureValidation[ContainerDto] =
    FutureValidation {
      whenContainerPermitted(requestUserId, slug) { container =>
        containerToDto(container)
      }
    }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def search(
      requestUserId: UserId,
      centreId:      CentreId,
      query:         PagedQuery
    ): FutureValidation[PagedResults[ContainerDto]] =
    FutureValidation {
      for {
        centre <- centresService.getCentre(requestUserId, centreId)
        rootContainers = containerRepository.rootContainers(centreId).toSeq
        containers <- filterContainersInternal(rootContainers, query.filter, query.sort)
        validPage  <- query.validPage(containers.size)
        dtos       <- containers.map(containerToDto).toList.sequenceU.map(_.toSeq)
        result     <- PagedResults.create(dtos, query.page, query.limit)
      } yield result
    }

  def getChildrenBySlug(requestUserId: UserId, slug: Slug): FutureValidation[ContainerChildrenInfo] = {
    FutureValidation {
      whenContainerPermitted(requestUserId, slug) { container =>
        containerRepository.getChildren(container.id).map { children =>
          ContainerChildrenInfo(container, children.map(ContainerInfo(_)))
        }
      }
    }
  }

  def processCommand(cmd: ContainerCommand): FutureValidation[ContainerDto] = {
    val validCentreId = cmd match {
      case c: AddRootContainerCmd    => centreRepository.getByKey(CentreId(c.centreId)).map(_.id)
      case c: AddSubContainerCommand => getContainerCentreId(ContainerId(c.parentId))
      case c: ContainerModifyCommand => getContainerCentreId(ContainerId(c.id))
    }

    val permission = cmd match {
      case c: AddContainerCommand => PermissionId.ContainerCreate
      case _ => PermissionId.ContainerUpdate
    }

    val requestUserId = UserId(cmd.sessionUserId)

    validCentreId
      .fold(err => FutureValidation((err.failure[ContainerDto])),
            centreId =>
              whenPermittedAndIsMemberAsync(requestUserId, permission, None, Some(centreId)) { () =>
                for {
                  event     <- FutureValidation(ask(processor, cmd).mapTo[ServiceValidation[ContainerEvent]])
                  container <- FutureValidation(containerRepository.getByKey(ContainerId(event.id)))
                  dto       <- FutureValidation(containerToDto(container))
                } yield dto
              })
  }

  def processRemoveCommand(cmd: RemoveContainerCmd): FutureValidation[Boolean] = {
    val validCentre = for {
      container     <- containerRepository.getByKey(ContainerId(cmd.id))
      containerType <- containerTypeRepository.getByKey(container.containerTypeId)
      centre        <- centreRepository.getByKey(containerType.centreId)
    } yield centre

    validCentre
      .fold(err => FutureValidation((err.failure[Boolean])),
            centre =>
              whenPermittedAndIsMemberAsync(UserId(cmd.sessionUserId),
                                            PermissionId.ContainerDelete,
                                            None,
                                            Some(centre.id)) { () =>
                FutureValidation(ask(processor, cmd).mapTo[ServiceValidation[ContainerEvent]]).map(_ => true)
              })
  }

  // Invokes function "block" if user that invoked this service has the permission and membership to do so.
  private def whenContainerPermitted[T](
      requestUserId: UserId,
      containerSlug: Slug
    )(block:         Container => ServiceValidation[T]
    ): ServiceValidation[T] =
    for {
      container <- containerRepository.getBySlug(containerSlug)
      centreId  <- getContainerCentreId(container.id)
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
      if (sort.expression.isEmpty) new SortString("label")
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
      container match {
        case c: RootContainer =>
          for {
            centre   <- centreRepository.getByKey(c.centreId)
            location <- centre.locationWithId(c.locationId)
          } yield RootContainerDto(c, containerType, centre, location)

        case c: ChildContainer =>
          for {
            parent <- containerRepository.getByKey(c.parentId)
          } yield {
            c match {
              case c: StorageContainer  => StorageContainerDto(c, containerType, parent)
              case c: SpecimenContainer => SpecimenContainerDto(c, containerType, parent)
            }
          }
      }
    }

  private def getContainerCentreId(containerId: ContainerId): ServiceValidation[CentreId] =
    containerRepository.getRootContainer(containerId).map(_.centreId)

}
