package org.biobank.services.centres

import akka.actor._
import akka.pattern.ask
import com.google.inject.ImplementedBy
import javax.inject.{Inject, Named}
import org.biobank._
import org.biobank.domain._
import org.biobank.domain.centres._
import org.biobank.domain.access.PermissionId
import org.biobank.domain.participants._
import org.biobank.domain.studies.CollectionEventTypeRepository
import org.biobank.domain.users.UserId
import org.biobank.dto.participants.SpecimenDto
import org.biobank.dto.centres.{CentreLocationInfo, ShipmentDto, ShipmentSpecimenDto}
import org.biobank.infrastructure.AscendingOrder
import org.biobank.infrastructure.commands.ShipmentCommands._
import org.biobank.infrastructure.commands.ShipmentSpecimenCommands._
import org.biobank.query.centres.{ShipmentSpecimensReadRepository, ShipmentsReadRepository}
import org.biobank.services._
import org.biobank.services.access.AccessService
import org.biobank.services.participants.SpecimensService
import org.slf4j.{Logger, LoggerFactory}
import scala.concurrent._
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._
import scalaz._

@ImplementedBy(classOf[ShipmentsServiceImpl])
trait ShipmentsService extends BbwebService {

  def getShipment(requestUserId: UserId, id: ShipmentId): FutureValidation[ShipmentDto]

  /**
   * Returns a set of shipments to or from a Centre. The shipments can be filtered and or sorted using
   * expressions.
   *
   * @param centreId the ID of the centre the shipments belong to.
   *
   * @param filter the string representation of the filter expression to use to filter the shipments.
   *
   * @param sort the string representation of the sort expression to use when sorting the shipments.
   */
  def getShipments(requestUserId: UserId, pagedQuery: PagedQuery): FutureValidation[PagedResults[ShipmentDto]]

  /**
   * Returns a set of shipment specimens. The entities can be filtered and or sorted using expressions.
   *
   * @param shipmentId the ID of the shipment the shipment specimens belong to.
   *
   * @param filter the string representation of the filter expression to use to filter the shipment specimens
   *               in the shipment.
   */
  def getShipmentSpecimens(
      requestUserId: UserId,
      shipmentId:    ShipmentId,
      pagedQuery:    PagedQuery
    ): FutureValidation[PagedResults[ShipmentSpecimenDto]]

  def shipmentCanAddSpecimen(
      requestUserId:      UserId,
      shipmentId:         ShipmentId,
      shipmentSpecimenId: String
    ): FutureValidation[SpecimenDto]

  def getShipmentSpecimen(
      requestUserId:      UserId,
      shipmentId:         ShipmentId,
      shipmentSpecimenId: ShipmentSpecimenId
    ): FutureValidation[ShipmentSpecimenDto]

  def processCommand(cmd: ShipmentCommand): FutureValidation[ShipmentDto]

  def removeShipment(cmd: ShipmentRemoveCmd): FutureValidation[Boolean]

  def processShipmentSpecimenCommand(cmd: ShipmentSpecimenCommand): FutureValidation[ShipmentDto]

  def snapshotRequest(requestUserId: UserId): ServiceValidation[Unit]

}

/**
 * Handles all commands dealing with shipments, shipment specimens, and shipment containers.
 */
@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
class ShipmentsServiceImpl @Inject()(
    @Named("shipmentsProcessor") val processor: ActorRef,
    val accessService:                          AccessService,
    val centreRepository:                       CentreRepository,
    protected val shipmentsReadRepository:      ShipmentsReadRepository,
    protected val shipmentSpecimensRepository:  ShipmentSpecimensReadRepository,
    val specimenRepository:                     SpecimenRepository,
    val ceventSpecimenRepository:               CeventSpecimenRepository,
    val collectionEventRepository:              CollectionEventRepository,
    val collectionEventTypeRepository:          CollectionEventTypeRepository,
    val specimensService:                       SpecimensService,
    val shipmentFilter:                         ShipmentFilter
  )(
    implicit
    val executionContext: ExecutionContext)
    extends ShipmentsService with AccessChecksSerivce with CentreServicePermissionChecks
    with ShipmentConstraints {

  import org.biobank.domain.access.AccessItem._

  val log: Logger = LoggerFactory.getLogger(this.getClass)

  def getShipment(requestUserId: UserId, id: ShipmentId): FutureValidation[ShipmentDto] = {
    shipmentWhenPermitted(requestUserId, id)
  }

  /**
   * Sorting should be done by controller since the DTO has additional fields to sort by.
   *
   */
  def getShipments(requestUserId: UserId, query: PagedQuery): FutureValidation[PagedResults[ShipmentDto]] = {
    def internal(shipments: Seq[ShipmentDto]): FutureValidation[Set[ShipmentDto]] =
      FutureValidation {
        for {
          filtered  <- shipmentFilter.filterShipments(shipments.toSet, query.filter)
          validPage <- query.validPage(filtered.size)
        } yield filtered
      }

    def sort(dtos: Seq[ShipmentDto]): FutureValidation[Seq[ShipmentDto]] =
      FutureValidation {
        val sortStr =
          if (query.sort.expression.isEmpty) new SortString("courierName")
          else query.sort

        for {
          sortExpressions <- QuerySortParser(sortStr)
                              .toSuccessNel(ServiceError(s"could not parse sort expression: ${query.sort}"))

          sortFunc <- ShipmentDto.sort2Compare
                       .get(sortExpressions(0).name).toSuccessNel(
                         ServiceError(s"invalid sort field: ${sortExpressions(0).name}")
                       )
        } yield {
          val results = dtos.sortWith(sortFunc)
          if (sortExpressions(0).order == AscendingOrder) results
          else results.reverse
        }
      }

    for {
      centreIds <- permittedShippingCentresAsync(requestUserId).map(_.map(_.id))
      shipments <- FutureValidation(shipmentsReadRepository.withCentres(centreIds).map(_.successNel[String]))
      filtered  <- internal(shipments)
      sorted    <- sort(filtered.toSeq)
      result    <- FutureValidation { PagedResults.create(sorted, query.page, query.limit) }
    } yield result
  }

  def shipmentCanAddSpecimen(
      requestUserId:       UserId,
      shipmentId:          ShipmentId,
      specimenInventoryId: String
    ): FutureValidation[SpecimenDto] = {
    def internal(shipment: ShipmentDto): ServiceValidation[(Specimen, SpecimenDto)] = {
      for {
        specimen <- specimensService.getByInventoryId(requestUserId, specimenInventoryId)
        sameLocation <- {
          if (shipment.origin.location.id == specimen.locationId) {
            specimen.successNel[ServiceError]
          } else {
            ServiceError(s"specimen not at shipment's from location").failureNel[Specimen]
          }
        }
        dto <- specimensService.get(requestUserId, specimen.id)
      } yield (specimen, dto)
    }

    for {
      shipment <- shipmentWhenPermitted(requestUserId, shipmentId)
      result   <- FutureValidation { internal(shipment) }
      present  <- specimensNotPresentInShipment(result._1)
    } yield result._2
  }

  def getShipmentSpecimen(
      requestUserId:      UserId,
      shipmentId:         ShipmentId,
      shipmentSpecimenId: ShipmentSpecimenId
    ): FutureValidation[ShipmentSpecimenDto] = {
    for {
      shipment         <- shipmentWhenPermitted(requestUserId, shipmentId)
      shipmentSpecimen <- shipmentSpecimensRepository.getByKey(shipmentSpecimenId)
    } yield shipmentSpecimen
  }

  /**
   * Sorting should be done by controller since the DTO has additional fields to sort by.
   *
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def getShipmentSpecimens(
      requestUserId: UserId,
      shipmentId:    ShipmentId,
      query:         PagedQuery
    ): FutureValidation[PagedResults[ShipmentSpecimenDto]] = {
    def internal(shipment: ShipmentDto) = {
      shipmentSpecimensRepository.forShipment(shipmentId).map { shipmentSpecimens =>
        val sortStr =
          if (query.sort.expression.isEmpty) new SortString("state")
          else query.sort

        for {
          filtered  <- ShipmentSpecimenFilter.filterShipmentSpecimens(shipmentSpecimens.toSet, query.filter)
          validPage <- query.validPage(filtered.size)
          sortExpressions <- QuerySortParser(sortStr)
                              .toSuccessNel(ServiceError(s"could not parse sort expression: ${query.sort}"))
          sortFunc <- ShipmentSpecimenDto.sort2Compare
                       .get(sortExpressions(0).name).toSuccessNel(
                         ServiceError(s"invalid sort field: ${sortExpressions(0).name}")
                       )
          results <- {
            val results = filtered.toList.sortWith(sortFunc)
            val sortedResults =
              if (sortExpressions(0).order == AscendingOrder) results
              else results.reverse
            PagedResults.create(sortedResults, query.page, query.limit)
          }
        } yield results
      }
    }

    for {
      shipment <- shipmentWhenPermitted(requestUserId, shipmentId)
      result <- FutureValidation {
                 internal(shipment)
               }
    } yield result
  }

  def processCommand(cmd: ShipmentCommand): FutureValidation[ShipmentDto] = {
    cmd match {
      case c: ShipmentRemoveCmd =>
        FutureValidation(
          Future
            .successful(ServiceError(s"invalid service call: $c, use removeShipment").failureNel[ShipmentDto])
        )
      case c =>
        whenShipmentPermittedAsync(c) { () =>
          for {
            shipment <- FutureValidation(ask(processor, c).mapTo[ServiceValidation[Shipment]])
            dto      <- shipmentToDto(shipment)
          } yield dto
        }
    }
  }

  def removeShipment(cmd: ShipmentRemoveCmd): FutureValidation[Boolean] = {
    whenShipmentPermittedAsync(cmd) { () =>
      FutureValidation(ask(processor, cmd).mapTo[ServiceValidation[Shipment]].map { validation =>
        validation.map(_ => true)
      })
    }
  }

  def processShipmentSpecimenCommand(cmd: ShipmentSpecimenCommand): FutureValidation[ShipmentDto] = {
    whenShipmentPermittedAsync(cmd) { () =>
      for {
        shipment <- FutureValidation(ask(processor, cmd).mapTo[ServiceValidation[Shipment]])
        dto      <- shipmentToDto(shipment)
      } yield dto
    }
  }

  private def shipmentWhenPermitted(
      requestUserId: UserId,
      shipmentId:    ShipmentId
    ): FutureValidation[ShipmentDto] = {
    for {
      shipment <- shipmentsReadRepository.getByKey(shipmentId)
      result <- FutureValidation {
                 for {
                   originCentre      <- centreRepository.getByLocationId(shipment.origin.location.id)
                   destinationCentre <- centreRepository.getByLocationId(shipment.destination.location.id)
                   hasMembership     <- accessService.hasMembership(requestUserId, None, Some(originCentre.id))
                   hasAccess <- accessService.hasPermissionAndIsMember(requestUserId,
                                                                       PermissionId.ShipmentRead,
                                                                       None,
                                                                       Some(destinationCentre.id))
                 } yield shipment
               }
    } yield result
  }

  case class ShipmentCentreIds(fromId: CentreId, toId: CentreId)

  private def validCentresIds(shipmentId: ShipmentId): FutureValidation[ShipmentCentreIds] = {
    shipmentsReadRepository.getByKey(shipmentId).flatMap { shipment =>
      FutureValidation {
        for {
          originCentre      <- centreRepository.getByLocationId(shipment.origin.location.id)
          destinationCentre <- centreRepository.getByLocationId(shipment.destination.location.id)
        } yield ShipmentCentreIds(originCentre.id, destinationCentre.id)
      }
    }
  }

  private def isMemberOfCentres(userId: UserId, centreIds: ShipmentCentreIds): ServiceValidation[Unit] = {
    for {
      fromMember <- accessService.hasMembership(userId, None, Some(centreIds.fromId))
      toMember   <- accessService.hasMembership(userId, None, Some(centreIds.toId))
    } yield ()
  }

  //
  // Invokes function "block" if user that issued the command has the permission and membership
  // to do so.
  //
  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  private def whenShipmentPermittedAsync[T](
      cmd:   ShipmentCommand
    )(block: () => FutureValidation[T]
    ): FutureValidation[T] = {

    val sessionUserId = UserId(cmd.sessionUserId)
    val validCentreIds: FutureValidation[ShipmentCentreIds] = cmd match {
      case c: ShipmentModifyCommand => validCentresIds(ShipmentId(c.id))
      case c: AddShipmentCmd =>
        FutureValidation {
          for {
            originCentre      <- centreRepository.getByLocationId(LocationId(c.originLocationId))
            destinationCentre <- centreRepository.getByLocationId(LocationId(c.destinationLocationId))
          } yield ShipmentCentreIds(originCentre.id, destinationCentre.id)
        }
    }

    val permission = cmd match {
      case c: AddShipmentCmd    => PermissionId.ShipmentCreate
      case c: ShipmentRemoveCmd => PermissionId.ShipmentDelete
      case c => PermissionId.ShipmentUpdate
    }

    for {
      centreIds <- validCentreIds
      isMember  <- FutureValidation { isMemberOfCentres(sessionUserId, centreIds) }
      result    <- whenPermittedAsync(sessionUserId, permission)(block)
    } yield result
  }

  //
  // Invokes function "block" if user that issued the command has the permission and membership
  // to do so.
  //
  private def whenShipmentPermittedAsync[T](
      cmd:   ShipmentSpecimenCommand
    )(block: () => FutureValidation[T]
    ): FutureValidation[T] = {

    val sessionUserId = UserId(cmd.sessionUserId)
    val shipmentId    = ShipmentId(cmd.shipmentId)

    for {
      centreIds <- validCentresIds(shipmentId)
      isMember  <- FutureValidation { isMemberOfCentres(sessionUserId, centreIds) }
      result    <- whenPermittedAsync(sessionUserId, PermissionId.ShipmentUpdate)(block)
    } yield result
  }

  private def shipmentToDto(
      shipment: Shipment,
      counts:   ShipmentSpecimenCounts
    ): ServiceValidation[ShipmentDto] = {
    for {
      originCentre        <- centreRepository.getByLocationId(shipment.originLocationId)
      originLocation      <- originCentre.locationWithId(shipment.originLocationId)
      destinationCentre   <- centreRepository.getByLocationId(shipment.destinationLocationId)
      destinationLocation <- destinationCentre.locationWithId(shipment.destinationLocationId)
    } yield {
      // TODO: update with container count when ready
      ShipmentDto(shipment,
                  CentreLocationInfo(originCentre, originLocation),
                  CentreLocationInfo(destinationCentre, destinationLocation),
                  counts.specimens,
                  counts.presentSpecimens,
                  0)
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  private def shipmentToDto(shipment: Shipment): FutureValidation[ShipmentDto] = {
    val f = shipmentSpecimensRepository.countsForShipments(shipment.id).map { counts =>
      shipmentToDto(shipment, counts.getOrElse(shipment.id, ShipmentSpecimenCounts(0, 0)))
    }
    FutureValidation(f)
  }

  // private def shipmentSpecimenToDto(
  //     requestUserId:    UserId,
  //     shipmentSpecimen: ShipmentSpecimen
  //   ): ServiceValidation[ShipmentSpecimenDto] =
  //   specimensService.get(requestUserId, shipmentSpecimen.specimenId).map { specimen =>
  //     //ShipmentSpecimenDto(shipmentSpecimen, specimen)
  //     ShipmentSpecimenDto.from(shipmentSpecimen)
  //   }

}
