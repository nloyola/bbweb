package org.biobank.services.centres

import akka.actor._
import akka.pattern.ask
import cats.data._
import cats.implicits._
import com.google.inject.ImplementedBy
import javax.inject.{Inject, Named}
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
import org.biobank.validation.Validation._
import org.slf4j.{Logger, LoggerFactory}
import scala.concurrent._

@ImplementedBy(classOf[ShipmentsServiceImpl])
trait ShipmentsService extends BbwebService {

  def getShipment(requestUserId: UserId, id: ShipmentId): FutureValidationResult[ShipmentDto]

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
  def getShipments(
      requestUserId: UserId,
      pagedQuery:    PagedQuery
    ): FutureValidationResult[PagedResults[ShipmentDto]]

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
    ): FutureValidationResult[PagedResults[ShipmentSpecimenDto]]

  def shipmentCanAddSpecimen(
      requestUserId:      UserId,
      shipmentId:         ShipmentId,
      shipmentSpecimenId: String
    ): FutureValidationResult[SpecimenDto]

  def getShipmentSpecimen(
      requestUserId:      UserId,
      shipmentId:         ShipmentId,
      shipmentSpecimenId: ShipmentSpecimenId
    ): FutureValidationResult[ShipmentSpecimenDto]

  def processCommand(cmd: ShipmentCommand): FutureValidationResult[ShipmentDto]

  def removeShipment(cmd: ShipmentRemoveCmd): FutureValidationResult[Boolean]

  def processShipmentSpecimenCommand(cmd: ShipmentSpecimenCommand): FutureValidationResult[ShipmentDto]

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

  def getShipment(requestUserId: UserId, id: ShipmentId): FutureValidationResult[ShipmentDto] = {
    shipmentWhenPermitted(requestUserId, id)
  }

  /**
   * Sorting should be done by controller since the DTO has additional fields to sort by.
   *
   */
  def getShipments(
      requestUserId: UserId,
      query:         PagedQuery
    ): FutureValidationResult[PagedResults[ShipmentDto]] = {

    def internal(shipments: Seq[ShipmentDto]): FutureValidationResult[Set[ShipmentDto]] = {
      val v = shipmentFilter.filterShipments(shipments.toSet, query.filter).andThen { filtered =>
        query.validPageCats(filtered.size).map(_ => filtered)
      }

      EitherT(Future.successful(v.toEither))
    }

    def sort(dtos: Seq[ShipmentDto]): FutureValidationResult[Seq[ShipmentDto]] = {
      val sortStr =
        if (query.sort.expression.isEmpty) new SortString("courierName")
        else query.sort

      val v1 = Validated
        .fromOption(QuerySortParser(sortStr), Error(s"could not parse sort expression: ${query.sort}"))
        .toValidatedNec

      val v3 = v1.andThen { sortExpressions =>
        val v2 = Validated
          .fromOption(ShipmentDto.sort2Compare.get(sortExpressions(0).name),
                      Error(s"invalid sort field: ${sortExpressions(0).name}"))
          .toValidatedNec

        v2.map(sortFunc => (sortExpressions, sortFunc))
      }

      val v4 = v3.map {
        case (sortExpressions, sortFunc) =>
          val results = dtos.sortWith(sortFunc)
          if (sortExpressions(0).order == AscendingOrder) results
          else results.reverse
      }
      EitherT(Future.successful(v4.toEither))
    }

    val vcentreIds = EitherT(permittedShippingCentresAsync(requestUserId).futval.map {
      _ match {
        case scalaz.Failure(err)     => Either.left(NonEmptyChain(Error(err.list.toList.mkString(","))))
        case scalaz.Success(centres) => Either.right(centres.map(_.id))
      }
    })

    for {
      centreIds <- vcentreIds
      shipments <- EitherT(shipmentsReadRepository.withCentres(centreIds).map(fs => Either.right(fs)))
      filtered  <- internal(shipments)
      sorted    <- sort(filtered.toSeq)
      result    <- EitherT(Future.successful(PagedResults.createCats(sorted, query.page, query.limit).toEither))
    } yield result
  }

  def shipmentCanAddSpecimen(
      requestUserId:       UserId,
      shipmentId:          ShipmentId,
      specimenInventoryId: String
    ): FutureValidationResult[SpecimenDto] = {
    def internal(shipment: ShipmentDto): ValidationResult[(Specimen, SpecimenDto)] = {
      import scalaz.Scalaz._
      import scalaz.Validation.FlatMap._

      val v = for {
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

      v match {
        case scalaz.Success((specimen, dto)) => (specimen, dto).validNec
        case scalaz.Failure(err)             => Error(err.list.toList.mkString(",")).invalidNec
      }
    }

    for {
      shipment <- shipmentWhenPermitted(requestUserId, shipmentId)
      result   <- EitherT(Future.successful(internal(shipment).toEither))
      present  <- specimensNotPresentInShipment(result._1)
    } yield result._2
  }

  def getShipmentSpecimen(
      requestUserId:      UserId,
      shipmentId:         ShipmentId,
      shipmentSpecimenId: ShipmentSpecimenId
    ): FutureValidationResult[ShipmentSpecimenDto] = {
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
    ): FutureValidationResult[PagedResults[ShipmentSpecimenDto]] = {

    def internal(
        shipmentSpecimens: Seq[ShipmentSpecimenDto]
      ): FutureValidationResult[Set[ShipmentSpecimenDto]] = {
      val v = ShipmentSpecimenFilter.filterShipmentSpecimens(shipmentSpecimens.toSet, query.filter).andThen {
        filtered =>
          query.validPageCats(filtered.size).map(_ => filtered)
      }

      EitherT(Future.successful(v.toEither))
    }

    def sort(dtos: Seq[ShipmentSpecimenDto]): FutureValidationResult[Seq[ShipmentSpecimenDto]] = {
      val sortStr =
        if (query.sort.expression.isEmpty) new SortString("state")
        else query.sort

      val v1 = Validated
        .fromOption(QuerySortParser(sortStr), Error(s"could not parse sort expression: ${query.sort}"))
        .toValidatedNec

      val v3 = v1.andThen { sortExpressions =>
        val v2 = Validated
          .fromOption(ShipmentSpecimenDto.sort2Compare.get(sortExpressions(0).name),
                      Error(s"invalid sort field: ${sortExpressions(0).name}"))
          .toValidatedNec

        v2.map(sortFunc => (sortExpressions, sortFunc))
      }

      val v4 = v3.map {
        case (sortExpressions, sortFunc) =>
          val results = dtos.sortWith(sortFunc)
          if (sortExpressions(0).order == AscendingOrder) results
          else results.reverse
      }
      EitherT(Future.successful(v4.toEither))
    }

    for {
      shipment          <- shipmentWhenPermitted(requestUserId, shipmentId)
      shipmentSpecimens <- EitherT.right(shipmentSpecimensRepository.forShipment(shipmentId))
      filtered          <- internal(shipmentSpecimens)
      sorted            <- sort(filtered.toSeq)
      result            <- EitherT(Future.successful(PagedResults.createCats(sorted, query.page, query.limit).toEither))
    } yield result
  }

  def processCommand(cmd: ShipmentCommand): FutureValidationResult[ShipmentDto] = {
    cmd match {
      case c: ShipmentRemoveCmd =>
        EitherT.left(Future.successful(NonEmptyChain(Error(s"invalid service call: $c, use removeShipment"))))
      case c =>
        whenShipmentPermittedAsync(c) { () =>
          for {
            shipment <- EitherT(ask(processor, c).mapTo[ValidationResult[Shipment]].map(v => v.toEither))
            dto      <- shipmentToDto(shipment)
          } yield dto
        }
    }
  }

  def removeShipment(cmd: ShipmentRemoveCmd): FutureValidationResult[Boolean] = {
    whenShipmentPermittedAsync(cmd) { () =>
      EitherT(ask(processor, cmd).mapTo[ValidationResult[Shipment]].map { v =>
        v.map(_ => true).toEither
      })
    }
  }

  def processShipmentSpecimenCommand(cmd: ShipmentSpecimenCommand): FutureValidationResult[ShipmentDto] = {
    whenShipmentPermittedAsync(cmd) { () =>
      for {
        shipment <- EitherT(ask(processor, cmd).mapTo[ValidationResult[Shipment]].map(v => v.toEither))
        dto      <- shipmentToDto(shipment)
      } yield dto
    }
  }

  private def shipmentWhenPermitted(
      requestUserId: UserId,
      shipmentId:    ShipmentId
    ): FutureValidationResult[ShipmentDto] = {
    import scalaz.Validation.FlatMap._

    shipmentsReadRepository.getByKey(shipmentId).flatMap { shipment =>
      val v: DomainValidation[ShipmentDto] = for {
        originCentre      <- centreRepository.getByLocationId(shipment.origin.location.id)
        destinationCentre <- centreRepository.getByLocationId(shipment.destination.location.id)
        hasMembership     <- accessService.hasMembership(requestUserId, None, Some(originCentre.id))
        hasAccess <- accessService.hasPermissionAndIsMember(requestUserId,
                                                            PermissionId.ShipmentRead,
                                                            None,
                                                            Some(destinationCentre.id))
      } yield shipment

      val v2 = v match {
        case scalaz.Success(shipment) => shipment.validNec
        case scalaz.Failure(err)      => Error(err.list.toList.mkString(",")).invalidNec
      }

      EitherT(Future.successful(v2.toEither))
    }
  }

  case class ShipmentCentreIds(fromId: CentreId, toId: CentreId)

  private def validCentresIds(shipmentId: ShipmentId): FutureValidationResult[ShipmentCentreIds] = {
    shipmentsReadRepository.getByKey(shipmentId).flatMap { shipment =>
      import scalaz.Validation.FlatMap._

      val v = for {
        originCentre      <- centreRepository.getByLocationId(shipment.origin.location.id)
        destinationCentre <- centreRepository.getByLocationId(shipment.destination.location.id)
      } yield ShipmentCentreIds(originCentre.id, destinationCentre.id)

      val v2 = v match {
        case scalaz.Success(centreIds) => centreIds.validNec
        case scalaz.Failure(err)       => Error(err.list.toList.mkString(",")).invalidNec
      }

      EitherT(Future.successful(v2.toEither))
    }
  }

  private def isMemberOfCentres(userId: UserId, centreIds: ShipmentCentreIds): ValidationResult[Unit] = {
    import scalaz.Validation.FlatMap._

    val v = for {
      fromMember <- accessService.hasMembership(userId, None, Some(centreIds.fromId))
      toMember   <- accessService.hasMembership(userId, None, Some(centreIds.toId))
    } yield ()

    v match {
      case scalaz.Success(_)   => ().validNec
      case scalaz.Failure(err) => Error(err.list.toList.mkString(",")).invalidNec
    }
  }

  //
  // Invokes function "block" if user that issued the command has the permission and membership
  // to do so.
  //
  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  private def whenShipmentPermittedAsync[T](
      cmd:   ShipmentCommand
    )(block: () => FutureValidationResult[T]
    ): FutureValidationResult[T] = {
    import scalaz.Validation.FlatMap._

    val sessionUserId = UserId(cmd.sessionUserId)
    val vCentreIds = cmd match {
      case c: ShipmentModifyCommand => validCentresIds(ShipmentId(c.id))
      case c: AddShipmentCmd =>
        val v = for {
          originCentre      <- centreRepository.getByLocationId(LocationId(c.originLocationId))
          destinationCentre <- centreRepository.getByLocationId(LocationId(c.destinationLocationId))
        } yield ShipmentCentreIds(originCentre.id, destinationCentre.id)
        val v2 = v match {
          case scalaz.Success(centreIds) => centreIds.validNec
          case scalaz.Failure(err)       => Error(err.list.toList.mkString(",")).invalidNec
        }
        EitherT(Future.successful(v2.toEither))
    }

    val permission = cmd match {
      case c: AddShipmentCmd    => PermissionId.ShipmentCreate
      case c: ShipmentRemoveCmd => PermissionId.ShipmentDelete
      case c => PermissionId.ShipmentUpdate
    }

    for {
      centreIds <- vCentreIds
      isMember  <- EitherT(Future.successful(isMemberOfCentres(sessionUserId, centreIds).toEither))
      result    <- whenPermittedAsyncCats(sessionUserId, permission)(block)
    } yield result
  }

  //
  // Invokes function "block" if user that issued the command has the permission and membership
  // to do so.
  //
  private def whenShipmentPermittedAsync[T](
      cmd:   ShipmentSpecimenCommand
    )(block: () => FutureValidationResult[T]
    ): FutureValidationResult[T] = {

    val sessionUserId = UserId(cmd.sessionUserId)
    val shipmentId    = ShipmentId(cmd.shipmentId)

    for {
      centreIds <- validCentresIds(shipmentId)
      isMember  <- EitherT(Future.successful(isMemberOfCentres(sessionUserId, centreIds).toEither))
      result    <- whenPermittedAsyncCats(sessionUserId, PermissionId.ShipmentUpdate)(block)
    } yield result
  }

  private def shipmentToDto(
      shipment: Shipment,
      counts:   ShipmentSpecimenCounts
    ): ValidationResult[ShipmentDto] = {
    import scalaz.Validation.FlatMap._

    val v = for {
      originCentre        <- centreRepository.getByLocationId(shipment.originLocationId)
      originLocation      <- originCentre.locationWithId(shipment.originLocationId)
      destinationCentre   <- centreRepository.getByLocationId(shipment.destinationLocationId)
      destinationLocation <- destinationCentre.locationWithId(shipment.destinationLocationId)
    } yield ShipmentSourceDest(originCentre, originLocation, destinationCentre, destinationLocation)

    v match {
      case scalaz.Failure(err)     => Error(err.list.toList.mkString(",")).invalidNec
      case scalaz.Success(srcDest) =>
        // TODO: update with container count when ready
        ShipmentDto(shipment,
                    CentreLocationInfo(srcDest.originCentre, srcDest.originLocation),
                    CentreLocationInfo(srcDest.destinationCentre, srcDest.destinationLocation),
                    counts.specimens,
                    counts.presentSpecimens,
                    0).validNec
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  private def shipmentToDto(shipment: Shipment): FutureValidationResult[ShipmentDto] = {
    val v = shipmentSpecimensRepository.countsForShipments(shipment.id).map { aMap =>
      aMap.get(shipment.id) match {
        case Some(counts) => counts
        case None         => ShipmentSpecimenCounts(0, 0)
      }
    }
    EitherT(v.map(shipmentToDto(shipment, _).toEither))
  }

}
