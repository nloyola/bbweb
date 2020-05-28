package org.biobank.services.centres

import cats.data._
import cats.implicits._
import org.biobank.domain.{EntityState, LocationId}
import org.biobank.domain.centres.{ShipmentId, ShipmentSpecimen}
import org.biobank.domain.participants.{Specimen, SpecimenRepository}
import org.biobank.dto.centres.ShipmentSpecimenDto
import org.biobank.query.centres.ShipmentSpecimensReadRepository
import org.biobank.validation.Validation._
import scala.concurrent.{ExecutionContext, Future}

trait ShipmentConstraints {

  implicit val executionContext: ExecutionContext

  //private val log: Logger = LoggerFactory.getLogger(this.getClass)

  protected val shipmentSpecimensRepository: ShipmentSpecimensReadRepository

  protected val specimenRepository: SpecimenRepository

  case class InvalidLocation(msg: String) extends ValidationError {
    def errorMessage: String = msg
  }

  case class SpecimensFoundInShipment(msg: String) extends ValidationError {
    def errorMessage: String = msg
  }

  case class SpecimenFoundInShipment(msg: String) extends ValidationError {
    def errorMessage: String = msg
  }

  case class InventoryIdNotFoundInShipment(msg: String) extends ValidationError {
    def errorMessage: String = msg
  }

  case class InventoryIdIsPresent(msg: String) extends ValidationError {
    def errorMessage: String = msg
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  protected def specimensAtCentre(
      locationId: LocationId,
      specimens:  Specimen*
    ): ValidationResult[List[Specimen]] = {
    specimens
      .map { specimen =>
        if (locationId == specimen.locationId) specimen.validNec
        else InvalidLocation(specimen.inventoryId).invalidNec
      }
      .toList.sequence
  }

  /**
   * Checks that a specimen is not present in any shipment.
   */
  protected def specimensNotPresentInShipment(specimens: Specimen*): FutureValidationResult[Seq[Specimen]] = {
    val specimenIdMap = specimens.map(s => (s.id -> s)).toMap

    val r = shipmentSpecimensRepository
      .allForSpecimens(specimenIdMap.keys.toSeq: _*)
      .map { shipmentSpecimens =>
        val presentInShipments = shipmentSpecimens.filter(_.state == ShipmentSpecimen.presentState)

        val v = if (presentInShipments.isEmpty) {
          specimens.validNec
        } else {
          val presentInventoryIds = presentInShipments.map { ss =>
            specimenIdMap(ss.specimenId).inventoryId
          }

          SpecimensFoundInShipment(
            s"specimens are already in an active shipment: " + presentInventoryIds.mkString(", ")
          ).invalidNec
        }
        v.toEither
      }
    EitherT(r)
  }

  /**
   * Checks that a specimen is not present in a shipment.
   */
  protected def specimensNotInShipment(
      shipmentId: ShipmentId,
      specimens:  Specimen*
    ): FutureValidationResult[Seq[Specimen]] = {
    val specimenIdMap = specimens.map(s => (s.id -> s)).toMap

    val shipmentSpecimens =
      shipmentSpecimensRepository.getBySpecimens(shipmentId, specimenIdMap.keys.toSeq: _*)

    val r = if (shipmentSpecimens.isEmpty) {
      specimens.validNec
    } else {
      val inventoryIds =
        shipmentSpecimens.map(ss => specimenIdMap(ss.specimenId).inventoryId)

      EntityCriteriaError(s"specimen inventory IDs already in this shipment: " + inventoryIds.mkString(", ")).invalidNec
    }
    EitherT(Future.successful(r.toEither))
  }

  /**
   *
   * @param shipSpecimenMap map of inventory ID to Shipment Specimen.
   *
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def getPackedShipmentSpecimens(
      shipSpecimenMap: Map[String, ShipmentSpecimenDto]
    ): FutureValidationResult[List[ShipmentSpecimenDto]] = {
    val r = shipSpecimenMap
      .map {
        case (inventoryId, shipSpecimen) =>
          if (shipSpecimen.state == ShipmentSpecimen.presentState) shipSpecimen.validNec
          else InventoryIdNotFoundInShipment(inventoryId).invalidNec
      }
      .toList.sequence.toEither

    EitherT(Future.successful(r))
  }

  /**
   *
   * @param shipSpecimenMap map of inventory ID to Shipment Specimen.
   *
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def getNonPackedShipmentSpecimens(
      shipSpecimenMap: Map[String, ShipmentSpecimenDto]
    ): FutureValidationResult[List[ShipmentSpecimenDto]] = {
    val r = shipSpecimenMap
      .map {
        case (inventoryId, shipSpecimen) =>
          if (shipSpecimen.state != ShipmentSpecimen.presentState) shipSpecimen.validNec
          else InventoryIdIsPresent(inventoryId).invalidNec
      }.toList.sequence.toEither

    EitherT(Future.successful(r))
  }

  def shipmentSpecimensPresent(
      shipmentId: ShipmentId,
      specimens:  Specimen*
    ): FutureValidationResult[Seq[ShipmentSpecimenDto]] = {
    val specimenIdMap = specimens.map(s => (s.id -> s)).toMap

    val ssMap = shipmentSpecimensRepository
      .getBySpecimens(shipmentId, specimenIdMap.keys.toSeq: _*).map { ss =>
        specimenIdMap(ss.specimenId).inventoryId -> ss
      }.toMap

    getPackedShipmentSpecimens(ssMap).map(_.toSeq)
  }

  def shipmentSpecimensNotPresent(
      shipmentId: ShipmentId,
      specimens:  Specimen*
    ): FutureValidationResult[Seq[ShipmentSpecimenDto]] = {
    val specimenIdMap = specimens.map(s => (s.id -> s)).toMap

    val ssMap = shipmentSpecimensRepository
      .getBySpecimens(shipmentId, specimenIdMap.keys.toSeq: _*).map { ss =>
        specimenIdMap(ss.specimenId).inventoryId -> ss
      }.toMap

    getNonPackedShipmentSpecimens(ssMap).map(_.toSeq)
  }

  def shipmentSpecimenCount(shipmentId: ShipmentId, state: EntityState): Future[Int] = {
    shipmentSpecimensRepository.forShipment(shipmentId).map {
      _.filter(ss => ss.state == state).size
    }
  }

}
