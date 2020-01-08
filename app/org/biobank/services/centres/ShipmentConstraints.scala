package org.biobank.services.centres

import org.biobank.services.ServiceValidation
import org.biobank._
import org.biobank.domain.LocationId
import org.biobank.domain.centres.{ShipmentId, ShipmentItemState, ShipmentSpecimen}
import org.biobank.domain.centres.ShipmentItemState._
import org.biobank.domain.participants.{Specimen, SpecimenRepository}
import org.biobank.query.centres.ShipmentSpecimensReadRepository
import scala.concurrent.{ExecutionContext, Future}
import scalaz._
import scalaz.Scalaz._

trait ShipmentConstraints {

  import org.biobank.CommonValidations._

  implicit val executionContext: ExecutionContext

  //private val log: Logger = LoggerFactory.getLogger(this.getClass)

  protected val shipmentSpecimensRepository: ShipmentSpecimensReadRepository

  protected val specimenRepository: SpecimenRepository

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  protected def specimensAtCentre(
      locationId: LocationId,
      specimens:  Specimen*
    ): ServiceValidation[List[Specimen]] =
    specimens
      .map { specimen =>
        if (locationId == specimen.locationId) specimen.successNel[String]
        else specimen.inventoryId.failureNel[Specimen]
      }.toList.sequenceU.leftMap(
        err =>
          EntityCriteriaError(s"invalid centre for specimen inventory IDs: " + err.list.toList.mkString(", ")).nel
      )

  /**
   * Checks that a specimen is not present in any shipment.
   */
  protected def specimensNotPresentInShipment(specimens: Specimen*): FutureValidation[Seq[Specimen]] = {
    val specimenIdMap = specimens.map(s => (s.id -> s)).toMap

    FutureValidation(
    shipmentSpecimensRepository
      .allForSpecimens(specimenIdMap.keys.toSeq: _*)
      .map { shipmentSpecimens =>
        val presentInShipments = shipmentSpecimens.filter(_.state == ShipmentItemState.Present)

        if (presentInShipments.isEmpty) {
          specimens.successNel[String]
        } else {
          val presentInventoryIds = presentInShipments.map(ss => specimenIdMap(ss.specimenId).inventoryId)
          EntityCriteriaError(
            s"specimens are already in an active shipment: " + presentInventoryIds.mkString(", ")
          ).failureNel[Seq[Specimen]]
        }
      }
      )
  }

  /**
   * Checks that a specimen is not present in a shipment.
   */
  protected def specimensNotInShipment(
      shipmentId: ShipmentId,
      specimens:  Specimen*
    ): FutureValidation[Seq[Specimen]] = {
    val specimenIdMap = specimens.map(s => (s.id -> s)).toMap

    for {
      shipmentSpecimens <- shipmentSpecimensRepository
      .getBySpecimens(shipmentId, specimenIdMap.keys.toSeq: _*)
      result <- {
        FutureValidation {
          if (shipmentSpecimens.isEmpty) {
            specimens.successNel[String]
          } else {
            val inventoryIds = shipmentSpecimens.map(ss => specimenIdMap(ss.specimenId).inventoryId)
            EntityCriteriaError(
              s"specimen inventory IDs already in this shipment: " + inventoryIds.mkString(", ")
            ).failureNel[Seq[Specimen]]
          }
        }
      }
    } yield result
  }

  /**
   *
   * @param shipSpecimenMap map of inventory ID to Shipment Specimen.
   *
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def getPackedShipmentSpecimens(
      shipSpecimenMap: Map[String, ShipmentSpecimen]
    ): FutureValidation[List[ShipmentSpecimen]] = {
    val r = shipSpecimenMap
      .map {
        case (inventoryId, shipSpecimen) =>
          shipSpecimen.isStatePresent.map(_ => shipSpecimen).leftMap(err => NonEmptyList(inventoryId))
      }
      .toList.sequenceU
      .leftMap(
        err => EntityCriteriaError("shipment specimens not present: " + err.list.toList.mkString(",")).nel
      )
    FutureValidation(r)
  }

  /**
   *
   * @param shipSpecimenMap map of inventory ID to Shipment Specimen.
   *
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def getNonPackedShipmentSpecimens(
      shipSpecimenMap: Map[String, ShipmentSpecimen]
    ): FutureValidation[List[ShipmentSpecimen]] = {
    val r = shipSpecimenMap
      .map {
        case (inventoryId, shipSpecimen) =>
          shipSpecimen.isStateNotPresent
            .map { _ =>
              shipSpecimen
            }.leftMap(err => NonEmptyList(inventoryId))
      }.toList.sequenceU.leftMap(
        err => EntityCriteriaError("shipment specimens are present: " + err.list.toList.mkString(",")).nel
      )
    FutureValidation(r)
  }

  def shipmentSpecimensPresent(
      shipmentId:           ShipmentId,
      specimens:  Specimen*
    ): FutureValidation[Seq[ShipmentSpecimen]] = {
    val specimenIdMap = specimens.map(s => (s.id -> s)).toMap

    for {
      ss <- shipmentSpecimensRepository.getBySpecimens(shipmentId, specimenIdMap.keys.toSeq: _*)
      map = ss.map(ss => specimenIdMap(ss.specimenId).inventoryId -> ss).toMap
      result <- getPackedShipmentSpecimens(map).map(_.toSeq)
    } yield result
  }

  def shipmentSpecimensNotPresent(
      shipmentId:           ShipmentId,
      specimens:  Specimen*
    ): FutureValidation[Seq[ShipmentSpecimen]] = {
    val specimenIdMap = specimens.map(s => (s.id -> s)).toMap

    for {
      ss <- shipmentSpecimensRepository.getBySpecimens(shipmentId, specimenIdMap.keys.toSeq: _*)
      map = ss.map(ss => specimenIdMap(ss.specimenId).inventoryId -> ss).toMap
      result <- getNonPackedShipmentSpecimens(map)
    } yield result
  }

  def shipmentSpecimenCount(shipmentId: ShipmentId, state: ShipmentItemState): Future[Int] = {
    shipmentSpecimensRepository.forShipment(shipmentId).map {
      _.filter(ss => ss.state == state).size
    }
  }

}
