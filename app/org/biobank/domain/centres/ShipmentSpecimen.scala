package org.biobank.domain.centres

import cats.data.Validated._
import cats.implicits._
import java.time.OffsetDateTime
import org.biobank.domain._
import org.biobank.domain.participants.SpecimenId
import org.biobank.validation.Validation._
import play.api.libs.json._
import play.api.libs.json.Reads._

final case class ShipmentSpecimenId(id: String) extends IdentifiedValueObject[String]

object ShipmentSpecimenId {

  // Do not want JSON to create a sub object, we just want it to be converted
  // to a single string
  implicit val shipmentSpecimenIdFormat: Format[ShipmentSpecimenId] = new Format[ShipmentSpecimenId] {

    override def writes(id: ShipmentSpecimenId): JsValue = JsString(id.id)

    override def reads(json: JsValue): JsResult[ShipmentSpecimenId] =
      Reads.StringReads.reads(json).map(ShipmentSpecimenId.apply _)
  }

}

trait ShipmentSpecimenValidations {

  case object ShipmentIdRequired extends ValidationError {
    def errorMessage: String = "shipment id cannot be empty"
  }

  case object ShipmentContainerIdEmpty extends ValidationError {
    def errorMessage: String = "shipment container id cannot be empty"
  }

  case object ShipmentSpecimenNotPresent extends ValidationError {
    def errorMessage: String = "specimen not in shipment"
  }

  case object SpecimenIdRequired extends ValidationError {
    def errorMessage: String = "specimen id is empty"
  }
}

/**
 * Marks a specific [org.biobank.domain.participants.Specimen] as having been in a specific
 * [org.biobank.domain.centres.Shipment].
 *
 */
@SuppressWarnings(
  Array("org.wartremover.warts.DefaultArguments",
        "org.wartremover.warts.JavaSerializable",
        "org.wartremover.warts.Product",
        "org.wartremover.warts.Serializable")
)
final case class ShipmentSpecimen(
    id:                  ShipmentSpecimenId,
    version:             Long,
    timeAdded:           OffsetDateTime,
    timeModified:        Option[OffsetDateTime],
    shipmentId:          ShipmentId,
    specimenId:          SpecimenId,
    state:               EntityState,
    shipmentContainerId: Option[ShipmentContainerId])
    extends ConcurrencySafeEntity[ShipmentSpecimenId] with ShipmentSpecimenValidations {

  import ShipmentSpecimen._

  def withShipmentContainer(
      id:           Option[ShipmentContainerId],
      timeModified: OffsetDateTime = OffsetDateTime.now
    ): ValidationResult[ShipmentSpecimen] =
    validateContainerId(id) map { _ =>
      copy(shipmentContainerId = id, version = version + 1, timeModified = Some(timeModified))
    }

  def present(timeModified: OffsetDateTime = OffsetDateTime.now): ValidationResult[ShipmentSpecimen] =
    if (state == ShipmentSpecimen.presentState) {
      Error(s"already in PRESENT state: $id").invalidNec
    } else {
      copy(state = presentState, version = version + 1, timeModified = Some(timeModified)).validNec
    }

  def received(timeModified: OffsetDateTime = OffsetDateTime.now): ValidationResult[ShipmentSpecimen] =
    if (state != ShipmentSpecimen.presentState) {
      Error(s"cannot change state to RECEIVED: invalid state: $state").invalidNec
    } else {
      copy(state = receivedState, version = version + 1, timeModified = Some(timeModified)).validNec
    }

  def missing(timeModified: OffsetDateTime = OffsetDateTime.now): ValidationResult[ShipmentSpecimen] =
    if (state != ShipmentSpecimen.presentState) {
      Error(s"cannot change state to MISSING: invalid state: $state").invalidNec
    } else {
      copy(state = missingState, version = version + 1, timeModified = Some(timeModified)).validNec
    }

  def extra(timeModified: OffsetDateTime = OffsetDateTime.now): ValidationResult[ShipmentSpecimen] =
    if (state != ShipmentSpecimen.presentState) {
      Error(s"cannot change state to EXTRA: invalid state: $state").invalidNec
    } else {
      copy(state = extraState, version = version + 1, timeModified = Some(timeModified)).validNec
    }

  def isStatePresent(): ValidationResult[Unit] =
    if (state == ShipmentSpecimen.presentState) ().validNec
    else Error(s"shipment specimen is not in present state").invalidNec

  def isStateNotPresent(): ValidationResult[Unit] =
    if (state != ShipmentSpecimen.presentState) ().validNec
    else Error(s"shipment specimen in present state").invalidNec

  def isStateExtra(): ValidationResult[Unit] =
    if (state == ShipmentSpecimen.extraState) ().validNec
    else Error(s"shipment specimen is not in extra state").invalidNec

  override def toString: String =
    s"""|${this.getClass.getSimpleName}: {
        |  id:                  $id,
        |  version:             $version,
        |  timeAdded:           $timeAdded,
        |  timeModified:        $timeModified,
        |  shipmentId:          $shipmentId,
        |  specimenId:          $specimenId,
        |  state:               $state,
        |  shipmentContainerId: $shipmentContainerId,
        |""".stripMargin
}

@SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
object ShipmentSpecimen extends ShipmentSpecimenValidations {

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val shipmentSpecimenFormat: Format[ShipmentSpecimen] = Json.format[ShipmentSpecimen]

  val presentState:  EntityState = new EntityState("present")
  val receivedState: EntityState = new EntityState("received")
  val missingState:  EntityState = new EntityState("missing")
  val extraState:    EntityState = new EntityState("extra")

  val states: List[EntityState] = List(presentState, receivedState, missingState, extraState)

  def compareByState(a: ShipmentSpecimen, b: ShipmentSpecimen): Boolean =
    (a.state.id compareTo b.state.id) < 0

  def create(
      id:                  ShipmentSpecimenId,
      version:             Long,
      timeAdded:           OffsetDateTime = OffsetDateTime.now,
      shipmentId:          ShipmentId,
      specimenId:          SpecimenId,
      state:               EntityState,
      shipmentContainerId: Option[ShipmentContainerId]
    ): ValidationResult[ShipmentSpecimen] =
    validate(id, version, shipmentId, specimenId, shipmentContainerId)
      .map(
        _ =>
          ShipmentSpecimen(id, version, timeAdded, None, shipmentId, specimenId, state, shipmentContainerId)
      )

  def validateContainerId(id: Option[ShipmentContainerId]): ValidationResult[Option[ShipmentContainerId]] =
    validateIdOption(id, ShipmentContainerIdEmpty)

  def validate(
      id:                  ShipmentSpecimenId,
      version:             Long,
      shipmentId:          ShipmentId,
      specimenId:          SpecimenId,
      shipmentContainerId: Option[ShipmentContainerId]
    ): ValidationResult[Unit] = {
    (validateId(id),
     validateVersion(version),
     validateId(shipmentId, ShipmentIdRequired),
     validateId(specimenId, SpecimenIdRequired),
     validateIdOption(shipmentContainerId, ShipmentContainerIdEmpty)).mapN((_, _, _, _, _) => ())
  }

  def makePresent(
      ss:           List[ShipmentSpecimen],
      timeModified: OffsetDateTime = OffsetDateTime.now
    ): ValidationResult[List[ShipmentSpecimen]] = {
    ss.map(_.present(timeModified)).sequence
  }

  def makeReceived(
      ss:           List[ShipmentSpecimen],
      timeModified: OffsetDateTime = OffsetDateTime.now
    ): ValidationResult[List[ShipmentSpecimen]] = {
    ss.map(_.received(timeModified)).sequence
  }

  def makeMissing(
      ss:           List[ShipmentSpecimen],
      timeModified: OffsetDateTime = OffsetDateTime.now
    ): ValidationResult[List[ShipmentSpecimen]] = {
    ss.map(_.missing(timeModified)).sequence
  }

  def makeExtra(
      ss:           List[ShipmentSpecimen],
      timeModified: OffsetDateTime = OffsetDateTime.now
    ): ValidationResult[List[ShipmentSpecimen]] = {
    ss.map(_.extra(timeModified)).sequence
  }
}

final case class ShipmentSpecimenCounts(specimens: Int, presentSpecimens: Int)
