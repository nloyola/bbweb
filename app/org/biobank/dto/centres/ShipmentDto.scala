package org.biobank.dto.centres

import java.time.OffsetDateTime
import org.biobank.domain.EntityState
import org.biobank.domain.centres.{Shipment, ShipmentId}
import org.biobank.dto.Dto
import play.api.libs.json._

final case class ShipmentDto(
    id:                   ShipmentId,
    version:              Long,
    timeAdded:            OffsetDateTime,
    timeModified:         Option[OffsetDateTime],
    state:                EntityState,
    courierName:          String,
    trackingNumber:       String,
    origin:               CentreLocationInfo,
    destination:          CentreLocationInfo,
    timePacked:           Option[OffsetDateTime],
    timeSent:             Option[OffsetDateTime],
    timeReceived:         Option[OffsetDateTime],
    timeUnpacked:         Option[OffsetDateTime],
    timeCompleted:        Option[OffsetDateTime],
    specimenCount:        Int,
    presentSpecimenCount: Int,
    containerCount:       Int)
    extends Dto {

  override def toString: String = s"|${this.getClass.getSimpleName}: ${Json.prettyPrint(Json.toJson(this))}"

}

object ShipmentDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(
      shipment:             Shipment,
      origin:               CentreLocationInfo,
      destination:          CentreLocationInfo,
      specimenCount:        Int,
      presentSpecimenCount: Int,
      containerCount:       Int
    ): ShipmentDto =
    ShipmentDto(id                   = shipment.id,
                version              = shipment.version,
                timeAdded            = shipment.timeAdded,
                timeModified         = shipment.timeModified,
                courierName          = shipment.courierName,
                trackingNumber       = shipment.trackingNumber,
                state                = shipment.state,
                origin               = origin,
                destination          = destination,
                timePacked           = shipment.timePacked,
                timeSent             = shipment.timeSent,
                timeReceived         = shipment.timeReceived,
                timeUnpacked         = shipment.timeUnpacked,
                timeCompleted        = shipment.timeCompleted,
                specimenCount        = specimenCount,
                presentSpecimenCount = presentSpecimenCount,
                containerCount       = containerCount)

  val sort2Compare: Map[String, (ShipmentDto, ShipmentDto) => Boolean] =
    Map[String, (ShipmentDto, ShipmentDto) => Boolean]("courierName"    -> compareByCourier,
                                                       "trackingNumber" -> compareByTrackingNumber,
                                                       "state"          -> compareByState,
                                                       "origin"         -> compareByOrigin,
                                                       "destination"    -> compareByDestination)

  def compareByCourier(a: ShipmentDto, b: ShipmentDto): Boolean =
    (a.courierName compareToIgnoreCase b.courierName) < 0

  def compareByTrackingNumber(a: ShipmentDto, b: ShipmentDto): Boolean =
    (a.trackingNumber compareToIgnoreCase b.trackingNumber) < 0

  def compareByState(a: ShipmentDto, b: ShipmentDto): Boolean =
    (a.state.toString compareToIgnoreCase b.state.toString) < 0

  def compareByOrigin(a: ShipmentDto, b: ShipmentDto): Boolean =
    (a.origin.combinedName compareToIgnoreCase b.origin.combinedName) < 0

  def compareByDestination(a: ShipmentDto, b: ShipmentDto): Boolean =
    (a.destination.combinedName compareToIgnoreCase b.destination.combinedName) < 0

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val shipmentDtoFormat: Format[ShipmentDto] = Json.format[ShipmentDto]

}
