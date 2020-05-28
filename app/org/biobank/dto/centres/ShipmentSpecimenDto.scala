package org.biobank.dto.centres

import java.time.OffsetDateTime
import org.biobank.domain.{EntityState, HasState}
import org.biobank.domain.centres.{ShipmentContainerId, ShipmentId, ShipmentSpecimen, ShipmentSpecimenId}
import org.biobank.domain.participants.SpecimenId
import org.biobank.dto.EntityDto
import play.api.libs.json._

final case class ShipmentSpecimenDto(
    id:                  ShipmentSpecimenId,
    version:             Long,
    timeAdded:           OffsetDateTime,
    timeModified:        Option[OffsetDateTime],
    state:               EntityState,
    shipmentId:          ShipmentId,
    specimenId:          SpecimenId,
    shipmentContainerId: Option[ShipmentContainerId])
    extends EntityDto[ShipmentSpecimenId] with HasState {

  override def toString: String = s"|${this.getClass.getSimpleName}: ${Json.prettyPrint(Json.toJson(this))}"

}

object ShipmentSpecimenDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def from(shipmentSpecimen: ShipmentSpecimen): ShipmentSpecimenDto = {
    ShipmentSpecimenDto(id                  = shipmentSpecimen.id,
                        version             = shipmentSpecimen.version,
                        timeAdded           = shipmentSpecimen.timeAdded,
                        timeModified        = shipmentSpecimen.timeModified,
                        state               = shipmentSpecimen.state,
                        shipmentId          = shipmentSpecimen.shipmentId,
                        specimenId          = shipmentSpecimen.specimenId,
                        shipmentContainerId = shipmentSpecimen.shipmentContainerId)
  }

  val sort2Compare: Map[String, (ShipmentSpecimenDto, ShipmentSpecimenDto) => Boolean] =
    Map[String, (ShipmentSpecimenDto, ShipmentSpecimenDto) => Boolean]("state" -> compareByState,
                                                                       "inventoryId" -> compareByInventoryId
                                                                       //"specName"    -> compareBySpecName,
                                                                       //"timeCreated" -> compareByTimeCreated
    )

  def compareByState(a: ShipmentSpecimenDto, b: ShipmentSpecimenDto): Boolean =
    (a.state.id compareTo b.state.id) < 0

  def compareByInventoryId(a: ShipmentSpecimenDto, b: ShipmentSpecimenDto): Boolean = {
    // (a.specimen.inventoryId compareTo b.specimen.inventoryId) < 0

    // FIXME: temporary - change to valid impl soon
    (a.state.id compareTo b.state.id) < 0
  }

  // def compareBySpecName(a: ShipmentSpecimenDto, b: ShipmentSpecimenDto): Boolean = {
  //   (a.specimen.specimenDefinitionName compareTo b.specimen.specimenDefinitionName) < 0
  // }

  // def compareByTimeCreated(a: ShipmentSpecimenDto, b: ShipmentSpecimenDto): Boolean =
  //   (a.specimen.timeCreated compareTo b.specimen.timeCreated) < 0

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val shipmentSpecimenDtoFormat: Format[ShipmentSpecimenDto] = Json.format[ShipmentSpecimenDto]
}
