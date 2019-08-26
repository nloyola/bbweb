package org.biobank.domain.centres

import java.time.OffsetDateTime
import org.biobank._
import org.biobank.domain._
import org.biobank.domain.centres.ShipmentItemState._
import org.biobank.domain.containers.ContainerId
import play.api.libs.json._
import scalaz.Scalaz._

final case class ShipmentContainerId(id: String) extends IdentifiedValueObject[String]

object ShipmentContainerId {

  implicit val shipmentContainerIdReader: Reads[ShipmentContainerId] =
    (__ \ "id").read[String].map(ShipmentContainerId(_))

}

/**
 * Marks a specific [org.biobank.domain.containers.Container] as having been in a specific
 * [org.biobank.domain.centres.Shipment].
 *
 */
final case class ShipmentContainer(
    id:           ShipmentContainerId,
    version:      Long,
    timeAdded:    OffsetDateTime,
    timeModified: Option[OffsetDateTime],
    shipmentId:   ShipmentId,
    containerId:  ContainerId,
    state:        ShipmentItemState)
    extends ConcurrencySafeEntity[ShipmentContainerId] {}

object ShipmentContainer {
  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  case object ShipmentIdRequired extends ValidationKey

  def create(
      id:          ShipmentContainerId,
      version:     Long,
      shipmentId:  ShipmentId,
      containerId: ContainerId,
      state:       ShipmentItemState
    ): DomainValidation[ShipmentContainer] =
    validate(id, version, shipmentId, containerId)
      .map(_ => ShipmentContainer(id, version, OffsetDateTime.now, None, shipmentId, containerId, state))

  def validate(
      id:          ShipmentContainerId,
      version:     Long,
      shipmentId:  ShipmentId,
      containerId: ContainerId
    ): DomainValidation[Unit] =
    (validateId(id) |@|
      validateVersion(version) |@|
      validateId(shipmentId, ShipmentIdRequired) |@|
      validateId(containerId, ContainerIdInvalid)) {
      case _ => ()
    }
}
