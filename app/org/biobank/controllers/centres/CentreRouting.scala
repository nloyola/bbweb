package org.biobank.controllers.centres

import org.biobank.domain.centres.{CentreId, ShipmentId}
import org.biobank.domain.containers.ContainerId
import play.api.mvc.PathBindable.Parsing
import play.api.routing.sird._

object CentreRouting {

  implicit object bindableCentreId extends Parsing[CentreId](
    CentreId.apply,
    _.id,
    (key: String, e: Exception) => s"$key is not a valid centre Id"
  )

  implicit object bindableShipmentId extends Parsing[ShipmentId](
    ShipmentId.apply,
    _.id,
    (key: String, e: Exception) => s"$key is not a valid shipment Id"
  )

  implicit object bindableContainerId extends Parsing[ContainerId](
    ContainerId.apply,
    _.id,
    (key: String, e: Exception) => s"$key is not a valid container Id"
  )

  val centreId: PathBindableExtractor[CentreId]   = new PathBindableExtractor[CentreId]
  val shipmentId: PathBindableExtractor[ShipmentId] = new PathBindableExtractor[ShipmentId]
  val containerId: PathBindableExtractor[ContainerId] = new PathBindableExtractor[ContainerId]

}
