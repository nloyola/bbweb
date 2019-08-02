package org.biobank.controllers.centres

import org.biobank.domain.centres._
import org.biobank.fixtures.Url

private[centres] trait ShipmentsControllerSpecUtils {

  protected def uri(paths: String*): Url

  protected def uri(shipment: Shipment): Url = uri(shipment.id.id)

  protected def uri(shipment: Shipment, path: String): Url = uri(path, shipment.id.id)

}
