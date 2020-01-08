package org.biobank.services

import play.api.libs.json._

package centres {

  final case class CentreCountsByStatus(total: Long, disabledCount: Long, enabledCount: Long)

  object CentreCountsByStatus {

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val centreCountsByStatusFormat: Format[CentreCountsByStatus] = Json.format[CentreCountsByStatus]
  }

}
