package org.biobank.services

import play.api.libs.json._

package users {

  final case class UserCountsByStatus(
      total:           Long,
      registeredCount: Long,
      activeCount:     Long,
      lockedCount:     Long)

  object UserCountsByStatus {

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val userCountsByStatusForamt: Format[UserCountsByStatus] = Json.format[UserCountsByStatus]
  }

}
