package org.biobank.domain.users

import org.biobank.domain.IdentifiedValueObject

import play.api.libs.json._

final case class UserId(id: String) extends IdentifiedValueObject[String] {}

object UserId {

  implicit val userIdReader: Reads[UserId] = (__).read[String].map(UserId(_))

}
