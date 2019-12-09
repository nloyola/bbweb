package org.biobank.domain.users

import org.biobank.domain.IdentifiedValueObject

import play.api.libs.json._

final case class UserId(id: String) extends IdentifiedValueObject[String] {}

object UserId {

  implicit val userIdFormat: Format[UserId] = new Format[UserId] {

    override def writes(id: UserId): JsValue = JsString(id.id)

    override def reads(json: JsValue): JsResult[UserId] =
      Reads.StringReads.reads(json).map(UserId.apply _)
  }

}
