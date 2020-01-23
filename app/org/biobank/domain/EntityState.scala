package org.biobank.domain

import play.api.libs.json._

class EntityState(val id: String) extends AnyVal {
  override def toString: String = id
}

object EntityState {

  implicit val entityStateFormat: Format[EntityState] = new Format[EntityState] {

    override def writes(id: EntityState): JsValue = JsString(id.id)

    override def reads(json: JsValue): JsResult[EntityState] =
      Reads.StringReads.reads(json).map(new EntityState(_))
  }
}
