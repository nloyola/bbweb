package org.biobank.domain.containers

import org.biobank.domain.IdentifiedValueObject
import play.api.libs.json._

final case class ContainerSchemaId(val id: String) extends IdentifiedValueObject[String]

object ContainerSchemaId {

  // Do not want JSON to create a sub object, we just want it to be converted
  // to a single string
  implicit val containerSchemaIdFormat: Format[ContainerSchemaId] = new Format[ContainerSchemaId] {

    override def writes(id: ContainerSchemaId): JsValue = JsString(id.id)

    override def reads(json: JsValue): JsResult[ContainerSchemaId] =
      Reads.StringReads.reads(json).map(ContainerSchemaId.apply _)
  }

}
