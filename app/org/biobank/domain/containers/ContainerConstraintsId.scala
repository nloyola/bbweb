package org.biobank.domain.containers

import play.api.libs.json._
import org.biobank.domain._

/** Identifies a unique [[domain.studies.ContainerConstraints ContainerConstraints]] in the system.
  *
  * Used as a value object to maintain associations to with other entities in the system.
  */
final case class ContainerConstraintsId(id: String) extends IdentifiedValueObject[String]

object ContainerConstraintsId {

  // Do not want JSON to create a sub object, we just want it to be converted
  // to a single string
  implicit val containerConstraintsIdFormat: Format[ContainerConstraintsId] = new Format[ContainerConstraintsId] {

      override def writes(id: ContainerConstraintsId): JsValue = JsString(id.id)

      override def reads(json: JsValue): JsResult[ContainerConstraintsId] =
        Reads.StringReads.reads(json).map(ContainerConstraintsId.apply _)
    }

}
