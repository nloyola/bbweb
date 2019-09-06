package org.biobank.domain.containers

import org.biobank.domain._
import play.api.libs.json._

/** Identifies a unique [[Container]] in the system.
 *
 * Used as a value object to maintain associations to with objects in the system.
 */
final case class ContainerId(val id: String) extends IdentifiedValueObject[String]

object ContainerId {

  // Do not want JSON to create a sub object, we just want it to be converted
  // to a single string
  implicit val containerIdFormat: Format[ContainerId] = new Format[ContainerId] {

    override def writes(id: ContainerId): JsValue = JsString(id.id)

    override def reads(json: JsValue): JsResult[ContainerId] =
      Reads.StringReads.reads(json).map(ContainerId.apply _)
  }

}
