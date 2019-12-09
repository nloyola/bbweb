package org.biobank.domain.studies

import org.biobank.domain.IdentifiedValueObject

import play.api.libs.json._

/** Identifies a unique [[ProcessingType]] in the system.
 *
 * Used as a value object to maintain associations to with objects in the system.
 */
final case class ProcessingTypeId(val id: String) extends IdentifiedValueObject[String] {}

object ProcessingTypeId {

  implicit val processingTypeIdFormat: Format[ProcessingTypeId] = new Format[ProcessingTypeId] {

    override def writes(id: ProcessingTypeId): JsValue = JsString(id.id)

    override def reads(json: JsValue): JsResult[ProcessingTypeId] =
      Reads.StringReads.reads(json).map(ProcessingTypeId.apply _)
  }

}
