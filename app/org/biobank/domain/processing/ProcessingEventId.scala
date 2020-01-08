package org.biobank.domain.processing

import org.biobank.domain.IdentifiedValueObject
import play.api.libs.json._

final case class ProcessingEventId(id: String) extends IdentifiedValueObject[String]

object ProcessingEventId {

  implicit val processingEventIdFormat: Format[ProcessingEventId] = new Format[ProcessingEventId] {

    override def writes(id: ProcessingEventId): JsValue = JsString(id.id)

    override def reads(json: JsValue): JsResult[ProcessingEventId] =
      Reads.StringReads.reads(json).map(ProcessingEventId.apply _)
  }

}
