package org.biobank.domain.processing

import org.biobank.domain.IdentifiedValueObject
import org.biobank.domain.participants.SpecimenId
import play.api.libs.json._

final case class ProcessingEventInputSpecimenId(id: String) extends IdentifiedValueObject[String]

object ProcessingEventInputSpecimenId {

  implicit val processingEventInputSpecimenIdFormat: Format[ProcessingEventInputSpecimenId] =
    new Format[ProcessingEventInputSpecimenId] {

      override def writes(id: ProcessingEventInputSpecimenId): JsValue = JsString(id.id)

      override def reads(json: JsValue): JsResult[ProcessingEventInputSpecimenId] =
        Reads.StringReads.reads(json).map(ProcessingEventInputSpecimenId.apply _)
    }

}

final case class ProcessingEventInputSpecimen(
    id:                ProcessingEventInputSpecimenId,
    processingEventId: ProcessingEventId,
    specimenId:        SpecimenId)

object ProcessingEventInputSpecimen {

  implicit val processingEventInputSpecimenWrites: Writes[ProcessingEventInputSpecimen] =
    Json.writes[ProcessingEventInputSpecimen]

}
