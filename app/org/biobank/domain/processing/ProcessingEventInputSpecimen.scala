package org.biobank.domain.processing

import org.biobank.domain.IdentifiedValueObject
import org.biobank.domain.participants.SpecimenId
import play.api.libs.json._

final case class ProcessingEventInputSpecimenId(id: String) extends IdentifiedValueObject[String]

object ProcessingEventInputSpecimenId {

  implicit val processingEventInputSpecimenIdReader: Reads[ProcessingEventInputSpecimenId] =
    (__).read[String].map(ProcessingEventInputSpecimenId(_))

}

final case class ProcessingEventInputSpecimen(id:                ProcessingEventInputSpecimenId,
                                              processingEventId: ProcessingEventId,
                                              specimenId:        SpecimenId)

object ProcessingEventInputSpecimen {

  implicit val processingEventInputSpecimenWrites: Writes[ProcessingEventInputSpecimen] =
    Json.writes[ProcessingEventInputSpecimen]

}
