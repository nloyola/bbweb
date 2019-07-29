package org.biobank.domain.processing

import org.biobank.domain.IdentifiedValueObject
import play.api.libs.json._

final case class ProcessingEventId(id: String) extends IdentifiedValueObject[String]

object ProcessingEventId {

  implicit val processingEventIdReader: Reads[ProcessingEventId] =
    (__).read[String].map(ProcessingEventId(_))

}
