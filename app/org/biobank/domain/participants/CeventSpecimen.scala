package org.biobank.domain.participants

import play.api.libs.json._

/**
 *  Used to link a [[domain.participants.CollectionEvent CollectionEvent]] with one or more
 *  [[domain.participants.Specimen Specimens]].
 */
final case class CeventSpecimen(ceventId: CollectionEventId, specimenId: SpecimenId)

object CeventSpecimen {

  implicit val ceventSpecimenFormat: Format[CeventSpecimen] = Json.format[CeventSpecimen]

}
