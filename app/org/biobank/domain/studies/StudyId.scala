package org.biobank.domain.studies

import org.biobank.domain.IdentifiedValueObject
import play.api.libs.json._

/** Identifies a unique [[domain.studies.Study Study]] in the system.
 *
 * Used as a value object to maintain associations to with other entities in the system.
 */
final case class StudyId(id: String) extends IdentifiedValueObject[String]

object StudyId {

  implicit val studyIdRead: Reads[StudyId] = (__).read[String].map(StudyId(_))

}
