package org.biobank.domain.centres

import org.biobank.domain.IdentifiedValueObject

import play.api.libs.json._

/** Identifies a unique [[Centre]] in the system.
 *
 * Used as a value object to maintain associations to with objects in the system.
 */
final case class CentreId(id: String) extends IdentifiedValueObject[String]

object CentreId {

  implicit val centreIdReader: Reads[CentreId] = (__).read[String].map(CentreId(_))

}
