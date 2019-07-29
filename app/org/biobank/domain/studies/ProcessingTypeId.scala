package org.biobank.domain.studies

import org.biobank.domain.IdentifiedValueObject

import play.api.libs.json._
import play.api.libs.json.Reads._

/** Identifies a unique [[ProcessingType]] in the system.
  *
  * Used as a value object to maintain associations to with objects in the system.
  */
final case class ProcessingTypeId(val id: String) extends IdentifiedValueObject[String] {}

object ProcessingTypeId {

  implicit val processingTypeIdRead: Reads[ProcessingTypeId] =
    (__).read[String].map( new ProcessingTypeId(_) )

}
