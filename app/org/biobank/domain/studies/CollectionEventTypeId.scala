package org.biobank.domain.studies

import org.biobank.domain.IdentifiedValueObject

import play.api.libs.json._
import play.api.libs.json.Reads._

/** Identifies a unique [[CollectionEventType]] in the system.
  *
  * Used as a value object to maintain associations to with objects in the system.
  */
final case class CollectionEventTypeId(val id: String) extends IdentifiedValueObject[String] {}

object CollectionEventTypeId {

  implicit val collectionEventTypeIdReader: Reads[CollectionEventTypeId] =
    (__).read[String].map( new CollectionEventTypeId(_) )

}
