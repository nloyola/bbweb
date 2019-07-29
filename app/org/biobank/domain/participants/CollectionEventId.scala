package org.biobank.domain.participants

import org.biobank.domain.IdentifiedValueObject

import play.api.libs.json._
import play.api.libs.json.Reads._

/** Identifies a unique [[CollectionEvent]] in the system.
  *
  * Used as a value object to maintain associations to with objects in the system.
  */
final case class CollectionEventId(val id: String) extends IdentifiedValueObject[String] {}

object CollectionEventId {

  implicit val collectionEventIdReader: Reads[CollectionEventId] =
    (__).read[String].map( new CollectionEventId(_) )

}
