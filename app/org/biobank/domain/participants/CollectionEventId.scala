package org.biobank.domain.participants

import org.biobank.domain.IdentifiedValueObject

import play.api.libs.json._

/** Identifies a unique [[CollectionEvent]] in the system.
 *
 * Used as a value object to maintain associations to with objects in the system.
 */
final case class CollectionEventId(val id: String) extends IdentifiedValueObject[String] {}

object CollectionEventId {

  implicit val collectionEventIdFormat: Format[CollectionEventId] = new Format[CollectionEventId] {

    override def writes(id: CollectionEventId): JsValue = JsString(id.id)

    override def reads(json: JsValue): JsResult[CollectionEventId] =
      Reads.StringReads.reads(json).map(CollectionEventId.apply _)
  }

}
