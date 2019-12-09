package org.biobank.domain.studies

import org.biobank.domain.IdentifiedValueObject

import play.api.libs.json._

/** Identifies a unique [[CollectionEventType]] in the system.
 *
 * Used as a value object to maintain associations to with objects in the system.
 */
final case class CollectionEventTypeId(val id: String) extends IdentifiedValueObject[String] {}

object CollectionEventTypeId {

  implicit val collectionEventTypeIdFormat: Format[CollectionEventTypeId] =
    new Format[CollectionEventTypeId] {

      override def writes(id: CollectionEventTypeId): JsValue = JsString(id.id)

      override def reads(json: JsValue): JsResult[CollectionEventTypeId] =
        Reads.StringReads.reads(json).map(CollectionEventTypeId.apply _)
    }

}
