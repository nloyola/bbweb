package org.biobank.domain.participants

import org.biobank.domain.IdentifiedValueObject

import play.api.libs.json._

/** Identifies a unique [[domain.participants.Specimen Specimen]] in the system.
 *
 * Used as a value object to maintain associations to with objects in the system.
 */
final case class SpecimenId(id: String) extends IdentifiedValueObject[String]

object SpecimenId {

  implicit val specimenIdFormat: Format[SpecimenId] = new Format[SpecimenId] {

    override def writes(id: SpecimenId): JsValue = JsString(id.id)

    override def reads(json: JsValue): JsResult[SpecimenId] =
      Reads.StringReads.reads(json).map(SpecimenId.apply _)
  }

}
