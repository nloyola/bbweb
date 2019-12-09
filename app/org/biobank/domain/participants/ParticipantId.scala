package org.biobank.domain.participants

import org.biobank.domain.IdentifiedValueObject

import play.api.libs.json._

/** Identifies a unique [[Participant]] in the system.
 *
 * Used as a value object to maintain associations to with objects in the system.
 */
final case class ParticipantId(id: String) extends IdentifiedValueObject[String]

object ParticipantId {

  implicit val participantIdFormat: Format[ParticipantId] = new Format[ParticipantId] {

    override def writes(id: ParticipantId): JsValue = JsString(id.id)

    override def reads(json: JsValue): JsResult[ParticipantId] =
      Reads.StringReads.reads(json).map(ParticipantId.apply _)
  }

}
