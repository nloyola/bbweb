package org.biobank.domain.participants

import org.biobank.domain.IdentifiedValueObject

import play.api.libs.json._
import play.api.libs.json.Reads._

/** Identifies a unique [[Participant]] in the system.
  *
  * Used as a value object to maintain associations to with objects in the system.
  */
final case class ParticipantId(id: String) extends IdentifiedValueObject[String]

object ParticipantId {

  implicit val participantIdReader: Reads[ParticipantId] =
    (__).read[String].map( new ParticipantId(_) )

}
