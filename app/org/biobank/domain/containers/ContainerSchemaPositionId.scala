package org.biobank.domain.containers

import org.biobank.domain._

import play.api.libs.json._
import play.api.libs.json.Reads._

/** Identifies a unique [[domain.participants.Specimen Specimen]] in the system.
 *
 * Used as a value object to maintain associations to with objects in the system.
 */
final case class ContainerSchemaPositionId(id: String) extends IdentifiedValueObject[String]

object ContainerSchemaPositionId {

  implicit val containerSchemaPositionIdReader: Reads[ContainerSchemaPositionId] =
    (__).read[String].map(ContainerSchemaPositionId(_))

}
