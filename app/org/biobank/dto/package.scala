package org.biobank

import java.time.OffsetDateTime
import org.biobank.domain._
import play.api.libs.json._

package dto {

  trait Dto

  trait EntityDto[ID] extends Dto {
    val id:           ID
    val version:      Long
    val timeAdded:    OffsetDateTime
    val timeModified: Option[OffsetDateTime]
  }

  trait EntityInfo[ID] extends HasSlug {
    val id: ID
  }

  trait NamedEntityInfo[ID] extends EntityInfo[ID] with HasName

  trait EntitySetDto[T <: NamedEntityInfo[_]] {
    val allEntities: Boolean
    val entityData:  Set[T]
  }

  trait EntityInfoAndState[ID] extends NamedEntityInfo[ID] with HasState

  final case class AggregateCountsDto(studies: Long, centres: Long, users: Long)

  object AggregateCountsDto {

    implicit val aggregateCountsDtoWriter: Writes[AggregateCountsDto] = Json.writes[AggregateCountsDto]

  }

}
