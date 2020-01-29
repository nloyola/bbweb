package org.biobank.dto.centres

import java.time.OffsetDateTime
import org.biobank.domain.{
  EntityState,
  HasName,
  HasNamePredicates,
  HasOptionalDescription,
  HasState,
  Location,
  Slug
}
import org.biobank.domain.centres.{Centre, CentreId}
import org.biobank.domain.studies.Study
import org.biobank.dto.{EntityDto, EntityInfoAndState, EntitySetDto, NamedEntityInfo}
import org.biobank.dto.studies.StudyInfoAndStateDto
import play.api.libs.json._

/**
 * Predicates that can be used to filter collections of centres.
 *
 */
trait CentrePredicates extends HasNamePredicates[CentreDto] {

  type CentreFilter = CentreDto => Boolean

}

final case class CentreDto(
    id:           CentreId,
    version:      Long,
    timeAdded:    OffsetDateTime,
    timeModified: Option[OffsetDateTime],
    state:        EntityState,
    slug:         Slug,
    name:         String,
    description:  Option[String],
    studies:      Set[StudyInfoAndStateDto],
    locations:    Set[Location])
    extends EntityDto[CentreId] with HasState with HasName with HasOptionalDescription {

  override def toString: String = s"|${this.getClass.getSimpleName}: ${Json.prettyPrint(Json.toJson(this))}"

}

object CentreDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(centre: Centre, studies: Set[Study]): CentreDto =
    CentreDto(id           = centre.id,
              version      = centre.version,
              timeAdded    = centre.timeAdded,
              timeModified = centre.timeModified,
              state        = centre.state,
              slug         = centre.slug,
              name         = centre.name,
              description  = centre.description,
              studies      = studies.map(StudyInfoAndStateDto(_)),
              locations    = centre.locations)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val centreDtoFormat: Format[CentreDto] = Json.format[CentreDto]

}

final case class CentreInfoDto(id: CentreId, slug: Slug, name: String) extends NamedEntityInfo[CentreId]

object CentreInfoDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(centre: Centre): CentreInfoDto = CentreInfoDto(centre.id, centre.slug, centre.name)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val centreInfoDtoFormat: Format[CentreInfoDto] = Json.format[CentreInfoDto]

}

final case class CentreSetDto(allEntities: Boolean, entityData: Set[CentreInfoDto])
    extends EntitySetDto[CentreInfoDto]

object CentreSetDto {

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val centreSetDtoFormat: Format[CentreSetDto] = Json.format[CentreSetDto]

}

final case class CentreInfoAndStateDto(id: CentreId, slug: Slug, name: String, state: EntityState)
    extends EntityInfoAndState[CentreId]

object CentreInfoAndStateDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(centre: Centre): CentreInfoAndStateDto =
    CentreInfoAndStateDto(centre.id, centre.slug, centre.name, centre.state)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val centreInfoAndStateDtoFormat: Format[CentreInfoAndStateDto] =
    Json.format[CentreInfoAndStateDto]

}
