package org.biobank.dto.centres

import java.time.OffsetDateTime
import org.biobank.domain._
import org.biobank.domain.centres.{Centre, CentreId}
import org.biobank.domain.studies.Study
import org.biobank.dto.StudyInfoAndStateDto
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
    studyNames:   Set[StudyInfoAndStateDto],
    locations:    Set[Location])
    extends HasSlug with HasName

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
              studyNames   = studies.map(StudyInfoAndStateDto(_)),
              locations    = centre.locations)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val centreDtoFormat: Format[CentreDto] = Json.format[CentreDto]

}
