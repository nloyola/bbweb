package org.biobank.dto.studies

import java.time.OffsetDateTime
import org.biobank.domain.{EntityState, HasName, HasOptionalDescription, HasState, Slug}
import org.biobank.domain.annotations.AnnotationType
import org.biobank.domain.studies.{Study, StudyId}
import org.biobank.dto.{EntityDto, EntityInfoAndState, EntitySetDto, NamedEntityInfo}
import play.api.libs.json._

final case class StudyDto(
    id:              StudyId,
    version:         Long,
    timeAdded:       OffsetDateTime,
    timeModified:    Option[OffsetDateTime],
    state:           EntityState,
    slug:            Slug,
    name:            String,
    description:     Option[String],
    annotationTypes: Set[AnnotationType])
    extends EntityDto[StudyId] with HasState with HasName with HasOptionalDescription

object StudyDto {

  def from(study: Study) =
    StudyDto(id              = study.id,
             version         = study.version,
             timeAdded       = study.timeAdded,
             timeModified    = study.timeModified,
             state           = study.state,
             slug            = study.slug,
             name            = study.name,
             description     = study.description,
             annotationTypes = study.annotationTypes)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val studyDtoFormat: Format[StudyDto] = Json.format[StudyDto]

}

final case class StudyInfoDto(id: StudyId, slug: Slug, name: String) extends NamedEntityInfo[StudyId]

object StudyInfoDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(study: Study): StudyInfoDto = StudyInfoDto(study.id, study.slug, study.name)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val studyInfoDtoFormat: Format[StudyInfoDto] = Json.format[StudyInfoDto]

}

final case class StudyInfoAndStateDto(id: StudyId, slug: Slug, name: String, state: EntityState)
    extends EntityInfoAndState[StudyId]

object StudyInfoAndStateDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(study: Study): StudyInfoAndStateDto =
    StudyInfoAndStateDto(study.id, study.slug, study.name, study.state)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val studyInfoAndStateDtoFormat: Format[StudyInfoAndStateDto] =
    Json.format[StudyInfoAndStateDto]

}

final case class StudySetDto(allEntities: Boolean, entityData: Set[StudyInfoDto])
    extends EntitySetDto[StudyInfoDto]

object StudySetDto {

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val studySetDtoFormat: Format[StudySetDto] = Json.format[StudySetDto]

}
