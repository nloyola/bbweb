package org.biobank.dto.participants

import java.time.OffsetDateTime
import org.biobank.domain.Slug
import org.biobank.domain.annotations.Annotation
import org.biobank.domain.participants.{Participant, ParticipantId}
import org.biobank.domain.studies.Study
import org.biobank.dto.{EntityDto, EntityInfo}
import org.biobank.dto.studies.StudyInfoDto
import play.api.libs.json._

final case class ParticipantDto(
    id:           ParticipantId,
    slug:         Slug,
    study:        StudyInfoDto,
    version:      Long,
    timeAdded:    OffsetDateTime,
    timeModified: Option[OffsetDateTime],
    uniqueId:     String,
    annotations:  Set[Annotation])
    extends EntityDto[ParticipantId] {

  override def toString: String = s"|${this.getClass.getSimpleName}: ${Json.prettyPrint(Json.toJson(this))}"

}

object ParticipantDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(participant: Participant, study: Study): ParticipantDto =
    ParticipantDto(id           = participant.id,
                   slug         = participant.slug,
                   study        = StudyInfoDto(study),
                   version      = participant.version,
                   timeAdded    = participant.timeAdded,
                   timeModified = participant.timeModified,
                   uniqueId     = participant.uniqueId,
                   annotations  = participant.annotations)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val participantDtoFormat: Format[ParticipantDto] = Json.format[ParticipantDto]

}

final case class ParticipantInfoDto(id: ParticipantId, slug: Slug, uniqueId: String)
    extends EntityInfo[ParticipantId]

object ParticipantInfoDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(participant: Participant): ParticipantInfoDto =
    ParticipantInfoDto(participant.id, participant.slug, participant.uniqueId)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val participantInfoDtoFormat: Format[ParticipantInfoDto] = Json.format[ParticipantInfoDto]

}
