package org.biobank.dto.participants

import java.time.OffsetDateTime
import org.biobank.domain.{EntityState, Slug}
import org.biobank.domain.containers.ContainerId
import org.biobank.domain.participants.{CollectionEvent, Participant, Specimen, SpecimenId}
import org.biobank.domain.studies._
import org.biobank.dto._
import org.biobank.dto.centres.CentreLocationInfo
import play.api.libs.json._

final case class SpecimenDto(
    id:                      SpecimenId,
    version:                 Long,
    timeAdded:               OffsetDateTime,
    timeModified:            Option[OffsetDateTime],
    state:                   EntityState,
    slug:                    Slug,
    inventoryId:             String,
    collectionEvent:         CollectionEventInfoDto,
    specimenDefinitionId:    SpecimenDefinitionId,
    specimenDefinitionName:  String,
    specimenDefinitionUnits: String,
    originLocationInfo:      CentreLocationInfo,
    locationInfo:            CentreLocationInfo,
    containerId:             Option[ContainerId],
    label:                   Option[String],
    timeCreated:             OffsetDateTime,
    amount:                  BigDecimal,
    units:                   String,
    isDefaultAmount:         Boolean,
    study:                   StudyInfoDto,
    participant:             ParticipantInfoDto,
    eventType:               CollectionEventTypeInfoDto) {

  override def toString: String = s"${this.getClass.getSimpleName}: ${Json.prettyPrint(Json.toJson(this))}"

}

object SpecimenDto {

  def from(
      specimen:           Specimen,
      event:              CollectionEvent,
      eventType:          CollectionEventType,
      specimenDefinition: CollectedSpecimenDefinition,
      originLocationInfo: CentreLocationInfo,
      locationInfo:       CentreLocationInfo,
      study:              Study,
      participant:        Participant
    ): SpecimenDto = {
    SpecimenDto(id                      = specimen.id,
                version                 = specimen.version,
                timeAdded               = specimen.timeAdded,
                timeModified            = specimen.timeModified,
                state                   = specimen.state,
                slug                    = specimen.slug,
                inventoryId             = specimen.inventoryId,
                collectionEvent         = CollectionEventInfoDto(event),
                specimenDefinitionId    = specimen.specimenDefinitionId,
                specimenDefinitionName  = specimenDefinition.name,
                specimenDefinitionUnits = specimenDefinition.units,
                originLocationInfo      = originLocationInfo,
                locationInfo            = locationInfo,
                containerId             = specimen.containerId,
                label                   = specimen.schemaLabel.map(_.label),
                timeCreated             = specimen.timeCreated,
                amount                  = specimen.amount,
                units                   = specimenDefinition.units,
                isDefaultAmount         = (specimen.amount == specimenDefinition.amount),
                eventType               = CollectionEventTypeInfoDto(eventType),
                study                   = StudyInfoDto(study),
                participant             = ParticipantInfoDto(participant))
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val specimenDtoFormat: Format[SpecimenDto] = Json.format[SpecimenDto]

}
