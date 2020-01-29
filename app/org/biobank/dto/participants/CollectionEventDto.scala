package org.biobank.dto.participants

import java.time.OffsetDateTime
import org.biobank.domain.Slug
import org.biobank.domain.annotations.Annotation
import org.biobank.domain.studies.CollectionEventType
import org.biobank.domain.participants.{CollectionEvent, CollectionEventId, Participant}
import org.biobank.dto.{EntityDto, EntityInfo}
import play.api.libs.json._

final case class CollectionEventDto(
    id:                      CollectionEventId,
    version:                 Long,
    timeAdded:               OffsetDateTime,
    timeModified:            Option[OffsetDateTime],
    slug:                    Slug,
    participantId:           String,
    participantSlug:         String,
    collectionEventTypeId:   String,
    collectionEventTypeSlug: String,
    timeCompleted:           OffsetDateTime,
    visitNumber:             Int,
    annotations:             Set[Annotation])
    extends EntityDto[CollectionEventId]

object CollectionEventDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(
      event:       CollectionEvent,
      participant: Participant,
      eventType:   CollectionEventType
    ): CollectionEventDto =
    CollectionEventDto(id                      = event.id,
                       participantId           = participant.id.id,
                       participantSlug         = participant.slug.id,
                       collectionEventTypeId   = eventType.id.id,
                       collectionEventTypeSlug = eventType.slug.id,
                       version                 = event.version,
                       timeAdded               = event.timeAdded,
                       timeModified            = event.timeModified,
                       slug                    = event.slug,
                       timeCompleted           = event.timeCompleted,
                       visitNumber             = event.visitNumber,
                       annotations             = event.annotations)

  implicit val collectionEventDtoWriter: Writes[CollectionEventDto] = Json.writes[CollectionEventDto]

}

final case class CollectionEventInfoDto(id: CollectionEventId, slug: Slug, visitNumber: Int)
    extends EntityInfo[CollectionEventId]

object CollectionEventInfoDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(collectionEvent: CollectionEvent): CollectionEventInfoDto =
    CollectionEventInfoDto(collectionEvent.id, collectionEvent.slug, collectionEvent.visitNumber)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val collectionEventInfoDtoFormat: Format[CollectionEventInfoDto] =
    Json.format[CollectionEventInfoDto]

}
