package org.biobank.dto.studies

import java.time.OffsetDateTime
import org.biobank.domain._
import org.biobank.domain.annotations.AnnotationType
import org.biobank.domain.studies.{CollectedSpecimenDefinition, CollectionEventType, CollectionEventTypeId}
import org.biobank.dto.{EntityDto, NamedEntityInfo}
import play.api.libs.json._

final case class CollectionEventTypeDto(
    id:                  CollectionEventTypeId,
    version:             Long,
    timeAdded:           OffsetDateTime,
    timeModified:        Option[OffsetDateTime],
    slug:                Slug,
    name:                String,
    description:         Option[String],
    recurring:           Boolean,
    study:               StudyInfoDto,
    specimenDefinitions: Set[CollectedSpecimenDefinition],
    annotationTypes:     Set[AnnotationType])
    extends EntityDto[CollectionEventTypeId] with HasName with HasOptionalDescription

final case class CollectionEventTypeInfoDto(id: CollectionEventTypeId, slug: Slug, name: String)
    extends NamedEntityInfo[CollectionEventTypeId]

object CollectionEventTypeInfoDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(eventType: CollectionEventType): CollectionEventTypeInfoDto =
    CollectionEventTypeInfoDto(eventType.id, eventType.slug, eventType.name)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val collectionEventTypeInfoDtoFormat: Format[CollectionEventTypeInfoDto] =
    Json.format[CollectionEventTypeInfoDto]

}

final case class CollectedSpecimenDefinitionNames(
    id:                      CollectionEventTypeId,
    slug:                    Slug,
    name:                    String,
    specimenDefinitionNames: Set[SpecimenDefinitionInfoDto])
    extends NamedEntityInfo[CollectionEventTypeId]

object CollectedSpecimenDefinitionNames {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(eventType: CollectionEventType): CollectedSpecimenDefinitionNames = {
    val definitionNames = eventType.specimenDefinitions.map(sd => SpecimenDefinitionInfoDto(sd))
    CollectedSpecimenDefinitionNames(eventType.id, eventType.slug, eventType.name, definitionNames)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val specimenDefinitionNamesFormat: Format[CollectedSpecimenDefinitionNames] =
    Json.format[CollectedSpecimenDefinitionNames]

}
