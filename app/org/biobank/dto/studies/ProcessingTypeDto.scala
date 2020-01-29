package org.biobank.dto.studies

import org.biobank.domain.Slug
import org.biobank.domain.studies.{ProcessingType, ProcessingTypeId}
import org.biobank.dto.NamedEntityInfo
import play.api.libs.json._

final case class ProcessedSpecimenDefinitionName(
    id:                     ProcessingTypeId,
    slug:                   Slug,
    name:                   String,
    specimenDefinitionName: SpecimenDefinitionInfoDto)
    extends NamedEntityInfo[ProcessingTypeId]

object ProcessedSpecimenDefinitionName {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(processingType: ProcessingType): ProcessedSpecimenDefinitionName =
    ProcessedSpecimenDefinitionName(processingType.id,
                                    processingType.slug,
                                    processingType.name,
                                    SpecimenDefinitionInfoDto(processingType.output.specimenDefinition))

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val specimenDefinitionNamesFormat: Format[ProcessedSpecimenDefinitionName] =
    Json.format[ProcessedSpecimenDefinitionName]

}
