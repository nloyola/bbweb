package org.biobank.dto.studies

import org.biobank.domain.Slug
import org.biobank.domain.studies.{SpecimenDefinition, SpecimenDefinitionId}
import org.biobank.dto.NamedEntityInfo
import play.api.libs.json._

final case class SpecimenDefinitionInfoDto(id: SpecimenDefinitionId, slug: Slug, name: String)
    extends NamedEntityInfo[SpecimenDefinitionId]

object SpecimenDefinitionInfoDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(specimenDefinition: SpecimenDefinition): SpecimenDefinitionInfoDto =
    SpecimenDefinitionInfoDto(specimenDefinition.id, specimenDefinition.slug, specimenDefinition.name)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val specimenDefinitionInfoDtoFormat: Format[SpecimenDefinitionInfoDto] =
    Json.format[SpecimenDefinitionInfoDto]

}
