package org.biobank.dto.containers

import java.time.OffsetDateTime
import org.biobank.domain.{HasName, HasOptionalDescription, Slug}
import org.biobank.domain.centres.Centre
import org.biobank.domain.containers.{ContainerSchema, ContainerType, ContainerTypeId}
import org.biobank.dto.{EntityDto, NamedEntityInfo}
import org.biobank.dto.centres.CentreInfoAndStateDto
import org.biobank.dto.containers.ContainerSchemaInfoDto
import play.api.libs.json._

final case class ContainerTypeDto(
    id:           ContainerTypeId,
    version:      Long,
    timeAdded:    OffsetDateTime,
    timeModified: Option[OffsetDateTime],
    slug:         Slug,
    name:         String,
    description:  Option[String],
    centre:       CentreInfoAndStateDto,
    schema:       ContainerSchemaInfoDto,
    shared:       Boolean,
    enabled:      Boolean,
    storageType:  String)
    extends EntityDto[ContainerTypeId] with HasName with HasOptionalDescription {

  override def toString: String = s"|${this.getClass.getSimpleName}: ${Json.prettyPrint(Json.toJson(this))}"

}

object ContainerTypeDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(containerType: ContainerType, centre: Centre, schema: ContainerSchema): ContainerTypeDto =
    ContainerTypeDto(id           = containerType.id,
                     version      = containerType.version,
                     timeAdded    = containerType.timeAdded,
                     timeModified = containerType.timeModified,
                     slug         = containerType.slug,
                     name         = containerType.name,
                     description  = containerType.description,
                     centre       = CentreInfoAndStateDto(centre),
                     schema       = ContainerSchemaInfoDto(schema),
                     shared       = containerType.shared,
                     enabled      = containerType.enabled,
                     storageType  = containerType.storageType.id)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val containerTypeDtoFormat: Format[ContainerTypeDto] = Json.format[ContainerTypeDto]

}

final case class ContainerTypeInfoDto(id: ContainerTypeId, slug: Slug, name: String)
    extends NamedEntityInfo[ContainerTypeId]

object ContainerTypeInfoDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(containerType: ContainerType): ContainerTypeInfoDto =
    ContainerTypeInfoDto(containerType.id, containerType.slug, containerType.name)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val containerTypeInfoDtoFormat: Format[ContainerTypeInfoDto] = Json.format[ContainerTypeInfoDto]

}
