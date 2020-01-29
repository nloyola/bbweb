package org.biobank.dto.containers

import java.time.OffsetDateTime
import org.biobank.domain.{HasName, HasOptionalDescription, Slug}
import org.biobank.domain.centres.Centre
import org.biobank.domain.containers.{ContainerSchema, ContainerSchemaId}
import org.biobank.dto.{EntityDto, NamedEntityInfo}
import org.biobank.dto.centres.CentreInfoDto
import play.api.libs.json._

final case class ContainerSchemaDto(
    id:           ContainerSchemaId,
    version:      Long,
    timeAdded:    OffsetDateTime,
    timeModified: Option[OffsetDateTime],
    slug:         Slug,
    name:         String,
    description:  Option[String],
    shared:       Boolean,
    centre:       CentreInfoDto,
    labels:       Set[String])
    extends EntityDto[ContainerSchemaId] with HasName with HasOptionalDescription {

  override def toString: String = s"|${this.getClass.getSimpleName}: ${Json.prettyPrint(Json.toJson(this))}"
}

object ContainerSchemaDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(schema: ContainerSchema, centre: Centre): ContainerSchemaDto =
    ContainerSchemaDto(id           = schema.id,
                       version      = schema.version,
                       timeAdded    = schema.timeAdded,
                       timeModified = schema.timeModified,
                       slug         = schema.slug,
                       name         = schema.name,
                       description  = schema.description,
                       shared       = schema.shared,
                       centre       = CentreInfoDto(centre),
                       labels       = schema.labels)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val containerSchemaDtoFormat: Format[ContainerSchemaDto] = Json.format[ContainerSchemaDto]

}

final case class ContainerSchemaInfoDto(id: ContainerSchemaId, slug: Slug, name: String)
    extends NamedEntityInfo[ContainerSchemaId]

object ContainerSchemaInfoDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(containerSchema: ContainerSchema): ContainerSchemaInfoDto =
    ContainerSchemaInfoDto(containerSchema.id, containerSchema.slug, containerSchema.name)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val containerSchemaInfoDtoFormat: Format[ContainerSchemaInfoDto] =
    Json.format[ContainerSchemaInfoDto]

}
