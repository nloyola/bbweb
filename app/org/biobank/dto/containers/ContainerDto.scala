package org.biobank.dto.containers

import java.time.OffsetDateTime
import org.biobank.domain.{HasSlug, Location, Slug}
import org.biobank.domain.PreservationTemperature._
import org.biobank.domain.centres.Centre
import org.biobank.domain.containers.{
  Container,
  ContainerId,
  ContainerType,
  RootContainer,
  SpecimenContainer,
  StorageContainer
}
import org.biobank.dto.EntityDto
import org.biobank.dto.containers.ContainerTypeInfoDto
import org.biobank.dto.centres.CentreLocationInfo
import play.api.libs.json._

trait ContainerDto extends EntityDto[ContainerId] with HasSlug {
  val id:            ContainerId
  val version:       Long
  val timeAdded:     OffsetDateTime
  val timeModified:  Option[OffsetDateTime]
  val slug:          Slug
  val inventoryId:   String
  val containerType: ContainerTypeInfoDto
  val storageType:   String

  override def toString: String = Json.prettyPrint(Json.toJson(this))

}

object ContainerDto {

  implicit val containerDtoFormat: Format[ContainerDto] = new Format[ContainerDto] {

    override def writes(container: ContainerDto): JsValue =
      container match {
        case c: RootContainerDto     => Json.toJson(c)(RootContainerDto.rootContainerDtoFormat)
        case c: StorageContainerDto  => Json.toJson(c)(StorageContainerDto.storageContainerDtoFormat)
        case c: SpecimenContainerDto => Json.toJson(c)(SpecimenContainerDto.specimenContainerDtoFormat)
      }

    override def reads(json: JsValue): JsResult[ContainerDto] = (json \ "storageType") match {
      case JsDefined(JsString(Container.rootStorage.id))      => json.validate[RootContainerDto]
      case JsDefined(JsString(Container.containerStorage.id)) => json.validate[StorageContainerDto]
      case JsDefined(JsString(Container.specimenStorage.id))  => json.validate[SpecimenContainerDto]
      case _                                                  => JsError("error")
    }
  }

}

final case class RootContainerDto(
    id:                 ContainerId,
    version:            Long,
    timeAdded:          OffsetDateTime,
    timeModified:       Option[OffsetDateTime],
    slug:               Slug,
    label:              String,
    inventoryId:        String,
    enabled:            Boolean,
    containerType:      ContainerTypeInfoDto,
    centreLocationInfo: CentreLocationInfo,
    temperature:        PreservationTemperature,
    constraints:        Option[ContainerConstraintsDto],
    storageType:        String)
    extends ContainerDto {}

object RootContainerDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(
      c:             RootContainer,
      containerType: ContainerType,
      centre:        Centre,
      location:      Location
    ): RootContainerDto =
    RootContainerDto(id                 = c.id,
                     version            = c.version,
                     timeAdded          = c.timeAdded,
                     timeModified       = c.timeModified,
                     slug               = c.slug,
                     label              = c.label,
                     inventoryId        = c.inventoryId,
                     enabled            = c.enabled,
                     containerType      = ContainerTypeInfoDto(containerType),
                     centreLocationInfo = CentreLocationInfo(centre, location),
                     temperature        = c.temperature,
                     constraints        = c.constraints.map(ContainerConstraintsDto(_)),
                     storageType        = c.storageType.id)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val rootContainerDtoFormat: Format[RootContainerDto] = Json.format[RootContainerDto]

}

final case class ContainerInfoDto(id: String, slug: String, label: String)

object ContainerInfoDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(container: Container): ContainerInfoDto =
    container match {
      case c: RootContainer     => apply(c)
      case c: StorageContainer  => apply(c)
      case c: SpecimenContainer => apply(c)
    }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(container: RootContainer): ContainerInfoDto =
    ContainerInfoDto(id = container.id.id, slug = container.slug.id, label = container.label)

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(container: StorageContainer): ContainerInfoDto =
    ContainerInfoDto(id = container.id.id, slug = container.slug.id, label = container.schemaLabel.label)

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(container: SpecimenContainer): ContainerInfoDto =
    ContainerInfoDto(id = container.id.id, slug = container.slug.id, label = container.schemaLabel.label)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val containerInfoDtoFormat: Format[ContainerInfoDto] = Json.format[ContainerInfoDto]

}

final case class StorageContainerDto(
    id:            ContainerId,
    version:       Long,
    timeAdded:     OffsetDateTime,
    timeModified:  Option[OffsetDateTime],
    slug:          Slug,
    inventoryId:   String,
    enabled:       Boolean,
    containerType: ContainerTypeInfoDto,
    parent:        ContainerInfoDto,
    label:         String,
    constraints:   Option[ContainerConstraintsDto],
    storageType:   String)
    extends ContainerDto {}

object StorageContainerDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(c: StorageContainer, containerType: ContainerType, parent: Container): StorageContainerDto = {
    val constraintsDto = c.constraints match {
      case Some(constraints) => Some(ContainerConstraintsDto(constraints))
      case _                 => None
    }

    StorageContainerDto(id            = c.id,
                        version       = c.version,
                        timeAdded     = c.timeAdded,
                        timeModified  = c.timeModified,
                        slug          = c.slug,
                        inventoryId   = c.inventoryId,
                        enabled       = c.enabled,
                        containerType = ContainerTypeInfoDto(containerType),
                        parent        = ContainerInfoDto(parent),
                        label         = c.schemaLabel.label,
                        constraints   = constraintsDto,
                        storageType   = c.storageType.id)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val storageContainerDtoFormat: Format[StorageContainerDto] = Json.format[StorageContainerDto]

}

final case class SpecimenContainerDto(
    id:            ContainerId,
    version:       Long,
    timeAdded:     OffsetDateTime,
    timeModified:  Option[OffsetDateTime],
    slug:          Slug,
    inventoryId:   String,
    containerType: ContainerTypeInfoDto,
    parent:        ContainerInfoDto,
    label:         String,
    storageType:   String)
    extends ContainerDto {}

object SpecimenContainerDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(c: SpecimenContainer, containerType: ContainerType, parent: Container): SpecimenContainerDto =
    SpecimenContainerDto(id            = c.id,
                         version       = c.version,
                         timeAdded     = c.timeAdded,
                         timeModified  = c.timeModified,
                         slug          = c.slug,
                         inventoryId   = c.inventoryId,
                         containerType = ContainerTypeInfoDto(containerType),
                         parent        = ContainerInfoDto(parent),
                         label         = c.schemaLabel.label,
                         storageType   = c.storageType.id)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val specimenContainerDtoFormat: Format[SpecimenContainerDto] = Json.format[SpecimenContainerDto]

}

final case class ContainerInfo(id: ContainerId, slug: Slug, inventoryId: String, label: String)

object ContainerInfo {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(container: Container): ContainerInfo = {
    ContainerInfo(container.id, container.slug, container.inventoryId, container.getLabel)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val containerInfoFormat: Format[ContainerInfo] = Json.format[ContainerInfo]

}

final case class ContainerChildrenInfo(container: ContainerInfo, children: Set[ContainerInfo])

object ContainerChildrenInfo {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(container: Container, children: Set[ContainerInfo]): ContainerChildrenInfo = {
    ContainerChildrenInfo(ContainerInfo(container), children)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val containerChildrenInfoFormat: Format[ContainerChildrenInfo] =
    Json.format[ContainerChildrenInfo]

}
