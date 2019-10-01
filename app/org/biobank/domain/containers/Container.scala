package org.biobank.domain.containers

import java.time.OffsetDateTime
import org.biobank.ValidationKey
import org.biobank.domain._
import org.biobank.domain.PreservationTemperature._
import org.biobank.domain.centres.CentreId
import play.api.libs.json._
import scalaz.Scalaz._
import scalaz.NonEmptyList._

/**
 * Predicates that can be used to filter collections of containers.
 *
 */
trait ContainerPredicates {

  type ContainerFilter = Container => Boolean

  val labelIsOneOf: Set[String] => ContainerFilter =
    labels =>
      container =>
        container match {
          case c: RootContainer  => labels.contains(c.label)
          case c: ChildContainer => labels.contains(c.schemaLabel.label)
        }

  val labelIsLike: Set[String] => ContainerFilter =
    labels =>
      container => {
        val lc = container match {
          case c: RootContainer  => c.label.toLowerCase
          case c: ChildContainer => c.schemaLabel.label.toLowerCase
        }
        labels.forall(n => lc.contains(n.toLowerCase))
      }
}

trait ContainerValidations {

  case object ContainerInventoryIdInvalid extends ValidationKey

  case object ContainerSchemaLabelInvalid extends ValidationKey

  case object ContainerTypeIdInvalid extends ValidationKey

  case object ContainerSchemaIdInvalid extends ValidationKey

  case object ContainerParentIdInvalid extends ValidationKey

  case object CentreIdInvalid extends ValidationKey

  case object ContainerConstraintsInvalid extends ValidationKey

}

trait HasInventoryId {

  /**
   * An inventory identifier, such as a barcode. Global uniqueness is required so that
   * [[domain.containers.Container Containers]], like [[domain.participants.Specimen Specimen]]s, can be
   * shipped between [[domain.centres.Centre Centres]].
   */
  val inventoryId: String

}

trait HasEnabled {
  val enabled: Boolean

  def withEnabled(enabled: Boolean, timeModified: OffsetDateTime): DomainValidation[Container]
}

trait HasConstraints {
  val constraints: Option[ContainerConstraints]

  def withConstraints(
      constraints:  Option[ContainerConstraints],
      timeModified: OffsetDateTime
    ): DomainValidation[Container]
}

/**
 * A specifically built physical unit that can hold child containers, or can be contained in a parent
 * container.
 */
sealed trait Container
    extends ConcurrencySafeEntity[ContainerId] with HasSlug with HasInventoryId with ContainerValidations {
  import org.biobank.CommonValidations._

  /** The ID of the container type that classifiies this [[Container]]. */
  val containerTypeId: ContainerTypeId

  val storageType: ContainerStorageType

  def getLabel(): String

  def withLabel(label: String, timeModified: OffsetDateTime): DomainValidation[Container]

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def withLabel(label: String): DomainValidation[Container] =
    withLabel(label, OffsetDateTime.now)

  def withInventoryId(
      inventoryId:  String,
      slug:         Slug,
      timeModified: OffsetDateTime
    ): DomainValidation[Container]

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def withInventoryId(inventoryId: String, slug: Slug): DomainValidation[Container] =
    withInventoryId(inventoryId, slug, OffsetDateTime.now)

  def withContainerType(
      containerTypeId: ContainerTypeId,
      timeModified:    OffsetDateTime
    ): DomainValidation[Container]

  protected def validateInventoryId[T](inventoryId: String): DomainValidation[String] =
    validateNonEmptyString(inventoryId, ContainerInventoryIdInvalid)

  override def toString: String = this.getClass.getSimpleName + ": " + Json.prettyPrint(Json.toJson(this))

}

object Container extends ContainerValidations {
  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  def validate(
      id:              ContainerId,
      version:         Long,
      inventoryId:     String,
      containerTypeId: ContainerTypeId
    ): DomainValidation[Unit] =
    (validateId(id) |@|
      validateVersion(version) |@|
      validateNonEmptyString(inventoryId, ContainerInventoryIdInvalid) |@|
      validateId(containerTypeId, ContainerTypeIdInvalid)) { case _ => () }

  def validateLabel(schemaLabel: ContainerSchemaLabel): DomainValidation[Unit] =
    ContainerSchemaLabel.validate(schemaLabel).leftMap { err =>
      nel(ContainerSchemaLabelInvalid.toString, err.list)
    }

  def constraintsValidate(constraints: Option[ContainerConstraints]): DomainValidation[Unit] =
    ContainerConstraints.validate(constraints).leftMap { err =>
      nel(ContainerConstraintsInvalid.toString, err.list)
    }

  type ContainersCompare = (Container, Container) => Boolean

  val sort2Compare: Map[String, ContainersCompare] =
    Map[String, ContainersCompare]("label" -> Container.compareByLabel)

  def compareByLabel(c1: Container, c2: Container): Boolean =
    (c1, c2) match {
      case (a: StorageContainer, b: StorageContainer) =>
        (a.schemaLabel.label compareTo b.schemaLabel.label) < 0
      case (a: SpecimenContainer, b: SpecimenContainer) =>
        (a.schemaLabel.label compareTo b.schemaLabel.label) < 0
      case (a: RootContainer, b: RootContainer) =>
        (a.label compareTo b.label) < 0
      case _ => false
    }

  val rootStorage:      ContainerStorageType = new ContainerStorageType("root-container")
  val containerStorage: ContainerStorageType = new ContainerStorageType("storage-container")
  val specimenStorage:  ContainerStorageType = new ContainerStorageType("specimen-container")

  implicit val containerFormat: Format[Container] = new Format[Container] {
    override def writes(container: Container): JsValue =
      container match {
        case c: RootContainer     => Json.toJson(c)(rootContainerFormat)
        case c: StorageContainer  => Json.toJson(c)(storageContainerFormat)
        case c: SpecimenContainer => Json.toJson(c)(specimenContainerFormat)
      }

    override def reads(json: JsValue): JsResult[Container] = (json \ "storageType") match {
      case JsDefined(JsString(rootStorage.id))      => json.validate[RootContainer]
      case JsDefined(JsString(containerStorage.id)) => json.validate[StorageContainer]
      case JsDefined(JsString(specimenStorage.id))  => json.validate[SpecimenContainer]
      case _                                        => JsError("error")
    }
  }

  implicit val rootContainerFormat:     Format[RootContainer]     = Json.format[RootContainer]
  implicit val storageContainerFormat:  Format[StorageContainer]  = Json.format[StorageContainer]
  implicit val specimenContainerFormat: Format[SpecimenContainer] = Json.format[SpecimenContainer]

}

sealed trait ChildContainer extends Container with HasInventoryId {

  val parentId: ContainerId

  val schemaLabel: ContainerSchemaLabel

  def getLabel(): String = schemaLabel.label

  def withPosition(
      parentId:     ContainerId,
      label:        String,
      timeModified: OffsetDateTime
    ): DomainValidation[Container]
}

/**
 * When enabled is true the container can have new specimens added to it.
 */
final case class RootContainer(
    id:              ContainerId,
    version:         Long,
    timeAdded:       OffsetDateTime,
    timeModified:    Option[OffsetDateTime],
    slug:            Slug,
    inventoryId:     String,
    label:           String,
    enabled:         Boolean,
    containerTypeId: ContainerTypeId,
    centreId:        CentreId,
    locationId:      LocationId,
    temperature:     PreservationTemperature,
    constraints:     Option[ContainerConstraints])
    extends { val storageType = Container.rootStorage } with Container with HasEnabled with HasConstraints
with ContainerValidations {

  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  def getLabel(): String = label

  def withLabel(label: String, timeModified: OffsetDateTime): DomainValidation[Container] =
    validateNonEmptyString(label) map { _ =>
      update.copy(label = label, version = nextVersion, timeModified = Some(timeModified))
    }

  def withInventoryId(
      inventoryId:  String,
      slug:         Slug,
      timeModified: OffsetDateTime
    ): DomainValidation[Container] =
    validateInventoryId(inventoryId) map { _ =>
      copy(inventoryId = inventoryId, slug = slug, version = nextVersion, timeModified = Some(timeModified))
    }

  def withContainerType(
      containerTypeId: ContainerTypeId,
      timeModified:    OffsetDateTime
    ): DomainValidation[RootContainer] =
    validateId(containerTypeId, ContainerTypeIdInvalid) map { _ =>
      copy(containerTypeId = containerTypeId, version = nextVersion, timeModified = Some(timeModified))
    }

  def withEnabled(enabled: Boolean, timeModified: OffsetDateTime): DomainValidation[RootContainer] =
    copy(enabled = enabled, version = nextVersion, timeModified = Some(timeModified)).success

  def withConstraints(
      constraints:  Option[ContainerConstraints],
      timeModified: OffsetDateTime
    ): DomainValidation[RootContainer] =
    ContainerConstraints.validate(constraints) map { _ =>
      copy(constraints = constraints, version = nextVersion, timeModified = Some(timeModified))
    }

  def withCentreLocation(
      centreId:     CentreId,
      locationId:   LocationId,
      timeModified: OffsetDateTime
    ): DomainValidation[RootContainer] =
    (validateId(centreId, CentreIdInvalid) |@|
      (validateId(locationId, LocationIdInvalid))) {
      case _ =>
        copy(centreId     = centreId,
             locationId   = locationId,
             version      = nextVersion,
             timeModified = Some(timeModified))
    }

  def withTemperature(
      temperature:  PreservationTemperature,
      timeModified: OffsetDateTime
    ): DomainValidation[RootContainer] =
    copy(temperature = temperature, version = nextVersion, timeModified = Some(timeModified)).success

  private def update() =
    copy(version = nextVersion, timeModified = Some(OffsetDateTime.now))

}

object RootContainer extends ContainerValidations {

  def create(
      id:              ContainerId,
      version:         Long,
      label:           String,
      inventoryId:     String,
      containerTypeId: ContainerTypeId,
      centreId:        CentreId,
      locationId:      LocationId,
      temperature:     PreservationTemperature,
      constraints:     Option[ContainerConstraints]
    ): DomainValidation[RootContainer] =
    (Container.validate(id, version, inventoryId, containerTypeId) |@|
      Container.constraintsValidate(constraints)) {
      case _ =>
        RootContainer(id              = id,
                      version         = version,
                      timeAdded       = OffsetDateTime.now,
                      timeModified    = None,
                      slug            = Slug(label),
                      label           = label,
                      inventoryId     = inventoryId,
                      enabled         = false,
                      containerTypeId = containerTypeId,
                      centreId        = centreId,
                      locationId      = locationId,
                      temperature     = temperature,
                      constraints     = constraints)
    }
}

/**
 * @param enabled When is true the container can have new specimens added to it.
 *
 * @param parentId The ID of the [[Container]] that this container is stored in.
 *
 * @param position The position this [[Container]] has in its parent.
 */
final case class StorageContainer(
    id:              ContainerId,
    version:         Long,
    timeAdded:       OffsetDateTime,
    timeModified:    Option[OffsetDateTime],
    slug:            Slug,
    inventoryId:     String,
    schemaLabel:     ContainerSchemaLabel,
    enabled:         Boolean,
    containerTypeId: ContainerTypeId,
    parentId:        ContainerId,
    constraints:     Option[ContainerConstraints])
    extends { val storageType = Container.containerStorage } with Container with HasEnabled
with HasConstraints with ChildContainer with ContainerValidations {

  import org.biobank.domain.DomainValidations._

  def withLabel(label: String, timeModified: OffsetDateTime): DomainValidation[Container] = {
    val newLabel = schemaLabel.copy(label = label)
    ContainerSchemaLabel.validate(newLabel) map { _ =>
      update.copy(schemaLabel = newLabel, version = nextVersion, timeModified = Some(timeModified))
    }
  }

  def withInventoryId(
      inventoryId:  String,
      slug:         Slug,
      timeModified: OffsetDateTime
    ): DomainValidation[Container] =
    validateInventoryId(inventoryId) map { _ =>
      copy(inventoryId = inventoryId, slug = slug, version = nextVersion, timeModified = Some(timeModified))
    }

  def withEnabled(enabled: Boolean, timeModified: OffsetDateTime): DomainValidation[StorageContainer] =
    copy(enabled = enabled, version = nextVersion, timeModified = Some(timeModified)).success

  def withContainerType(
      containerTypeId: ContainerTypeId,
      timeModified:    OffsetDateTime
    ): DomainValidation[StorageContainer] =
    validateId(containerTypeId, ContainerTypeIdInvalid) map { _ =>
      copy(containerTypeId = containerTypeId, version = nextVersion, timeModified = Some(timeModified))
    }

  def withPosition(
      parentId:     ContainerId,
      label:        String,
      timeModified: OffsetDateTime
    ): DomainValidation[StorageContainer] = {
    val newPosition = ContainerSchemaLabel(schemaLabel.schemaId, label)
    (validateId(parentId, ContainerParentIdInvalid) |@|
      ContainerSchemaLabel.validate(newPosition)) {
      case _ =>
        copy(parentId     = parentId,
             schemaLabel  = newPosition,
             version      = nextVersion,
             timeModified = Some(timeModified))
    }
  }

  def withConstraints(
      constraints:  Option[ContainerConstraints],
      timeModified: OffsetDateTime
    ): DomainValidation[StorageContainer] =
    ContainerConstraints.validate(constraints) map { _ =>
      copy(constraints = constraints, version = nextVersion, timeModified = Some(timeModified))
    }

  private def update() =
    copy(version = nextVersion, timeModified = Some(OffsetDateTime.now))

}

object StorageContainer extends ContainerValidations {
  import org.biobank.domain.DomainValidations._

  def create(
      id:              ContainerId,
      version:         Long,
      inventoryId:     String,
      containerTypeId: ContainerTypeId,
      parentId:        ContainerId,
      schemaLabel:     ContainerSchemaLabel,
      constraints:     Option[ContainerConstraints]
    ): DomainValidation[StorageContainer] =
    (Container.validate(id, version, inventoryId, containerTypeId) |@|
      validateId(parentId, ContainerParentIdInvalid) |@|
      Container.validateLabel(schemaLabel) |@|
      Container.constraintsValidate(constraints)) {
      case _ =>
        StorageContainer(id              = id,
                         version         = version,
                         timeAdded       = OffsetDateTime.now,
                         timeModified    = None,
                         slug            = Slug(inventoryId),
                         inventoryId     = inventoryId,
                         enabled         = false,
                         containerTypeId = containerTypeId,
                         parentId        = parentId,
                         schemaLabel     = schemaLabel,
                         constraints     = constraints)
    }
}

/**
 * @param parentId The ID of the [[Container]] that this container is stored in.
 *
 * @param position The position this [[Container]] has in its parent.
 */
final case class SpecimenContainer(
    id:              ContainerId,
    version:         Long,
    timeAdded:       OffsetDateTime,
    timeModified:    Option[OffsetDateTime],
    slug:            Slug,
    inventoryId:     String,
    schemaLabel:     ContainerSchemaLabel,
    containerTypeId: ContainerTypeId,
    parentId:        ContainerId)
    extends { val storageType = Container.specimenStorage } with Container with ChildContainer
with ContainerValidations {
  import org.biobank.domain.DomainValidations._

  def withLabel(label: String, timeModified: OffsetDateTime): DomainValidation[Container] = {
    val newLabel = schemaLabel.copy(label = label)
    ContainerSchemaLabel.validate(newLabel) map { _ =>
      update.copy(schemaLabel = newLabel, version = nextVersion, timeModified = Some(timeModified))
    }
  }

  def withInventoryId(
      inventoryId:  String,
      slug:         Slug,
      timeModified: OffsetDateTime
    ): DomainValidation[Container] =
    validateInventoryId(inventoryId) map { _ =>
      copy(inventoryId = inventoryId, slug = slug, version = nextVersion, timeModified = Some(timeModified))
    }

  def withSchemaLabel(schemaLabel: ContainerSchemaLabel): DomainValidation[SpecimenContainer] =
    ContainerSchemaLabel.validate(schemaLabel) map { _ =>
      update.copy(schemaLabel = schemaLabel)
    }

  def withContainerType(
      containerTypeId: ContainerTypeId,
      timeModified:    OffsetDateTime
    ): DomainValidation[SpecimenContainer] =
    validateId(containerTypeId, ContainerTypeIdInvalid) map { _ =>
      copy(containerTypeId = containerTypeId, version = nextVersion, timeModified = Some(timeModified))
    }

  def withPosition(
      parentId:     ContainerId,
      label:        String,
      timeModified: OffsetDateTime
    ): DomainValidation[SpecimenContainer] = {
    val newPosition = ContainerSchemaLabel(schemaLabel.schemaId, label)
    (validateId(parentId, ContainerParentIdInvalid) |@|
      ContainerSchemaLabel.validate(newPosition)) {
      case _ =>
        copy(parentId     = parentId,
             schemaLabel  = newPosition,
             version      = nextVersion,
             timeModified = Some(timeModified))
    }
  }

  def withConstraints(
      constraints:  Option[ContainerConstraints],
      timeModified: OffsetDateTime
    ): DomainValidation[SpecimenContainer] =
    DomainError("cannot add constraints to a specimen container").failureNel[SpecimenContainer]

  private def update(): SpecimenContainer =
    copy(version = nextVersion, timeModified = Some(OffsetDateTime.now))

}

object SpecimenContainer extends ContainerValidations {
  import org.biobank.domain.DomainValidations._

  def create(
      id:              ContainerId,
      version:         Long,
      inventoryId:     String,
      containerTypeId: ContainerTypeId,
      parentId:        ContainerId,
      schemaLabel:     ContainerSchemaLabel
    ): DomainValidation[SpecimenContainer] =
    (Container.validate(id, version, inventoryId, containerTypeId) |@|
      validateId(parentId, ContainerParentIdInvalid) |@|
      ContainerSchemaLabel.validate(schemaLabel)) {
      case _ =>
        SpecimenContainer(id              = id,
                          version         = version,
                          timeAdded       = OffsetDateTime.now,
                          timeModified    = None,
                          slug            = Slug(inventoryId),
                          inventoryId     = inventoryId,
                          containerTypeId = containerTypeId,
                          parentId        = parentId,
                          schemaLabel     = schemaLabel)
    }

}
