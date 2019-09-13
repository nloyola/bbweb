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

  protected def validateInventoryId[T](inventoryId: String): DomainValidation[String] =
    validateNonEmptyString(inventoryId, ContainerInventoryIdInvalid)

  def withInventoryId(inventoryId: String): DomainValidation[Container]

  def withContainerType(containerTypeId: ContainerTypeId): DomainValidation[Container]

  override def toString: String =
    s"""|${this.getClass.getSimpleName}: {
        |  id:              $id,
        |  version:         $version,
        |  timeAdded:       $timeAdded,
        |  timeModified:    $timeModified,
        |  slug:            $slug,
        |  inventoryId:     $inventoryId,
        |  containerTypeId: $containerTypeId""".stripMargin
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

sealed trait ChildContainer extends HasInventoryId {

  val parentId: ContainerId

  val schemaLabel: ContainerSchemaLabel

  def getLabel(): String = schemaLabel.label
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
    extends { val storageType = Container.rootStorage } with Container with ContainerValidations {

  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  def getLabel(): String = label

  def withInventoryId(inventoryId: String): DomainValidation[Container] =
    validateInventoryId(inventoryId) map { _ =>
      update.copy(inventoryId = inventoryId)
    }

  def withLabel(inventoryId: String): DomainValidation[Container] =
    validateNonEmptyString(label) map { _ =>
      update.copy(label = label)
    }

  def withContainerType(containerTypeId: ContainerTypeId): DomainValidation[RootContainer] =
    validateId(containerTypeId, ContainerTypeIdInvalid) map { _ =>
      update.copy(containerTypeId = containerTypeId)
    }

  def withEnabled(enabled: Boolean): DomainValidation[RootContainer] =
    update.copy(enabled = enabled).success

  def withConstraints(constraints: Option[ContainerConstraints]): DomainValidation[RootContainer] =
    ContainerConstraints.validate(constraints) map { _ =>
      update.copy(constraints = constraints)
    }

  def withCentreLocation(centreId: CentreId, locationId: LocationId): DomainValidation[RootContainer] =
    (validateId(centreId, CentreIdInvalid) |@|
      (validateId(locationId, LocationIdInvalid))) {
      case _ =>
        update.copy(centreId = centreId, locationId = locationId)
    }

  def withTemperature(temperature: PreservationTemperature): DomainValidation[RootContainer] =
    update.copy(temperature = temperature).successNel[String]

  private def update() =
    copy(version = nextVersion, timeModified = Some(OffsetDateTime.now))

  override def toString: String =
    super.toString +
      s"""|,
          |  label:           $label,
          |  enabled          $enabled,
          |  centreId:        $centreId,
          |  locationId:      $locationId,
          |  temperature:     $temperature,
          |  constraints      $constraints
          |}""".stripMargin

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
    enabled:         Boolean,
    containerTypeId: ContainerTypeId,
    parentId:        ContainerId,
    schemaLabel:     ContainerSchemaLabel,
    constraints:     Option[ContainerConstraints])
    extends { val storageType = Container.containerStorage } with Container with ChildContainer
with ContainerValidations {

  import org.biobank.domain.DomainValidations._

  def withInventoryId(inventoryId: String): DomainValidation[Container] =
    validateInventoryId(inventoryId) map { _ =>
      update.copy(inventoryId = inventoryId)
    }

  def withEnabled(enabled: Boolean): DomainValidation[StorageContainer] =
    update.copy(enabled = enabled).success

  def withLabel(schemaLabel: ContainerSchemaLabel): DomainValidation[Container] =
    ContainerSchemaLabel.validate(schemaLabel) map { _ =>
      update.copy(schemaLabel = schemaLabel)
    }

  def withContainerType(containerTypeId: ContainerTypeId): DomainValidation[Container] =
    validateId(containerTypeId, ContainerTypeIdInvalid) map { _ =>
      update.copy(containerTypeId = containerTypeId)
    }

  def withParentLabel(parentId: ContainerId, schemaLabel: ContainerSchemaLabel): DomainValidation[Container] =
    (validateId(parentId, ContainerParentIdInvalid) |@|
      ContainerSchemaLabel.validate(schemaLabel)) {
      case _ =>
        update.copy(parentId = parentId, schemaLabel = schemaLabel)
    }

  def withConstraints(constraints: Option[ContainerConstraints]): DomainValidation[StorageContainer] =
    ContainerConstraints.validate(constraints) map { _ =>
      update.copy(constraints = constraints)
    }

  private def update() =
    copy(version = nextVersion, timeModified = Some(OffsetDateTime.now))

  override def toString: String =
    super.toString +
      s"""|,
          |  parentId:        $parentId,
          |  schemaLabel:     $schemaLabel,
          |  enabled          $enabled,
          |  constraints      $constraints
          |}""".stripMargin

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
                         slug            = Slug(schemaLabel.label),
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
    containerTypeId: ContainerTypeId,
    parentId:        ContainerId,
    schemaLabel:     ContainerSchemaLabel)
    extends { val storageType = Container.specimenStorage } with Container with ChildContainer
with ContainerValidations {
  import org.biobank.domain.DomainValidations._

  def withInventoryId(inventoryId: String): DomainValidation[Container] =
    validateInventoryId(inventoryId) map { _ =>
      update.copy(inventoryId = inventoryId)
    }

  def withSchemaLabel(schemaLabel: ContainerSchemaLabel): DomainValidation[Container] =
    ContainerSchemaLabel.validate(schemaLabel) map { _ =>
      update.copy(schemaLabel = schemaLabel)
    }

  def withContainerType(containerTypeId: ContainerTypeId): DomainValidation[Container] =
    validateId(containerTypeId, ContainerTypeIdInvalid) map { _ =>
      update.copy(containerTypeId = containerTypeId)
    }

  def withParentLabel(parentId: ContainerId, schemaLabel: ContainerSchemaLabel): DomainValidation[Container] =
    (validateId(parentId, ContainerParentIdInvalid) |@|
      ContainerSchemaLabel.validate(schemaLabel)) {
      case _ =>
        update.copy(parentId = parentId, schemaLabel = schemaLabel)
    }

  private def update(): SpecimenContainer =
    copy(version = nextVersion, timeModified = Some(OffsetDateTime.now))

  override def toString: String =
    super.toString +
      s"""|,
          |  parentId:        $parentId,
          |  schemaLabel:     $schemaLabel,
          |}""".stripMargin
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
                          slug            = Slug(schemaLabel.label),
                          inventoryId     = inventoryId,
                          containerTypeId = containerTypeId,
                          parentId        = parentId,
                          schemaLabel     = schemaLabel)
    }

}
