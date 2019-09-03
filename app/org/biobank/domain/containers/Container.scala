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
          case c: ChildContainer => labels.contains(c.position.label)
        }

  val labelIsLike: Set[String] => ContainerFilter =
    labels =>
      container => {
        val lc = container match {
          case c: RootContainer  => c.label.toLowerCase
          case c: ChildContainer => c.position.label.toLowerCase
        }
        labels.forall(n => lc.contains(n.toLowerCase))
      }
}

trait ContainerValidations {

  case object ContainerInventoryIdInvalid extends ValidationKey

  case object ContainerLabelInvalid extends ValidationKey

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

  def getLabel(): String

  protected def validateInventoryId[T](inventoryId: String): DomainValidation[String] =
    validateNonEmptyString(inventoryId, ContainerInventoryIdInvalid)

  def withInventoryId(inventoryId: String): DomainValidation[Container]

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
  import ContainerSchemaPositionValidations._

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

  def validatePosition(position: ContainerSchemaPosition): DomainValidation[Unit] =
    ContainerSchemaPosition.validate(position).leftMap { err =>
      nel(ContainerSchemaPositionInvalid.toString, err.list)
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
        (a.position.label compareTo b.position.label) < 0
      case (a: SpecimenContainer, b: SpecimenContainer) =>
        (a.position.label compareTo b.position.label) < 0
      case (a: RootContainer, b: RootContainer) =>
        (a.label compareTo b.label) < 0
      case _ => false
    }

  class StorageType(val id: String) extends AnyVal {
    override def toString: String = id
  }

  val rootStorage:      StorageType = new StorageType("root-container")
  val containerStorage: StorageType = new StorageType("storage-container")
  val specimenStorage:  StorageType = new StorageType("specimen-container")

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

  val position: ContainerSchemaPosition

  def getLabel(): String = position.label
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
    extends Container with ContainerValidations {

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
    position:        ContainerSchemaPosition,
    constraints:     Option[ContainerConstraints])
    extends Container with ChildContainer with ContainerValidations {

  import org.biobank.domain.DomainValidations._

  def withInventoryId(inventoryId: String): DomainValidation[Container] =
    validateInventoryId(inventoryId) map { _ =>
      update.copy(inventoryId = inventoryId)
    }

  def withEnabled(enabled: Boolean): DomainValidation[StorageContainer] =
    update.copy(enabled = enabled).success

  def withPosition(position: ContainerSchemaPosition): DomainValidation[Container] =
    ContainerSchemaPosition.validate(position) map { _ =>
      update.copy(position = position)
    }

  def withParentPosition(
      parentId: ContainerId,
      position: ContainerSchemaPosition
    ): DomainValidation[Container] =
    (validateId(parentId, ContainerParentIdInvalid) |@|
      ContainerSchemaPosition.validate(position)) {
      case _ =>
        update.copy(parentId = parentId, position = position)
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
          |  position:        $position,
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
      position:        ContainerSchemaPosition,
      constraints:     Option[ContainerConstraints]
    ): DomainValidation[StorageContainer] =
    (Container.validate(id, version, inventoryId, containerTypeId) |@|
      validateId(parentId, ContainerParentIdInvalid) |@|
      Container.validatePosition(position) |@|
      Container.constraintsValidate(constraints)) {
      case _ =>
        StorageContainer(id              = id,
                         version         = version,
                         timeAdded       = OffsetDateTime.now,
                         timeModified    = None,
                         slug            = Slug(position.label),
                         inventoryId     = inventoryId,
                         enabled         = false,
                         containerTypeId = containerTypeId,
                         parentId        = parentId,
                         position        = position,
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
    position:        ContainerSchemaPosition)
    extends Container with ChildContainer with ContainerValidations {
  import org.biobank.domain.DomainValidations._

  def withInventoryId(inventoryId: String): DomainValidation[Container] =
    validateInventoryId(inventoryId) map { _ =>
      update.copy(inventoryId = inventoryId)
    }

  def withPosition(position: ContainerSchemaPosition): DomainValidation[Container] =
    ContainerSchemaPosition.validate(position) map { _ =>
      update.copy(position = position)
    }

  def withParentPosition(
      parentId: ContainerId,
      position: ContainerSchemaPosition
    ): DomainValidation[Container] =
    (validateId(parentId, ContainerParentIdInvalid) |@|
      ContainerSchemaPosition.validate(position)) {
      case _ =>
        update.copy(parentId = parentId, position = position)
    }

  private def update(): SpecimenContainer =
    copy(version = nextVersion, timeModified = Some(OffsetDateTime.now))

  override def toString: String =
    super.toString +
      s"""|,
          |  parentId:        $parentId,
          |  position:        $position,
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
      position:        ContainerSchemaPosition
    ): DomainValidation[SpecimenContainer] =
    (Container.validate(id, version, inventoryId, containerTypeId) |@|
      validateId(parentId, ContainerParentIdInvalid) |@|
      Container.validatePosition(position)) {
      case _ =>
        SpecimenContainer(id              = id,
                          version         = version,
                          timeAdded       = OffsetDateTime.now,
                          timeModified    = None,
                          slug            = Slug(position.label),
                          inventoryId     = inventoryId,
                          containerTypeId = containerTypeId,
                          parentId        = parentId,
                          position        = position)
    }

}
