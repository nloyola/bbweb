package org.biobank.domain.containers

import java.time.OffsetDateTime
import org.biobank.ValidationKey
import org.biobank.domain._
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
    labels => container => labels.contains(container.label)

  val labelIsLike: Set[String] => ContainerFilter =
    labels => container => {
      val lc = container.label.toLowerCase
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

/**
 * A specifically built physical unit that can hold child containers, or can be contained in a parent
 * container.
 */
sealed trait Container extends ConcurrencySafeEntity[ContainerId]
    with HasSlug
    with HasContainerSchemaPosition
    with ContainerValidations {
  import org.biobank.CommonValidations._

 /**
  * An inventory identifier, such as a barcode. Global uniqueness is required so that
  * [[domain.containers.Container Containers]], like [[domain.participants.Specimen Specimen]]s, can be
  * shipped between [[domain.centres.Centre Centres]].
  */
  val inventoryId: String

  /**
   * Only used if this [[Container]] does not have a parent.
   */
  val label: String

  /** The ID of the container type that classifiies this [[Container]]. */
  val containerTypeId: ContainerTypeId

 /** The ID of the [[Container]] that this container is stored in. */
  val parentId: Option[ContainerId]

  /**
   * The position this [[Container]] has in its parent, or none if there is no specific
   * position. This value is always none if there is no parent.
   */
  val position: Option[ContainerSchemaPosition]

  /**
   */
  val sharedProperties: Option[ContainerSharedProperties]

  protected def validateInventoryId[T](inventoryId: String): DomainValidation[String] = {
    validateNonEmptyString(inventoryId, ContainerInventoryIdInvalid)
  }

  def withInventoryId(inventoryId: String): DomainValidation[Container]

  def withPosition(position: Option[ContainerSchemaPosition]): DomainValidation[Container]

  def withParentPosition(
    parentId: Option[ContainerId],
    position: Option[ContainerSchemaPosition]
  ): DomainValidation[Container]

  override def toString: String =
    s"""|${this.getClass.getSimpleName}: {
        |  id:              $id,
        |  version:         $version,
        |  timeAdded:       $timeAdded,
        |  timeModified:    $timeModified,
        |  slug:            $slug,
        |  inventoryId:     $inventoryId,
        |  containerTypeId: $containerTypeId,
        |  parentId:        $parentId,
        |  position:        $position""".stripMargin
}

object Container extends ContainerValidations {
  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._
  import ContainerSchemaPositionValidations._

  def validate(id:               ContainerId,
               version:          Long,
               inventoryId:      String,
               label:            String,
               containerTypeId:  ContainerTypeId,
               sharedProperties: Option[ContainerSharedProperties],
               parentId:         Option[ContainerId],
               position:         Option[ContainerSchemaPosition])
      : DomainValidation[Unit] = {
    (validateId(id) |@|
       validateVersion(version) |@|
       validateNonEmptyString(inventoryId, ContainerInventoryIdInvalid) |@|
       validateNonEmptyString(label, ContainerLabelInvalid) |@|
       validateId(containerTypeId, ContainerTypeIdInvalid) |@|
       validateIdOption(parentId, ContainerParentIdInvalid) |@|
       ContainerSharedProperties.validate(sharedProperties)  |@|
       ContainerSchemaPosition.validate(position)) { case _ => () }
  }

  def validatePosition(position: Option[ContainerSchemaPosition])
      : DomainValidation[Unit] = {
    ContainerSchemaPositionValidations.validate(position).leftMap { err =>
      nel(ContainerSchemaPositionInvalid.toString, err.list)
    }
  }

  def constraintsValidate(constraints: Option[ContainerConstraints])
      : DomainValidation[Unit] = {
    ContainerConstraints.validate(constraints).leftMap { err =>
      nel(ContainerConstraintsInvalid.toString, err.list)
    }
  }

  type ContainersCompare = (Container, Container) => Boolean

  val sort2Compare: Map[String, ContainersCompare] = Map[String, ContainersCompare](
      "label"  -> Container.compareByLabel
    )

  def compareByLabel(a: Container, b: Container): Boolean =
    (a.label compareToIgnoreCase b.label) < 0

  class StorageType(val id: String) extends AnyVal {
    override def toString: String = id
  }

  val containerStorage: StorageType = new StorageType("container")
  val specimenStorage: StorageType = new StorageType("specimen")


  implicit val containerWrites: Writes[Container] = new Writes[Container] {
      def writes(container: Container): JsValue = Json.obj(
          "id"               -> container.id,
          "version"          -> container.version,
          "timeAdded"        -> container.timeAdded,
          "timeModified"     -> container.timeModified,
          "slug"             -> container.slug,
          "inventoryId"      -> container.inventoryId,
          "containerTypeId"  -> container.containerTypeId,
          "parentId"         -> container.parentId,
          "position"         -> container.position,
          "sharedProperties" -> container.sharedProperties
        )
    }

  implicit val containerFormat: Format[Container] = new Format[Container] {
      override def writes(container: Container): JsValue = {
        container match {
          case c: StorageContainer => Json.toJson(c)
          case c: SpecimenContainer => Json.toJson(c)
        }
      }

      override def reads(json: JsValue): JsResult[Container] = (json \ "storageType") match {
          case JsDefined(JsString(containerStorage.id)) => json.validate[StorageContainer]
          case JsDefined(JsString(specimenStorage.id))  => json.validate[SpecimenContainer]
          case _ => JsError("error")
        }
    }

  implicit val storageContainerReads: Reads[StorageContainer] = Json.reads[StorageContainer]
  implicit val specimenContainerReads: Reads[SpecimenContainer]   = Json.reads[SpecimenContainer]

}

// The shared properties is only present in the root container
final case class StorageContainer(id:               ContainerId,
                                  version:          Long,
                                  timeAdded:        OffsetDateTime,
                                  timeModified:     Option[OffsetDateTime],
                                  slug:             Slug,
                                  inventoryId:      String,
                                  label:            String,
                                  enabled:          Boolean,
                                  containerTypeId:  ContainerTypeId,
                                  sharedProperties: Option[ContainerSharedProperties],
                                  parentId:         Option[ContainerId],
                                  position:         Option[ContainerSchemaPosition],
                                  constraints:      Option[ContainerConstraints])
    extends Container
    with ContainerValidations {

  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  def withInventoryId(inventoryId: String): DomainValidation[Container] = {
    validateInventoryId(inventoryId) map { _ =>
      update.copy(inventoryId = inventoryId)
    }
  }

  def withLabel(inventoryId: String): DomainValidation[Container] = {
    validateNonEmptyString(label) map { _ =>
      update.copy(label = label)
    }
  }

  def withEnabled(enabled: Boolean): DomainValidation[StorageContainer] = {
    update.copy(enabled = enabled).success
  }

  def withPosition(position: Option[ContainerSchemaPosition]): DomainValidation[Container] = {
    ContainerSchemaPosition.validate(position) map { _ =>
      update.copy(position = position)
    }
  }

  def withParentPosition(parentId: Option[ContainerId], position: Option[ContainerSchemaPosition])
      : DomainValidation[Container] = {
    (ContainerSchemaPosition.validate(position) |@|
       validateIdOption(parentId, ContainerParentIdInvalid)) { case _ =>
        update.copy(parentId = parentId, position = position)
    }
  }

  def withConstraints(constraints: Option[ContainerConstraints]): DomainValidation[StorageContainer] = {
    ContainerConstraints.validate(constraints) map { _ =>
      update.copy(constraints = constraints)
    }
  }

  private def update() = {
    copy(version = version + 1L, timeModified = Some(OffsetDateTime.now))
  }

  override def toString: String =  {
    super.toString +
    s"""|,
        |  enabled          $enabled
        |  constraints      $constraints
        |}""".stripMargin
  }

}

object StorageContainer extends ContainerValidations {

  def create(id:               ContainerId,
             version:          Long,
             inventoryId:      String,
             label:            String,
             enabled:          Boolean,
             containerTypeId:  ContainerTypeId,
             sharedProperties: Option[ContainerSharedProperties],
             parentId:         Option[ContainerId],
             position:         Option[ContainerSchemaPosition],
             constraints:      Option[ContainerConstraints])
      : DomainValidation[StorageContainer] = {
    (Container.validate(id,
                        version,
                        inventoryId,
                        label,
                        containerTypeId,
                        sharedProperties,
                        parentId,
                        position) |@|
       Container.validatePosition(position) |@|
       Container.constraintsValidate(constraints)) { case _ =>
      StorageContainer(id               = id,
                       version          = version,
                       timeAdded        = OffsetDateTime.now,
                       timeModified     = None,
                       slug             = Slug(inventoryId),
                       inventoryId      = inventoryId,
                       label            = label,
                       enabled          = enabled,
                       containerTypeId  = containerTypeId,
                       sharedProperties = sharedProperties,
                       parentId         = parentId,
                       position         = position,
                       constraints      = constraints)
    }
  }
}

// For operations on SpecimenContainers see the Container companion object
final case class SpecimenContainer(id:               ContainerId,
                                   version:          Long,
                                   timeAdded:        OffsetDateTime,
                                   timeModified:     Option[OffsetDateTime],
                                   slug:             Slug,
                                   inventoryId:      String,
                                   label:            String,
                                   containerTypeId:  ContainerTypeId,
                                   sharedProperties: Option[ContainerSharedProperties],
                                   parentId:         Option[ContainerId],
                                   position:         Option[ContainerSchemaPosition])
    extends Container
    with ContainerValidations {
  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  def withInventoryId(inventoryId: String): DomainValidation[Container] = {
    validateInventoryId(inventoryId) map { _ =>
      update.copy(inventoryId = inventoryId)
    }
  }

  def withLabel(inventoryId: String): DomainValidation[Container] = {
    validateNonEmptyString(label) map { _ =>
      update.copy(label = label)
    }
  }

  def withPosition(position: Option[ContainerSchemaPosition]): DomainValidation[Container] = {
    ContainerSchemaPosition.validate(position) map { _ =>
      update.copy(position = position)
    }
  }

  def withParentPosition(
    parentId: Option[ContainerId],
    position: Option[ContainerSchemaPosition]
  ): DomainValidation[Container] = {
    (ContainerSchemaPosition.validate(position) |@|
       validateIdOption(parentId, ContainerParentIdInvalid)) { case _ =>
        update.copy(parentId   = parentId, position = position)
    }
  }

  private def update(): SpecimenContainer = {
    copy(version = version + 1L, timeModified = Some(OffsetDateTime.now))
  }

  override def toString: String =  {
    super.toString +
    s"""|
        |}""".stripMargin
  }
}

object SpecimenContainer extends ContainerValidations {

  def create(id:               ContainerId,
             version:          Long,
             inventoryId:      String,
             label:            String,
             containerTypeId:  ContainerTypeId,
             sharedProperties: Option[ContainerSharedProperties],
             parentId:         Option[ContainerId],
             position:         Option[ContainerSchemaPosition])
      : DomainValidation[SpecimenContainer] = {
    (Container.validate(id,
                        version,
                        inventoryId,
                        label,
                        containerTypeId,
                        sharedProperties,
                        parentId,
                        position) |@|
       Container.validatePosition(position)) { case _ =>
      SpecimenContainer(id               = id,
                        version          = version,
                        timeAdded        = OffsetDateTime.now,
                        timeModified     = None,
                        slug             = Slug(inventoryId),
                        inventoryId      = inventoryId,
                        label            = label,
                        containerTypeId  = containerTypeId,
                        sharedProperties = sharedProperties,
                        parentId         = parentId,
                        position         = position)
    }
  }

}
