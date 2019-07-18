package org.biobank.domain.containers

import java.time.OffsetDateTime
import org.biobank.ValidationKey
import org.biobank.domain._
import play.api.libs.json._
import scalaz.Scalaz._
import scalaz.NonEmptyList._

trait ContainerValidations {

  case object ContainerInventoryIdInvalid extends ValidationKey

  case object ContainerTypeIdInvalid extends ValidationKey

  case object ContainerSchemaIdInvalid extends ValidationKey

  case object ContainerParentIdInvalid extends ValidationKey

  case object ContainerSchemaPositionInvalid extends ValidationKey

  case object ContainerConstraintsInvalid extends ValidationKey

}

/**
 * A specifically built physical unit that can hold child containers, or can be contained in a parent
 * container.
 */
sealed trait Container extends ConcurrencySafeEntity[ContainerId]
    with HasSlug
    with ContainerValidations {
  import org.biobank.CommonValidations._

 /**
  * An inventory identifier, such as a barcode. Global uniqueness is required so that
  * [[domain.containers.Container Containers]], like [[domain.participants.Specimen Specimen]]s, can be
  * shipped between [[domain.centres.Centre Centres]].
  */
  val inventoryId: String

  /** The ID of the container type that classifiies this [[Container]]. */
  val containerTypeId: ContainerTypeId

 /** The ID of the [[Container]] that this container is stored in. */
  val parentId: Option[ContainerId]

  /**
   * The position this [[Container]] has in its parent, or none if there is no specific
   * position. This value is always none if there is no parent.
   */
  val position: Option[ContainerSchemaPosition]

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

  def validate(id:              ContainerId,
               version:         Long,
               inventoryId:     String,
               containerTypeId: ContainerTypeId,
               parentId:        Option[ContainerId],
               position:        Option[ContainerSchemaPosition]): DomainValidation[Boolean] = {
    (validateId(id) |@|
       validateVersion(version) |@|
       validateNonEmptyString(inventoryId, ContainerInventoryIdInvalid) |@|
       validateId(containerTypeId, ContainerTypeIdInvalid) |@|
       validateIdOption(parentId, ContainerParentIdInvalid) |@|
       ContainerSchemaPosition.validate(position)) { case _ => true }
  }

  def validatePosition(position: Option[ContainerSchemaPosition])
      : DomainValidation[Boolean] = {
    ContainerSchemaPosition.validate(position).leftMap { err =>
      nel(ContainerSchemaPositionInvalid.toString, err.list)
    }
  }

  def constraintsValidate(constraints: Option[ContainerConstraints])
      : DomainValidation[Boolean] = {
    ContainerConstraints.validate(constraints).leftMap { err =>
      nel(ContainerConstraintsInvalid.toString, err.list)
    }
  }

  implicit val containerWrites: Writes[Container] = new Writes[Container] {
      def writes(container: Container): JsValue = Json.obj(
          "id"              -> container.id,
          "version"         -> container.version,
          "timeAdded"       -> container.timeAdded,
          "timeModified"    -> container.timeModified,
          "slug"            -> container.slug,
          "inventoryId"     -> container.inventoryId,
          "containerTypeId" -> container.containerTypeId,
          "parentId"        -> container.parentId,
          "position"        -> container.position
        )
    }

}

// For operations on StorageContainers see the Container companion object
final case class StorageContainer(id:              ContainerId,
                                  version:         Long,
                                  timeAdded:       OffsetDateTime,
                                  timeModified:    Option[OffsetDateTime],
                                  slug:            Slug,
                                  inventoryId:     String,
                                  enabled:         Boolean,
                                  containerTypeId: ContainerTypeId,
                                  parentId:        Option[ContainerId],
                                  position:        Option[ContainerSchemaPosition],
                                  constraints:     Option[ContainerConstraints])
    extends Container
    with ContainerValidations {

  import org.biobank.domain.DomainValidations._

  def withInventoryId(inventoryId: String): DomainValidation[Container] = {
    validateInventoryId(inventoryId) map { _ =>
      update.copy(inventoryId = inventoryId)
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

  def create(
    id:              ContainerId,
    version:         Long,
    inventoryId:     String,
    enabled:         Boolean,
    containerTypeId: ContainerTypeId,
    parentId:        Option[ContainerId],
    position:        Option[ContainerSchemaPosition],
    constraints:     Option[ContainerConstraints]
  ): DomainValidation[StorageContainer] = {
    (Container.validate(id, version, inventoryId, containerTypeId, parentId, position) |@|
       Container.validatePosition(position) |@|
       Container.constraintsValidate(constraints)) { case _ =>
        StorageContainer(id              = id,
                         version         = version,
                         timeAdded       = OffsetDateTime.now,
                         timeModified    = None,
                         slug            = Slug(inventoryId),
                         inventoryId     = inventoryId,
                         enabled         = enabled,
                         containerTypeId = containerTypeId,
                         parentId        = parentId,
                         position        = position,
                         constraints     = constraints)
    }
  }
}

// For operations on SpecimenContainers see the Container companion object
final case class SpecimenContainer(id:              ContainerId,
                                   version:         Long,
                                   timeAdded:       OffsetDateTime,
                                   timeModified:    Option[OffsetDateTime],
                                   slug:            Slug,
                                   inventoryId:     String,
                                   containerTypeId: ContainerTypeId,
                                   parentId:        Option[ContainerId],
                                   position:        Option[ContainerSchemaPosition])
    extends Container
    with ContainerValidations {
  import org.biobank.domain.DomainValidations._

  def withInventoryId(inventoryId: String): DomainValidation[Container] = {
    validateInventoryId(inventoryId) map { _ =>
      update.copy(inventoryId = inventoryId)
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

  def create(
    id:              ContainerId,
    version:         Long,
    inventoryId:     String,
    containerTypeId: ContainerTypeId,
    parentId:        Option[ContainerId],
    position:        Option[ContainerSchemaPosition]
  ): DomainValidation[SpecimenContainer] = {
    (Container.validate(id, version, inventoryId, containerTypeId, parentId, position) |@|
       Container.validatePosition(position)) { case _ =>
        SpecimenContainer(id              = id,
                          version         = version,
                          timeAdded       = OffsetDateTime.now,
                          timeModified    = None,
                          slug            = Slug(inventoryId),
                          inventoryId     = inventoryId,
                          containerTypeId = containerTypeId,
                          parentId        = parentId,
                          position        = position)
    }
  }

}
