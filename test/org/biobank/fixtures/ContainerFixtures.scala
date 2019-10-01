package org.biobank.fixtures

import org.biobank.domain._
import org.biobank.domain.centres.Centre
import org.biobank.domain.containers._
import org.scalatest.OptionValues._

trait ContainerFixture[C <: Container, T <: ContainerType] {
  val container:     C
  val containerType: T
  val schema:        ContainerSchema

  def allEntities: Set[ConcurrencySafeEntity[_]]

  def allEntitiesButContainer: Set[ConcurrencySafeEntity[_]]

  def containerWithInventoryId(inventoryId: String): C

  def containerWithLabel(label: String): C

  def createSiblingContainer(factory: Factory): C

  def createContainerType(factory: Factory): T

  def createChild(factory: Factory): Option[(Container, ContainerType, ContainerSchema)]

  def getCentre(): Centre

}

trait ChildContainerFixture[C <: Container, T <: ContainerType, PC <: Container]
    extends ContainerFixture[C, T] {
  val parent: ContainerFixture[PC, StorageContainerType]

  def createParentContainer(factory: Factory): PC
}

// schemas need minimum 2 labels for some tests
case class RootContainerFixture(
    container:     RootContainer,
    containerType: StorageContainerType,
    schema:        ContainerSchema,
    centre:        Centre)
    extends ContainerFixture[RootContainer, StorageContainerType] {

  def allEntities: Set[ConcurrencySafeEntity[_]] =
    Set(container, containerType, schema, centre)

  def allEntitiesButContainer: Set[ConcurrencySafeEntity[_]] =
    Set(containerType, schema, centre)

  def containerWithInventoryId(inventoryId: String) = container.copy(inventoryId = inventoryId)

  def containerWithLabel(label: String) =
    container.copy(label = label)

  def createSiblingContainer(factory: Factory) = factory.createRootContainer(centre, containerType)

  def createContainerType(factory: Factory) = factory.createStorageContainerType

  def createChild(factory: Factory) = {
    val childSchema        = factory.createContainerSchema.copy(labels = Set("A1", "B1"))
    val childContainerType = factory.createStorageContainerType(centre, childSchema)
    val childContainer = factory.createStorageContainer(
      childContainerType,
      container,
      ContainerSchemaLabel(schema.id, schema.labels.toSeq(1))
    )
    Some((childContainer, childContainerType, childSchema))
  }

  def getCentre() = centre

}

object RootContainerFixture {

  val labels = Set("AA", "AB")

  // schemas need minimum 2 labels for some tests
  def apply(factory: Factory): RootContainerFixture = {
    val location      = factory.createLocation
    val centre        = factory.defaultEnabledCentre.copy(locations = Set(location))
    val schema        = factory.createContainerSchema.copy(labels = labels)
    val containerType = factory.createStorageContainerType
    val container     = factory.createRootContainer(centre, containerType)

    RootContainerFixture(container, containerType, schema, centre)
  }

  def apply(factory: Factory, centre: Centre): RootContainerFixture = {
    val schema        = factory.createContainerSchema.copy(labels = labels)
    val containerType = factory.createStorageContainerType
    val container     = factory.createRootContainer(centre, containerType)

    RootContainerFixture(container, containerType, schema, centre)
  }

}

// schemas need minimum 2 labels for some tests
case class StorageContainerFixture(
    container:     StorageContainer,
    containerType: StorageContainerType,
    schema:        ContainerSchema,
    parent:        RootContainerFixture)
    extends ChildContainerFixture[StorageContainer, StorageContainerType, RootContainer] {

  def allEntities: Set[ConcurrencySafeEntity[_]] =
    Set(container, containerType, schema) ++ parent.allEntities

  def allEntitiesButContainer: Set[ConcurrencySafeEntity[_]] =
    Set(containerType, schema) ++ parent.allEntities

  def containerWithInventoryId(inventoryId: String) = container.copy(inventoryId = inventoryId)

  def containerWithLabel(label: String) = {
    val schemaLabel = container.schemaLabel.copy(label = label)
    container.copy(schemaLabel = schemaLabel)
  }

  def createParentContainer(factory: Factory) =
    factory.createRootContainer(parent.centre, parent.containerType)

  def createSiblingContainer(factory: Factory) = {
    val labels = parent.schema.labels.toSeq
    factory.createStorageContainer(containerType,
                                   parent.container,
                                   ContainerSchemaLabel(parent.schema.id, labels(1)))
  }

  def createContainerType(factory: Factory) = factory.createStorageContainerType

  def createChild(factory: Factory) = {
    val childSchema        = factory.createContainerSchema.copy(labels = StorageContainerFixture.labels)
    val childContainerType = factory.createSpecimenContainerType(childSchema)
    val childContainer = factory.createSpecimenContainer(
      childContainerType,
      container,
      ContainerSchemaLabel(schema.id, StorageContainerFixture.labels.toSeq(1))
    )
    Some((childContainer, childContainerType, childSchema))
  }

  def getCentre() = parent.centre
}

object StorageContainerFixture {

  val labels = Set("A1", "A2")

  // schemas need minimum 2 labels for some tests
  def apply(factory: Factory): StorageContainerFixture = {
    val rootFixture          = RootContainerFixture(factory)
    val storageSchema        = factory.createContainerSchema.copy(labels = labels)
    val storageContainerType = factory.createStorageContainerType(rootFixture.centre, storageSchema)

    val storageContainer = factory.createStorageContainer(
      storageContainerType,
      rootFixture.container,
      ContainerSchemaLabel(rootFixture.schema.id, rootFixture.schema.labels.headOption.value)
    )

    StorageContainerFixture(container     = storageContainer,
                            containerType = storageContainerType,
                            schema        = storageSchema,
                            parent        = rootFixture)
  }

  // schemas need minimum 2 labels for some tests
  def apply(factory: Factory, centre: Centre): StorageContainerFixture = {
    val rootFixture          = RootContainerFixture(factory, centre)
    val storageSchema        = factory.createContainerSchema.copy(labels = labels)
    val storageContainerType = factory.createStorageContainerType(centre, storageSchema)

    val storageContainer = factory.createStorageContainer(
      storageContainerType,
      rootFixture.container,
      ContainerSchemaLabel(rootFixture.schema.id, rootFixture.schema.labels.headOption.value)
    )

    StorageContainerFixture(container     = storageContainer,
                            containerType = storageContainerType,
                            schema        = storageSchema,
                            parent        = rootFixture)
  }
}

// schemas need minimum 2 labels for some tests
case class SpecimenContainerFixture(
    container:     SpecimenContainer,
    containerType: SpecimenContainerType,
    schema:        ContainerSchema,
    parent:        StorageContainerFixture)
    extends ChildContainerFixture[SpecimenContainer, SpecimenContainerType, StorageContainer] {

  def allEntities: Set[ConcurrencySafeEntity[_]] =
    Set(container, containerType, schema) ++ parent.allEntities

  def allEntitiesButContainer: Set[ConcurrencySafeEntity[_]] =
    Set(containerType, schema) ++ parent.allEntities

  def containerWithInventoryId(inventoryId: String) = container.copy(inventoryId = inventoryId)

  def containerWithLabel(label: String) = {
    val schemaLabel = container.schemaLabel.copy(label = label)
    container.copy(schemaLabel = schemaLabel)
  }

  def createParentContainer(factory: Factory) =
    factory.createStorageContainer(
      parent.containerType,
      parent.parent.container,
      ContainerSchemaLabel(parent.parent.schema.id, parent.parent.schema.labels.toSeq(1))
    )

  def createSiblingContainer(factory: Factory) = {
    val labels = parent.schema.labels.toSeq
    factory.createSpecimenContainer(containerType,
                                    parent.container,
                                    ContainerSchemaLabel(parent.schema.id, labels(1)))
  }

  def createContainerType(factory: Factory) = factory.createSpecimenContainerType

  def createChild(factory: Factory) = None

  def getCentre() = parent.parent.centre

}

object SpecimenContainerFixture {

  def apply(factory: Factory): SpecimenContainerFixture = {
    val storageFixture = StorageContainerFixture(factory)

    val specimenSchema =
      factory.createContainerSchema.copy(labels = Set(factory.createContainerSchemaLabel.label))

    val specimenContainerType = factory.createSpecimenContainerType(specimenSchema)

    val specimenContainer = factory.createSpecimenContainer(
      specimenContainerType,
      storageFixture.container,
      ContainerSchemaLabel(storageFixture.schema.id, storageFixture.schema.labels.headOption.value)
    )

    SpecimenContainerFixture(container     = specimenContainer,
                             containerType = specimenContainerType,
                             schema        = specimenSchema,
                             parent        = storageFixture)
  }

  def apply(factory: Factory, centre: Centre): SpecimenContainerFixture = {
    val storageFixture = StorageContainerFixture(factory, centre)

    val specimenSchema =
      factory.createContainerSchema.copy(labels = Set(factory.createContainerSchemaLabel.label))

    val specimenContainerType = factory.createSpecimenContainerType(specimenSchema)

    val specimenContainer = factory.createSpecimenContainer(
      specimenContainerType,
      storageFixture.container,
      ContainerSchemaLabel(storageFixture.schema.id, storageFixture.schema.labels.headOption.value)
    )

    SpecimenContainerFixture(container     = specimenContainer,
                             containerType = specimenContainerType,
                             schema        = specimenSchema,
                             parent        = storageFixture)
  }
}
