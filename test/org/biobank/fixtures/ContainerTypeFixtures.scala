package org.biobank.fixtures

import org.biobank.domain._
import org.biobank.domain.centres.Centre
import org.biobank.domain.containers._
import org.scalatest.OptionValues._

trait ContainerTypeFixture[T <: ContainerType] {
  val centre:        Centre
  val schema:        ContainerSchema
  val containerType: T

  def allEntities: Set[ConcurrencySafeEntity[_]]

  def allEntitiesButContainerType: Set[ConcurrencySafeEntity[_]]

  def createSibling(factory: Factory): T

  def createContainer(factory: Factory): Container

}

case class StorageContainerTypeFixture(
    centre:        Centre,
    schema:        ContainerSchema,
    containerType: StorageContainerType)
    extends ContainerTypeFixture[StorageContainerType] {

  def allEntities: Set[ConcurrencySafeEntity[_]] =
    Set(centre, schema, containerType)

  def allEntitiesButContainerType: Set[ConcurrencySafeEntity[_]] =
    Set(centre, schema)

  def createSibling(factory: Factory): StorageContainerType =
    factory.createStorageContainerType(centre, schema)

  def createContainer(factory: Factory): StorageContainer = {
    val labelInfo     = ContainerSchemaLabel(schema.id, StorageContainerTypeFixture.labels.headOption.value)
    val rootContainer = factory.createRootContainer(centre, containerType)
    factory.createStorageContainer(containerType, rootContainer, labelInfo)
  }

}

object StorageContainerTypeFixture {

  val labels = Set("AA", "AB")

  // schemas need minimum 2 labels for some tests
  def apply(factory: Factory): StorageContainerTypeFixture = {
    val centre        = factory.createDisabledCentre()
    val schema        = factory.createContainerSchema.copy(labels = labels)
    val containerType = factory.createStorageContainerType(centre, schema)

    StorageContainerTypeFixture(centre, schema, containerType)
  }

  // schemas need minimum 2 labels for some tests
  def apply(factory: Factory, centre: Centre): StorageContainerTypeFixture = {
    val schema        = factory.createContainerSchema.copy(labels = labels)
    val containerType = factory.createStorageContainerType(centre, schema)

    StorageContainerTypeFixture(centre, schema, containerType)
  }
}

case class SpecimenContainerTypeFixture(
    centre:        Centre,
    schema:        ContainerSchema,
    containerType: SpecimenContainerType)
    extends ContainerTypeFixture[SpecimenContainerType] {

  def allEntities: Set[ConcurrencySafeEntity[_]] =
    Set(centre, schema, containerType)

  def allEntitiesButContainerType: Set[ConcurrencySafeEntity[_]] =
    Set(centre, schema)

  def createSibling(factory: Factory): SpecimenContainerType =
    factory.createSpecimenContainerType(centre, schema)

  def createContainer(factory: Factory): SpecimenContainer = {
    val storageFixture = StorageContainerTypeFixture(factory)
    val schemaLabel =
      ContainerSchemaLabel(storageFixture.schema.id, storageFixture.schema.labels.headOption.value)
    val storageContainer = storageFixture.createContainer(factory)
    factory.createSpecimenContainer(containerType, storageContainer, schemaLabel)
  }

}

object SpecimenContainerTypeFixture {

  val labels = Set("A1", "A2")

  def apply(factory: Factory): SpecimenContainerTypeFixture = {
    val centre        = factory.createDisabledCentre()
    val schema        = factory.createContainerSchema.copy(labels = labels)
    val containerType = factory.createSpecimenContainerType(centre, schema)

    SpecimenContainerTypeFixture(centre, schema, containerType)
  }

  def apply(factory: Factory, centre: Centre): SpecimenContainerTypeFixture = {
    val schema        = factory.createContainerSchema.copy(labels = labels)
    val containerType = factory.createSpecimenContainerType(centre, schema)

    SpecimenContainerTypeFixture(centre, schema, containerType)
  }
}
