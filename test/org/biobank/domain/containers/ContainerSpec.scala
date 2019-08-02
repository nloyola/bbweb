package org.biobank.domain.containers

import java.time.OffsetDateTime
import org.biobank.domain.DomainValidation
import org.biobank.domain._
import org.biobank.fixtures.NameGenerator
import org.scalatest.FunSpec
import org.slf4j.LoggerFactory
import scalaz.Scalaz._

trait ContainerSharedSpec { this: FunSpec =>
  import org.biobank.TestUtils._
  import org.biobank.matchers.EntityMatchers._

  val factory: Factory

  protected val nameGenerator: NameGenerator

  protected def createFrom(container: Container): DomainValidation[Container]

  protected def createEntity(): Container

  protected def createWithId(id: ContainerId): Container

  protected def createWithInventoryId(inventoryId: String): Container

  protected def createWithVersion(version: Long): Container

  protected def createWithContainerTypeId(id: ContainerTypeId): Container

  protected def createWithParentId(id: Option[ContainerId]): Container

  protected def createWithPosition(id: Option[ContainerSchemaPosition]): Container

  describe("(container shared behaviour)") {

    it("can be created") {
      val container = createEntity
      createFrom(container) mustSucceed {
        _ must matchContainer(container)
      }
    }

    it("can have it's inventory ID updated") {
      val container = createEntity
      val inventoryId = nameGenerator.next[Container]
      container.withInventoryId(inventoryId) mustSucceed { c =>
        c must have (
          'id              (container.id),
          'version         (container.version + 1),
          'slug            (container.slug.id),
          'inventoryId     (inventoryId),
          'containerTypeId (container.containerTypeId),
          'parentId        (container.parentId),
          'position        (container.position)
        )

        c must beEntityWithTimeStamps(OffsetDateTime.now, Some(OffsetDateTime.now), 5L)
      }
    }

    it("can have it's position ID updated") {
      val container = createEntity
      val position  = Some(factory.createContainerSchemaPosition)
      container.withPosition(position) mustSucceed{ c =>
        c must have (
          'id              (container.id),
          'version         (container.version + 1),
          'slug            (container.slug.id),
          'inventoryId     (container.inventoryId),
          'containerTypeId (container.containerTypeId),
          'parentId        (container.parentId),
          'position        (position)
        )

        c must beEntityWithTimeStamps(OffsetDateTime.now, Some(OffsetDateTime.now), 5L)
      }
    }

    it("can have it's parent ID and position ID updated") {
      val container = createEntity
      val parentId = Some(ContainerId(nameGenerator.next[Container]))
      val position = Some(factory.createContainerSchemaPosition)
      container.withParentPosition(parentId, position) mustSucceed{ c =>
        c must have (
          'id              (container.id),
          'version         (container.version + 1),
          'slug            (container.slug.id),
          'inventoryId     (container.inventoryId),
          'containerTypeId (container.containerTypeId),
          'parentId        (parentId),
          'position        (position)
        )

        c must beEntityWithTimeStamps(OffsetDateTime.now, Some(OffsetDateTime.now), 5L)
      }
    }

    it("not be created with an empty id") {
      val container = createWithId(ContainerId(""))
      createFrom(container) mustFail "IdRequired"
    }

    it("not be created with an empty inventory id") {
      val container = createWithInventoryId("")
      createFrom(container) mustFail "ContainerInventoryIdInvalid"
    }

    it("not be created with an invalid version") {
      val container = createWithVersion(-1)
      createFrom(container) mustFail "InvalidVersion"
    }

    it("not be created with an invalid container type ID") {
      val container = createWithContainerTypeId(ContainerTypeId(""))
      createFrom(container) mustFail "ContainerTypeIdInvalid"
    }

    it("not be created with an invalid parent ID") {
      val container = createWithParentId(Some(ContainerId("")))
      createFrom(container) mustFail "ContainerParentIdInvalid"
    }

    it("not be created with an invalid position") {
      val position = factory.createContainerSchemaPosition.copy(id = ContainerSchemaPositionId(""))
      val container = createWithPosition(Some(position))
      createFrom(container) mustFail("ContainerSchemaPositionInvalid", "IdRequired")
    }
  }
}

class StorageContainerSpec extends DomainSpec with ContainerSharedSpec {
  import org.biobank.TestUtils._
  import org.biobank.matchers.EntityMatchers._

  val log = LoggerFactory.getLogger(this.getClass)

  val nameGenerator = new NameGenerator(this.getClass)

  protected def createFrom(container: Container): DomainValidation[StorageContainer] = {
    container match {
      case c: StorageContainer =>
        StorageContainer.create(id               = c.id,
                                version          = c.version,
                                inventoryId      = c.inventoryId,
                                label            = c.label,
                                enabled          = c.enabled,
                                containerTypeId  = c.containerTypeId,
                                sharedProperties = c.sharedProperties,
                                parentId         = c.parentId,
                                position         = c.position,
                                constraints      = c.constraints)
      case _ => DomainError("invalid container").failureNel[StorageContainer]
    }
  }

  protected def createEntity(): StorageContainer = factory.createStorageContainer()

  protected def createWithId(id: ContainerId): Container =
    factory.createStorageContainer().copy(id = id)

  protected def createWithInventoryId(inventoryId: String): Container =
    factory.createStorageContainer().copy(inventoryId = inventoryId)

  protected def createWithVersion(version: Long): Container =
    factory.createStorageContainer().copy(version = version)

  protected def createWithContainerTypeId(id: ContainerTypeId): Container =
    factory.createStorageContainer().copy(containerTypeId = id)

  protected def createWithParentId(id: Option[ContainerId]): Container =
    factory.createStorageContainer().copy(parentId = id)

  protected def createWithPosition(position: Option[ContainerSchemaPosition]): Container =
    factory.createStorageContainer().copy(position = position)

  describe("A StorageContainer") {

    it("can be created with correct type") {
      val container = createEntity
      createFrom(container) mustSucceed { c =>
        c mustBe a[StorageContainer]
        c must matchContainer(container)
      }
    }

    it("can be enabled and disabled") {
      Set(true, false).foreach { enabled =>
        val container = factory.createStorageContainer()
        container.withEnabled(enabled) mustSucceed { c =>
          c must matchContainer(container.copy(enabled = enabled,
                                               version = container.version + 1L,
                                               timeModified = Some(OffsetDateTime.now)))
        }
      }
    }

    it("can have it's constraints updated") {
      val container = createEntity
      val constraints = Some(factory.createContainerConstraints)
      container.withConstraints(constraints) mustSucceed {
        _ must matchContainer(container.copy(constraints = constraints,
                                             version = container.version + 1L,
                                             timeModified = Some(OffsetDateTime.now)))
      }
    }

    it("not be created with an invalid constraints") {
      val constraints = Some(factory.createContainerConstraints.copy(id = ContainerConstraintsId("")))
      val container = factory.createStorageContainer().copy(constraints = constraints)
      createFrom(container) mustFail("ContainerConstraintsInvalid", "IdRequired")
    }
  }

}

class SpecimenContainerSpec extends DomainSpec with ContainerSharedSpec {
  import org.biobank.TestUtils._

  val log = LoggerFactory.getLogger(this.getClass)

  val nameGenerator = new NameGenerator(this.getClass)

  protected def createFrom(container: Container): DomainValidation[Container] =
    SpecimenContainer.create(id               = container.id,
                             version          = container.version,
                             inventoryId      = container.inventoryId,
                             label            = container.label,
                             containerTypeId  = container.containerTypeId,
                             sharedProperties = container.sharedProperties,
                             parentId         = container.parentId,
                             position         = container.position)

  protected def createEntity(): Container = factory.createSpecimenContainer()

  protected def createWithId(id: ContainerId): Container =
    factory.createStorageContainer().copy(id = id)

  protected def createWithInventoryId(inventoryId: String): Container =
    factory.createSpecimenContainer().copy(inventoryId = inventoryId)

  protected def createWithVersion(version: Long): Container =
    factory.createSpecimenContainer().copy(version = version)

  protected def createWithContainerTypeId(id: ContainerTypeId): Container =
    factory.createSpecimenContainer().copy(containerTypeId = id)

  protected def createWithParentId(id: Option[ContainerId]): Container =
    factory.createSpecimenContainer().copy(parentId = id)

  protected def createWithPosition(position: Option[ContainerSchemaPosition]): Container =
    factory.createSpecimenContainer().copy(position = position)

  describe("A SpecimenContainer") {

    it("can be created with correct type") {
      val container = createEntity
      createFrom(container) mustSucceed { _ mustBe a[SpecimenContainer] }
    }
  }

}
