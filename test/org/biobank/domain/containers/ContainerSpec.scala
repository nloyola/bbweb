package org.biobank.domain.containers

import java.time.OffsetDateTime
import org.biobank.domain._
import org.biobank.fixtures.NameGenerator
import org.scalatest.FunSpec
import org.slf4j.LoggerFactory
import scalaz.Scalaz._

trait ContainerSharedSpec[T <: Container] { this: FunSpec =>
  import org.biobank.TestUtils._
  import org.biobank.matchers.EntityMatchers._

  val factory: Factory

  protected val nameGenerator: NameGenerator

  protected def createFrom(container: T): DomainValidation[Container]

  protected def createEntity(): T

  protected def createWithId(id: ContainerId): T

  protected def createWithInventoryId(inventoryId: String): T

  protected def createWithVersion(version: Long): T

  protected def createWithContainerTypeId(id: ContainerTypeId): T

  protected def createWithParentId(id: ContainerId): T

  protected def createwithLabel(label: String): T

  describe("(container shared behaviour)") {

    it("can be created") {
      val container = createEntity
      createFrom(container) mustSucceed {
        _ must matchContainer(container)
      }
    }

    it("can have it's inventory ID updated") {
      val container   = createEntity
      val inventoryId = nameGenerator.next[Container]
      container.withInventoryId(inventoryId, Slug(inventoryId)) mustSucceed { c =>
        c must have('id (container.id),
                    'version (container.version + 1),
                    'slug (Slug(inventoryId).id),
                    'inventoryId (inventoryId),
                    'containerTypeId (container.containerTypeId))

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
      val container = createWithParentId(ContainerId(""))
      createFrom(container) mustFail "ContainerParentIdInvalid"
    }

    it("not be created with an invalid label") {
      val container = createwithLabel("")
      createFrom(container) mustFail ("ContainerSchemaLabelInvalid")
    }
  }
}

class StorageContainerSpec extends DomainSpec with ContainerSharedSpec[StorageContainer] {
  import org.biobank.TestUtils._
  import org.biobank.matchers.EntityMatchers._

  val log = LoggerFactory.getLogger(this.getClass)

  val nameGenerator = new NameGenerator(this.getClass)

  protected def createFrom(container: StorageContainer): DomainValidation[StorageContainer] =
    container match {
      case c: StorageContainer =>
        StorageContainer.create(id              = c.id,
                                version         = c.version,
                                inventoryId     = c.inventoryId,
                                containerTypeId = c.containerTypeId,
                                parentId        = c.parentId,
                                schemaLabel     = c.schemaLabel,
                                constraints     = c.constraints)
      case _ => DomainError("invalid container").failureNel[StorageContainer]
    }

  protected def createEntity(): StorageContainer = {
    val location = factory.createLocation()
    val centre   = factory.createEnabledCentre().copy(locations = Set(location))

    val label  = nameGenerator.next[ContainerSchema]
    val schema = factory.createContainerSchema(Set(label))

    val ctype  = factory.createStorageContainerType(centre, schema)
    val parent = factory.createRootContainer(centre, ctype)
    factory.createStorageContainer(ctype, parent, ContainerSchemaLabel(schema.id, label))
  }

  protected def createWithId(id: ContainerId): StorageContainer =
    createEntity.copy(id = id)

  protected def createWithInventoryId(inventoryId: String): StorageContainer =
    createEntity.copy(inventoryId = inventoryId)

  protected def createWithVersion(version: Long): StorageContainer =
    createEntity.copy(version = version)

  protected def createWithContainerTypeId(id: ContainerTypeId): StorageContainer =
    createEntity.copy(containerTypeId = id)

  protected def createWithParentId(id: ContainerId): StorageContainer =
    createEntity.copy(parentId = id)

  protected def createwithLabel(label: String): StorageContainer = {
    val entity = createEntity
    entity.copy(schemaLabel = entity.schemaLabel.copy(label = label))
  }

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
        val container = createEntity
        container.withEnabled(enabled, OffsetDateTime.now) mustSucceed { c =>
          c must matchContainer(
            container.copy(enabled      = enabled,
                           version      = container.version + 1L,
                           timeModified = Some(OffsetDateTime.now))
          )
        }
      }
    }

    it("can have it's constraints updated") {
      val container   = createEntity
      val constraints = Some(factory.createContainerConstraints)
      container.withConstraints(constraints, OffsetDateTime.now) mustSucceed {
        _ must matchContainer(
          container.copy(constraints  = constraints,
                         version      = container.version + 1L,
                         timeModified = Some(OffsetDateTime.now))
        )
      }
    }

    it("not be created with an invalid constraints") {
      val constraints = Some(factory.createContainerConstraints.copy(name = ""))
      val container   = factory.createStorageContainer().copy(constraints = constraints)
      createFrom(container) mustFail ("ContainerConstraintsInvalid")
    }
  }

}

class SpecimenContainerSpec extends DomainSpec with ContainerSharedSpec[SpecimenContainer] {
  import org.biobank.TestUtils._

  val log = LoggerFactory.getLogger(this.getClass)

  val nameGenerator = new NameGenerator(this.getClass)

  protected def createFrom(container: SpecimenContainer): DomainValidation[SpecimenContainer] =
    SpecimenContainer.create(id              = container.id,
                             version         = container.version,
                             inventoryId     = container.inventoryId,
                             containerTypeId = container.containerTypeId,
                             parentId        = container.parentId,
                             schemaLabel     = container.schemaLabel)

  protected def createEntity(): SpecimenContainer = {
    val location = factory.createLocation()
    val centre   = factory.createEnabledCentre().copy(locations = Set(location))

    val rootLabel  = nameGenerator.next[ContainerSchema]
    val rootSchema = factory.createContainerSchema(Set(rootLabel))

    val rootCType     = factory.createStorageContainerType(centre, rootSchema)
    val rootContainer = factory.createRootContainer(centre, rootCType)

    val storageLabel  = nameGenerator.next[ContainerSchema]
    val storageSchema = factory.createContainerSchema(Set(storageLabel))

    val storageCType = factory.createStorageContainerType(centre, storageSchema)
    val storageParent = factory
      .createStorageContainer(storageCType, rootContainer, ContainerSchemaLabel(rootSchema.id, rootLabel))

    val ctype = factory.createSpecimenContainerType(centre, factory.createContainerSchema)

    factory
      .createSpecimenContainer(ctype, storageParent, ContainerSchemaLabel(storageSchema.id, storageLabel))
  }

  protected def createWithId(id: ContainerId): SpecimenContainer =
    createEntity.copy(id = id)

  protected def createWithInventoryId(inventoryId: String): SpecimenContainer =
    createEntity.copy(inventoryId = inventoryId)

  protected def createWithVersion(version: Long): SpecimenContainer =
    createEntity.copy(version = version)

  protected def createWithContainerTypeId(id: ContainerTypeId): SpecimenContainer =
    createEntity.copy(containerTypeId = id)

  protected def createWithParentId(id: ContainerId): SpecimenContainer =
    createEntity.copy(parentId = id)

  protected def createwithLabel(label: String): SpecimenContainer = {
    val entity = createEntity
    entity.copy(schemaLabel = entity.schemaLabel.copy(label = label))
  }

  describe("A SpecimenContainer") {

    it("can be created with correct type") {
      val container = createEntity
      createFrom(container) mustSucceed { _ mustBe a[SpecimenContainer] }
    }
  }

}
