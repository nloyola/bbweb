package org.biobank.domain.containers

import java.time.OffsetDateTime
import org.biobank.domain.DomainSpec
import org.biobank.fixtures.NameGenerator
import org.slf4j.LoggerFactory
import org.biobank.domain._
import org.biobank.domain.centres.CentreId
import org.scalatest.FunSpec

trait ContainerTypeSharedSpec { this: FunSpec =>
  import org.biobank.TestUtils._
  import org.biobank.matchers.EntityMatchers._

  val factory: Factory

  protected val nameGenerator: NameGenerator

  protected def createEntity(): ContainerType

  protected def createFrom(containerType: ContainerType): DomainValidation[ContainerType]

  protected def createWithId(id: ContainerTypeId): ContainerType

  protected def createWithVersion(version: Long): ContainerType

  protected def createWithName(name: String): ContainerType

  protected def createWithDescription(description: Option[String]): ContainerType

  protected def createWithCentreId(id: CentreId): ContainerType

  protected def createWithSchemaId(id: ContainerSchemaId): ContainerType

  protected def createWithShared(shared: Boolean): ContainerType

  protected def createWithEnabled(enabled: Boolean): ContainerType

  describe("(container type shared behaviour)") {

    it("can be created") {
      val containerType = createEntity
      createFrom(containerType) mustSucceed {
        _ must matchContainerType(containerType)
      }
    }

    it("have it's name updated") {
      val containerType = createEntity
      val name          = nameGenerator.next[StorageContainer]

      containerType.withName(name) mustSucceed { updatedCt =>
        updatedCt.id must be(containerType.id)
        updatedCt.version must be(containerType.version + 1)
        updatedCt.name must be(name)
        updatedCt.description must be(containerType.description)
        updatedCt.centreId must be(containerType.centreId)
        updatedCt.schemaId must be(containerType.schemaId)
        updatedCt.shared must be(containerType.shared)
        updatedCt.enabled must be(containerType.enabled)

        updatedCt must beEntityWithTimeStamps(OffsetDateTime.now, Some(OffsetDateTime.now), 5L)
      }

    }

    it("have it's description updated") {
      val containerType = createEntity
      val description   = Some(nameGenerator.next[StorageContainer])

      containerType.withDescription(description) mustSucceed { updatedCt =>
        updatedCt.id must be(containerType.id)
        updatedCt.version must be(containerType.version + 1)
        updatedCt.name must be(containerType.name)
        updatedCt.description must be(description)
        updatedCt.centreId must be(containerType.centreId)
        updatedCt.schemaId must be(containerType.schemaId)
        updatedCt.shared must be(containerType.shared)
        updatedCt.enabled must be(containerType.enabled)

        updatedCt must beEntityWithTimeStamps(OffsetDateTime.now, Some(OffsetDateTime.now), 5L)
      }

    }

    it("can enable or disabled a container type") {
      Set(true, false).foreach { value =>
        val containerType = createEntity
        containerType.withEnabled(value) mustSucceed { updatedCt =>
          updatedCt.enabled must be(value)
          updatedCt must beEntityWithTimeStamps(OffsetDateTime.now, Some(OffsetDateTime.now), 5L)
        }
      }
    }

    it("not be created with an empty id") {
      createFrom(createWithId(id = ContainerTypeId(""))) mustFail "IdRequired"
    }

    it("not be created with an invalid version") {
      createFrom(createWithVersion(-2)) mustFail "InvalidVersion"
    }

    it("not be created with an null or empty name") {
      createFrom(createWithName(null)) mustFail "InvalidName"
      createFrom(createWithName("")) mustFail "InvalidName"
    }

    it("not be created with an empty description option") {
      createFrom(createWithDescription(Some(null))) mustFail "InvalidDescription"
      createFrom(createWithDescription(Some(""))) mustFail "InvalidDescription"
    }

    it("not be created with an invalid centre id") {
      createFrom(createWithCentreId(CentreId(""))) mustFail "CentreIdRequired"
    }

    it("not be created with an invalid schema id") {
      createFrom(createWithSchemaId(ContainerSchemaId(""))) mustFail "ContainerSchemaIdInvalid"
    }

  }

}

class StorageContainerTypeSpec extends DomainSpec with ContainerTypeSharedSpec {
  import org.biobank.TestUtils._
  import org.biobank.matchers.EntityMatchers._

  val log = LoggerFactory.getLogger(this.getClass)

  val nameGenerator = new NameGenerator(this.getClass)

  def createFrom(containerType: ContainerType): DomainValidation[ContainerType] =
    StorageContainerType.create(id          = containerType.id,
                                version     = containerType.version,
                                name        = containerType.name,
                                description = containerType.description,
                                centreId    = containerType.centreId,
                                schemaId    = containerType.schemaId,
                                shared      = containerType.shared,
                                enabled     = containerType.enabled)

  protected def createEntity(): ContainerType = factory.createStorageContainerType

  protected def createWithId(id: ContainerTypeId): ContainerType =
    factory.createStorageContainerType.copy(id = id)

  protected def createWithVersion(version: Long): ContainerType =
    factory.createStorageContainerType.copy(version = version)

  protected def createWithName(name: String): ContainerType =
    factory.createStorageContainerType.copy(name = name)

  protected def createWithDescription(description: Option[String]): ContainerType =
    factory.createStorageContainerType.copy(description = description)

  protected def createWithCentreId(id: CentreId): ContainerType =
    factory.createStorageContainerType.copy(centreId = id)

  protected def createWithSchemaId(id: ContainerSchemaId): ContainerType =
    factory.createStorageContainerType.copy(schemaId = id)

  protected def createWithShared(shared: Boolean): ContainerType =
    factory.createStorageContainerType.copy(shared = shared)

  protected def createWithEnabled(enabled: Boolean): ContainerType =
    factory.createStorageContainerType.copy(enabled = enabled)

  describe("A Storage Container Type") {

    it("can be created wth correct type") {
      val containerType = factory.createStorageContainerType
      createFrom(containerType) mustSucceed { ct =>
        ct mustBe a[StorageContainerType]
        ct must matchContainerType(containerType)
      }
    }
  }

}

class SpecimenContainerTypeSpec extends DomainSpec with ContainerTypeSharedSpec {
  import org.biobank.TestUtils._
  import org.biobank.matchers.EntityMatchers._

  val log = LoggerFactory.getLogger(this.getClass)

  val nameGenerator = new NameGenerator(this.getClass)

  def createFrom(containerType: ContainerType): DomainValidation[ContainerType] =
    SpecimenContainerType.create(id          = containerType.id,
                                 version     = containerType.version,
                                 name        = containerType.name,
                                 description = containerType.description,
                                 centreId    = containerType.centreId,
                                 schemaId    = containerType.schemaId,
                                 shared      = containerType.shared,
                                 enabled     = containerType.enabled)

  protected def createEntity(): ContainerType = factory.createSpecimenContainerType

  protected def createWithId(id: ContainerTypeId): ContainerType =
    factory.createSpecimenContainerType.copy(id = id)

  protected def createWithVersion(version: Long): ContainerType =
    factory.createSpecimenContainerType.copy(version = version)

  protected def createWithName(name: String): ContainerType =
    factory.createSpecimenContainerType.copy(name = name)

  protected def createWithDescription(description: Option[String]): ContainerType =
    factory.createSpecimenContainerType.copy(description = description)

  protected def createWithCentreId(id: CentreId): ContainerType =
    factory.createSpecimenContainerType.copy(centreId = id)

  protected def createWithSchemaId(id: ContainerSchemaId): ContainerType =
    factory.createSpecimenContainerType.copy(schemaId = id)

  protected def createWithShared(shared: Boolean): ContainerType =
    factory.createSpecimenContainerType.copy(shared = shared)

  protected def createWithEnabled(enabled: Boolean): ContainerType =
    factory.createSpecimenContainerType.copy(enabled = enabled)

  describe("A Specimen Container Type") {

    it("can be created wth correct type") {
      val containerType = factory.createSpecimenContainerType
      createFrom(containerType) mustSucceed { ct =>
        ct mustBe a[SpecimenContainerType]
        ct must matchContainerType(containerType)
      }
    }
  }

}
