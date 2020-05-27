package org.biobank.domain.containers

import java.time.OffsetDateTime
import org.biobank.domain.DomainSpec
import org.biobank.domain.centres.CentreId
import org.biobank.fixtures.NameGenerator
import org.slf4j.LoggerFactory

class ContainerSchemaSpec extends DomainSpec {
  import org.biobank.TestUtils._
  import org.biobank.matchers.EntityMatchers._
  import org.scalatest.matchers.must.Matchers._

  val log = LoggerFactory.getLogger(this.getClass)

  val nameGenerator = new NameGenerator(this.getClass)

  def createFrom(schema: ContainerSchema) =
    ContainerSchema.create(id          = schema.id,
                           version     = schema.version,
                           name        = schema.name,
                           description = schema.description,
                           shared      = schema.shared,
                           centreId    = schema.centreId)

  describe("A container schema can") {

    it("be created") {
      val containerSchema = factory.createContainerSchema.copy(labels = Set.empty[String])
      createFrom(containerSchema) mustSucceed { s =>
        s mustBe a[ContainerSchema]
        s must matchContainerSchema(containerSchema)
      }
    }

    it("have it's name updated") {

      val containerSchema = factory.createContainerSchema
      val name            = nameGenerator.next[ContainerSchema]

      containerSchema.withName(name) mustSucceed {
        _ must matchContainerSchema(
          containerSchema.copy(name         = name,
                               version      = containerSchema.version + 1L,
                               timeModified = Some(OffsetDateTime.now))
        )
      }
    }

    it("have it's description updated") {
      val containerSchema = factory.createContainerSchema
      val description     = Some(nameGenerator.next[ContainerSchema])

      containerSchema.withDescription(description) mustSucceed {
        _ must matchContainerSchema(
          containerSchema.copy(description  = description,
                               version      = containerSchema.version + 1L,
                               timeModified = Some(OffsetDateTime.now))
        )
      }
    }

    it("have it's shared attribute updated") {
      Set(true, false).foreach { shared =>
        val containerSchema = factory.createContainerSchema.copy(shared = !shared)

        containerSchema.withShared(shared) mustSucceed {
          _ must matchContainerSchema(
            containerSchema.copy(shared       = shared,
                                 version      = containerSchema.version + 1L,
                                 timeModified = Some(OffsetDateTime.now))
          )
        }
      }
    }

    it("have it's centre ID attribute updated") {
      val containerSchema = factory.createContainerSchema
      val centreId        = CentreId(nameGenerator.next[ContainerSchema])

      containerSchema.withCentre(centreId) mustSucceed {
        _ must matchContainerSchema(
          containerSchema.copy(centreId     = centreId,
                               version      = containerSchema.version + 1L,
                               timeModified = Some(OffsetDateTime.now))
        )
      }
    }

    it("have it's labels updated") {
      val schema = factory.createContainerSchema.copy(labels = Set.empty[String])
      val labels = Set(nameGenerator.next[ContainerSchema])

      schema.withLabels(labels) mustSucceed {
        _ must matchContainerSchema(
          schema.copy(labels = labels, version = schema.version + 1L, timeModified = Some(OffsetDateTime.now))
        )
      }
    }

  }

  describe("A container schema can not") {

    it("be created with an empty id") {
      val schema = factory.createContainerSchema.copy(id = ContainerSchemaId(""))
      createFrom(schema) mustFail "IdRequired"
    }

    it("be created with an invalid version") {
      val schema = factory.createContainerSchema.copy(version = -2)
      createFrom(schema) mustFail "InvalidVersion"
    }

    it("be created with an null or empty name") {
      var schema = factory.createContainerSchema.copy(name = null)
      createFrom(schema) mustFail "InvalidName"

      schema = factory.createContainerSchema.copy(name = "")
      createFrom(schema) mustFail "InvalidName"
    }

    it("be created with an empty description option") {
      var schema = factory.createContainerSchema.copy(description = Some(null))
      createFrom(schema) mustFail "InvalidDescription"

      schema = factory.createContainerSchema.copy(description = Some(""))
      createFrom(schema) mustFail "InvalidDescription"
    }

    it("be created with an empty centre id") {
      val schema = factory.createContainerSchema.copy(centreId = CentreId(""))
      createFrom(schema) mustFail "InvalidCentreId"
    }

    it("have more than one validation fail") {
      val schema = factory.createContainerSchema.copy(version = -2, name = "")
      createFrom(schema) mustFail ("InvalidVersion", "InvalidName")
    }

  }

}
