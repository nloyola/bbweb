package org.biobank.domain.containers

import org.biobank.domain.DomainSpec
import org.biobank.fixtures.NameGenerator
import org.slf4j.LoggerFactory
import org.biobank.domain.DomainValidation

class ContainerSchemaPositionSpec extends DomainSpec {
  import org.biobank.TestUtils._
  import org.biobank.matchers.EntityMatchers._

  val log = LoggerFactory.getLogger(this.getClass)

  val nameGenerator = new NameGenerator(this.getClass)

  def createFrom(schemaPosition: ContainerSchemaPosition): DomainValidation[ContainerSchemaPosition] =
    ContainerSchemaPosition
      .create(id = schemaPosition.id, schemaId = schemaPosition.schemaId, label = schemaPosition.label)

  describe("A container position schema") {

    it("can be created") {
      val schemaPosition = factory.createContainerSchemaPosition()
      createFrom(schemaPosition) mustSucceed { pos =>
        pos mustBe a[ContainerSchemaPosition]
        pos must matchContainerSchemaPosition(schemaPosition)
      }
    }

  }

  describe("A container schema can") {

    it("not be created with an empty id") {
      val schemaPosition = factory.createContainerSchemaPosition().copy(id = ContainerSchemaPositionId(""))
      createFrom(schemaPosition) mustFail "IdRequired"
    }

    it("not be created with an empty schema id") {
      val schemaPosition = factory.createContainerSchemaPosition().copy(schemaId = ContainerSchemaId(""))
      createFrom(schemaPosition) mustFail "InvalidContainerSchemaId"
    }

    it("not be created with an empty label") {
      val schemaPosition = factory.createContainerSchemaPosition().copy(label = "")
      createFrom(schemaPosition) mustFail "InvalidLabel"
    }

  }

}
