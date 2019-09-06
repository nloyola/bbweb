package org.biobank.domain.containers

import org.biobank.domain.DomainSpec
import org.biobank.fixtures.NameGenerator
import org.slf4j.LoggerFactory
import org.biobank.domain.DomainValidation

class ContainerSchemaLabelSpec extends DomainSpec {
  import org.biobank.TestUtils._
  import org.biobank.matchers.EntityMatchers._

  val log = LoggerFactory.getLogger(this.getClass)

  val nameGenerator = new NameGenerator(this.getClass)

  def createFrom(schemaLabel: ContainerSchemaLabel): DomainValidation[ContainerSchemaLabel] =
    ContainerSchemaLabel.create(schemaId = schemaLabel.schemaId, label = schemaLabel.label)

  describe("A container position schema") {

    it("can be created") {
      val schemaLabel = factory.createContainerSchemaLabel()
      createFrom(schemaLabel) mustSucceed { pos =>
        pos mustBe a[ContainerSchemaLabel]
        pos must matchContainerSchemaLabel(schemaLabel)
      }
    }

  }

  describe("A container schema can") {

    it("not be created with an empty schema id") {
      val schemaLabel = factory.createContainerSchemaLabel().copy(schemaId = ContainerSchemaId(""))
      createFrom(schemaLabel) mustFail "InvalidContainerSchemaId"
    }

    it("not be created with an empty label") {
      val schemaLabel = factory.createContainerSchemaLabel().copy(label = "")
      createFrom(schemaLabel) mustFail "ContainerSchemaLabelInvalid"
    }

  }

}
