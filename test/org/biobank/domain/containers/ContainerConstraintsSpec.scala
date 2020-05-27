package org.biobank.domain.containers

import org.biobank.domain.DomainSpec
import org.biobank.fixtures.NameGenerator
import org.biobank.domain.DomainValidation
import org.biobank.domain.AnatomicalSourceType
import org.biobank.domain.PreservationType
import org.biobank.domain.SpecimenType

class ContainerConstraintsSpec extends DomainSpec {
  import org.biobank.TestUtils._
  import org.scalatest.matchers.must.Matchers._

  val nameGenerator = new NameGenerator(this.getClass)

  def createFrom(constraints: ContainerConstraints): DomainValidation[ContainerConstraints] =
    ContainerConstraints.create(name              = constraints.name,
                                description       = constraints.description,
                                anatomicalSources = constraints.anatomicalSources,
                                preservationTypes = constraints.preservationTypes,
                                specimenTypes     = constraints.specimenTypes)

  describe("A Container Constraints can") {

    import org.biobank.matchers.EntityMatchers._

    it("be created") {
      val constraints =
        factory.createContainerConstraints.copy(anatomicalSources = Set(AnatomicalSourceType.Blood),
                                                preservationTypes = Set(PreservationType.FreshSpecimen),
                                                specimenTypes     = Set(SpecimenType.BuffyCoat))
      createFrom(constraints) mustSucceed {
        _ must matchContainerConstraints(constraints)
      }
    }

  }

  describe("A Container Constraints can not") {

    it("be created with an empty name") {
      val constraints = factory.createContainerConstraints.copy(name = "")
      createFrom(constraints) mustFail "InvalidName"
    }

    it("be created with an invalid description") {
      createFrom(factory.createContainerConstraints.copy(description = Some(""))) mustFail "InvalidDescription"

      createFrom(factory.createContainerConstraints.copy(description = Some(null))) mustFail "InvalidDescription"
    }

  }

}
