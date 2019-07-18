package org.biobank.domain.containers

import org.biobank.domain.DomainSpec
import org.biobank.domain.centres.CentreId
import org.biobank.fixtures.NameGenerator
import org.biobank.domain.DomainValidation
import org.biobank.domain.AnatomicalSourceType
import org.biobank.domain.PreservationType
import org.biobank.domain.SpecimenType

class ContainerConstraintsSpec extends DomainSpec {
  import org.biobank.TestUtils._

  val nameGenerator = new NameGenerator(this.getClass)

  def createFrom(containerConstraints: ContainerConstraints): DomainValidation[ContainerConstraints] =
    ContainerConstraints.create(id                    = containerConstraints.id,
                                name                  = containerConstraints.name,
                                description           = containerConstraints.description,
                                centreId              = containerConstraints.centreId,
                                anatomicalSourceTypes = containerConstraints.anatomicalSourceTypes,
                                preservationTypes     = containerConstraints.preservationTypes,
                                specimenTypes         = containerConstraints.specimenTypes)

  describe("A Container Constraints can") {

    import org.biobank.matchers.EntityMatchers._

    it("be created") {
      val constraints = factory.createContainerConstraints.copy(
          anatomicalSourceTypes = Set(AnatomicalSourceType.Blood),
          preservationTypes = Set(PreservationType.FreshSpecimen),
          specimenTypes = Set(SpecimenType.BuffyCoat)
        )
      createFrom(constraints) mustSucceed {
        _ must matchContainerConstraints(constraints)
      }
    }

    it("can have it's name updated") {
      val constraints = factory.createContainerConstraints
      val newName = nameGenerator.next[ContainerConstraints]
      constraints.withName(newName) mustSucceed {
        _ must matchContainerConstraints(constraints.copy(name = newName))
      }
    }

    it("can have it's description updated") {
      val constraints = factory.createContainerConstraints
      val newDescription = Some(nameGenerator.next[ContainerConstraints])
      constraints.withDescription(newDescription) mustSucceed {
        _ must matchContainerConstraints(constraints.copy(description = newDescription))
      }
    }

    it("can have it's centre updated") {
      val constraints = factory.createContainerConstraints
      val centre = factory.createDisabledCentre
      constraints.withCentre(centre.id) mustSucceed {
        _ must matchContainerConstraints(constraints.copy(centreId = centre.id))
      }
    }

    it("can have the anatomical sources updated") {
      val constraints = factory.createContainerConstraints
      val sources = Set(AnatomicalSourceType.Colon)
      constraints.withAnatomicalSourceTypes(sources) mustSucceed {
        _ must matchContainerConstraints(constraints.copy(anatomicalSourceTypes = sources))
      }
    }

    it("can have the preservation types updated") {
      val constraints = factory.createContainerConstraints
      val sources = Set(PreservationType.FrozenSpecimen)
      constraints.withPreservationTypes(sources) mustSucceed {
        _ must matchContainerConstraints(constraints.copy(preservationTypes = sources))
      }
    }

    it("can have the specimen types updated") {
      val constraints = factory.createContainerConstraints
      val sources = Set(SpecimenType.DescendingColon)
      constraints.withSpecimenTypes(sources) mustSucceed {
        _ must matchContainerConstraints(constraints.copy(specimenTypes = sources))
      }
    }

  }

  describe("A Container Constraints can not") {

    it("be created with an empty id") {
      val constraints = factory.createContainerConstraints.copy(id = ContainerConstraintsId(""))
      createFrom(constraints) mustFail "IdRequired"
    }

    it("be created with an invalid description") {
      createFrom(
        factory.createContainerConstraints.copy(description = Some(""))
      ) mustFail "InvalidDescription"

      createFrom(
        factory.createContainerConstraints.copy(description = Some(null))
      ) mustFail "InvalidDescription"
    }

    it("be created with an empty centre id") {
      val constraints = factory.createContainerConstraints.copy(centreId = CentreId(""))
      createFrom(constraints) mustFail "CentreIdRequired"
    }

  }

}
