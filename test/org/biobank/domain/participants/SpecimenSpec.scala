package org.biobank.domain.participants

import java.time.OffsetDateTime
import org.biobank.fixtures.NameGenerator
import org.biobank.domain._
import org.biobank.domain.containers.{ContainerId, ContainerSchemaId}
import org.biobank.domain.studies.SpecimenDefinitionId
import org.slf4j.LoggerFactory

class SpecimenSpec extends DomainSpec {
  import org.biobank.TestUtils._
  import org.biobank.matchers.EntityMatchers._

  val log = LoggerFactory.getLogger(this.getClass)

  val nameGenerator = new NameGenerator(this.getClass)

  def createFrom(specimen: Specimen): DomainValidation[Specimen] =
    UsableSpecimen.create(id                   = specimen.id,
                          inventoryId          = specimen.inventoryId,
                          specimenDefinitionId = specimen.specimenDefinitionId,
                          version              = specimen.version,
                          timeAdded            = OffsetDateTime.now,
                          timeCreated          = specimen.timeCreated,
                          originLocationId     = specimen.originLocationId,
                          locationId           = specimen.locationId,
                          containerId          = specimen.containerId,
                          schemaLabel          = specimen.schemaLabel,
                          amount               = specimen.amount)

  describe("A usable specimen") {

    describe("can be created") {

      it("when valid arguments are used") {
        val specimen = factory.createUsableSpecimen.copy(version = 0L)
        createFrom(specimen) mustSucceed {
          _ must matchSpecimen(specimen)
        }
      }

    }

    describe("can be updated") {

      it("with a new inventory ID") {
        val specimen       = factory.createUsableSpecimen
        val newInventoryId = nameGenerator.next[Specimen]

        specimen.withInventoryId(newInventoryId) mustSucceed { s =>
          s.inventoryId must be(newInventoryId)
          s.version must be(specimen.version + 1)

          s must beEntityWithTimeStamps(specimen.timeAdded, Some(OffsetDateTime.now), 5L)
        }
      }

      it("with a new amount") {
        val specimen  = factory.createUsableSpecimen
        val newAmount = specimen.amount + 1

        specimen.withAmount(newAmount) mustSucceed { s =>
          s.amount must be(newAmount)
          s.version must be(specimen.version + 1)
          s must beEntityWithTimeStamps(specimen.timeAdded, Some(OffsetDateTime.now), 5L)
        }
      }

      it("with a new origin location") {
        val specimen    = factory.createUsableSpecimen
        val newLocation = factory.createLocation

        specimen.withOrigin(newLocation.id) mustSucceed { s =>
          s.originLocationId must be(newLocation.id)
          s.version must be(specimen.version + 1)
          s must beEntityWithTimeStamps(specimen.timeAdded, Some(OffsetDateTime.now), 5L)
        }
      }

      it("with a new location") {
        val specimen    = factory.createUsableSpecimen
        val newLocation = factory.createLocation

        specimen.withLocation(newLocation.id) mustSucceed { s =>
          s.locationId must be(newLocation.id)
          s.version must be(specimen.version + 1)
          s must beEntityWithTimeStamps(specimen.timeAdded, Some(OffsetDateTime.now), 5L)
        }
      }

      it("with a new container label") {
        val specimen = factory.createUsableSpecimen
        val newLabel = Some(factory.createContainerSchemaLabel)

        specimen.withContainerLabel(newLabel) mustSucceed { s =>
          s.schemaLabel mustBe newLabel
          s.version must be(specimen.version + 1)
          s must beEntityWithTimeStamps(specimen.timeAdded, Some(OffsetDateTime.now), 5L)
        }
      }
    }

    describe("can be made unusable") {

      it("from a usable specimen") {
        val specimen = factory.createUsableSpecimen

        specimen.makeUnusable mustSucceed { s =>
          s mustBe a[UnusableSpecimen]
          s.version must be(specimen.version + 1)
          s must beEntityWithTimeStamps(specimen.timeAdded, Some(OffsetDateTime.now), 5L)
        }
      }

    }

    describe("cannot be created") {

      it("with an empty id") {
        val specimen = factory.createUsableSpecimen.copy(id = SpecimenId(""))
        createFrom(specimen) mustFail "IdRequired"
      }

      it("with an empty inventory id") {
        val specimen = factory.createUsableSpecimen.copy(inventoryId = "")
        createFrom(specimen) mustFail "InventoryIdInvalid"
      }

      it("with an empty specimen spec id") {
        val specimen = factory.createUsableSpecimen.copy(specimenDefinitionId = SpecimenDefinitionId(""))
        createFrom(specimen) mustFail "SpecimenDefinitionIdInvalid"
      }

      it("with an invalid version number") {
        val specimen = factory.createUsableSpecimen.copy(version = -2)
        createFrom(specimen) mustFail "InvalidVersion"
      }

      it("with an empty origin location id") {
        val specimen = factory.createUsableSpecimen.copy(originLocationId = LocationId(""))
        createFrom(specimen) mustFail "OriginLocationIdInvalid"
      }

      it("with an empty location id") {
        val specimen = factory.createUsableSpecimen.copy(locationId = LocationId(""))
        createFrom(specimen) mustFail "LocationIdInvalid"
      }

      it("with an empty container id") {
        val specimen = factory.createUsableSpecimen.copy(containerId = Some(ContainerId("")))
        createFrom(specimen) mustFail "ContainerIdInvalid"
      }

      it("with an empty invalid schema label id") {
        val schemaLabel =
          Some(factory.createContainerSchemaLabel.copy(schemaId = ContainerSchemaId("")))
        val specimen = factory.createUsableSpecimen.copy(schemaLabel = schemaLabel)
        createFrom(specimen) mustFail "InvalidContainerSchemaId"
      }

      it("with a negative amount") {
        val specimen = factory.createUsableSpecimen.copy(amount = BigDecimal(-1))
        createFrom(specimen) mustFail "AmountInvalid"
      }

    }

  }

  describe("cannot be updated") {

    it("with an invalid inventory ID") {
      val specimen = factory.createUsableSpecimen
      specimen.withInventoryId("") mustFail "InventoryIdInvalid"
    }

    it("with an invalid amount") {
      val specimen = factory.createUsableSpecimen
      specimen.withAmount(BigDecimal("-1")) mustFail "AmountInvalid"
    }

    it("with an invalid origin location") {
      val specimen    = factory.createUsableSpecimen
      val newLocation = factory.createLocation.copy(id = LocationId(""))
      specimen.withOrigin(newLocation.id) mustFail "LocationIdInvalid"
    }

    it("with an invalid location") {
      val specimen    = factory.createUsableSpecimen
      val newLocation = factory.createLocation.copy(id = LocationId(""))
      specimen.withLocation(newLocation.id) mustFail "LocationIdInvalid"
    }

    it("with an invalid schema label") {
      val specimen = factory.createUsableSpecimen
      val newLabel = factory.createContainerSchemaLabel.copy(schemaId = ContainerSchemaId(""))

      specimen.withContainerLabel(Some(newLabel)) mustFail "InvalidContainerSchemaId"
    }
  }

  describe("can be made unusable") {

    it("from a usable specimen") {
      val specimen = factory.createUsableSpecimen

      specimen.makeUnusable mustSucceed { s =>
        s mustBe a[UnusableSpecimen]
        s.version must be(specimen.version + 1)
        s must beEntityWithTimeStamps(specimen.timeAdded, Some(OffsetDateTime.now), 5L)
      }
    }

  }

  describe("A usable specimen can be made usable") {

    it("from a unusable specimen") {
      val specimen = factory.createUnusableSpecimen

      specimen.makeUsable mustSucceed { s =>
        s mustBe a[UsableSpecimen]
        s.version must be(specimen.version + 1)
        s must beEntityWithTimeStamps(specimen.timeAdded, Some(OffsetDateTime.now), 5L)
      }
    }

  }

  describe("Specimens can be compared") {

    it("by specimen ID") {
      val (specimen1, specimen2) = (factory.createUsableSpecimen.copy(id = SpecimenId("A")),
                                    factory.createUsableSpecimen.copy(id = SpecimenId("B")))
      Specimen.compareById(specimen1, specimen2) mustBe true
      Specimen.compareById(specimen2, specimen1) mustBe false
    }

    it("by inventory ID") {
      val (specimen1, specimen2) = (factory.createUsableSpecimen.copy(inventoryId = "A"),
                                    factory.createUsableSpecimen.copy(inventoryId = "B"))
      Specimen.compareByInventoryId(specimen1, specimen2) mustBe true
      Specimen.compareByInventoryId(specimen2, specimen1) mustBe false
    }

    it("by time created") {
      val (specimen1, specimen2) =
        (factory.createUsableSpecimen.copy(timeCreated = OffsetDateTime.now.minusDays(1)),
         factory.createUsableSpecimen.copy(timeCreated = OffsetDateTime.now))
      Specimen.compareByTimeCreated(specimen1, specimen2) mustBe true
      Specimen.compareByTimeCreated(specimen2, specimen1) mustBe false
    }

    it("by state") {
      val (specimen1, specimen2) = (factory.createUnusableSpecimen, factory.createUsableSpecimen)
      Specimen.compareByState(specimen1, specimen2) mustBe true
      Specimen.compareByState(specimen2, specimen1) mustBe false
    }
  }
}
