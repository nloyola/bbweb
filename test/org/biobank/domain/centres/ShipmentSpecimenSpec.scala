package org.biobank.domain.centres

import org.biobank.domain.DomainSpec
import org.biobank.domain.participants.SpecimenId
import org.biobank.fixtures.NameGenerator
import org.biobank.validation.Validation._
import org.slf4j.LoggerFactory

class ShipmentSpecimenSpec extends DomainSpec {

  import org.biobank.TestUtils._
  import org.biobank.matchers.EntityMatchers._
  import org.scalatest.matchers.must.Matchers._

  val log = LoggerFactory.getLogger(this.getClass)

  val nameGenerator = new NameGenerator(this.getClass)

  def createFrom(shipmentSpecimen: ShipmentSpecimen): ValidationResult[ShipmentSpecimen] =
    ShipmentSpecimen.create(id                  = shipmentSpecimen.id,
                            version             = shipmentSpecimen.version,
                            shipmentId          = shipmentSpecimen.shipmentId,
                            specimenId          = shipmentSpecimen.specimenId,
                            state               = shipmentSpecimen.state,
                            shipmentContainerId = shipmentSpecimen.shipmentContainerId)

  describe("A shipment specimen") {

    describe("can be created") {

      it("when valid arguments are used") {
        val shipmentSpecimen = factory.createShipmentSpecimen.copy(version = 0L)
        createFrom(shipmentSpecimen).mustSucceed {
          _ must matchShipmentSpecimen(shipmentSpecimen)
        }
      }

    }

    describe("cannot be created") {

      it("with an invalid ID") {
        val shipmentSpecimen = factory.createShipmentSpecimen.copy(id = ShipmentSpecimenId(""))
        createFrom(shipmentSpecimen) mustFail "IdEmpty"
      }

      it("with an invalid version") {
        val shipmentSpecimen = factory.createShipmentSpecimen.copy(version = -2L)
        createFrom(shipmentSpecimen) mustFail "InvalidVersion"
      }

      it("with an invalid shipment ID") {
        val shipmentSpecimen = factory.createShipmentSpecimen.copy(shipmentId = ShipmentId(""))
        createFrom(shipmentSpecimen) mustFail "ShipmentIdRequired"
      }

      it("with an invalid specimen ID") {
        val shipmentSpecimen = factory.createShipmentSpecimen.copy(specimenId = SpecimenId(""))
        createFrom(shipmentSpecimen) mustFail "SpecimenIdRequired"
      }

      it("with an invalid shipment container ID") {
        val shipmentSpecimen =
          factory.createShipmentSpecimen.copy(shipmentContainerId = Some(ShipmentContainerId("")))
        createFrom(shipmentSpecimen) mustFail "ShipmentContainerIdEmpty"
      }

    }

  }

}
