package org.biobank.domain.centres

import java.time.OffsetDateTime
import org.biobank.domain.{DomainSpec, DomainValidation, LocationId}
import org.biobank.fixtures.NameGenerator
import org.slf4j.LoggerFactory
import scalaz.Scalaz._
import scala.language.reflectiveCalls

class ShipmentSpec extends DomainSpec {

  import org.biobank.TestUtils._
  import org.biobank.matchers.EntityMatchers._

  val log = LoggerFactory.getLogger(this.getClass)

  val nameGenerator = new NameGenerator(this.getClass)

  def centresFixture = {
    val centres = (1 to 2).map { _ =>
      val location = factory.createLocation
      factory.createEnabledCentre.copy(locations = Set(location))
    }
    new {
      val originCentre      = centres(0)
      val destinationCentre = centres(1)
    }
  }

  def allShipmentsFixture = {
    val f = centresFixture
    new {
      val originCentre      = f.originCentre
      val destinationCentre = f.destinationCentre
      val shipments = Map[String, Shipment](
        "created"   -> factory.createShipment(originCentre, destinationCentre),
        "packed"    -> factory.createPackedShipment(originCentre, destinationCentre),
        "sent"      -> factory.createSentShipment(originCentre, destinationCentre),
        "received"  -> factory.createReceivedShipment(originCentre, destinationCentre),
        "unpacked"  -> factory.createUnpackedShipment(originCentre, destinationCentre),
        "completed" -> factory.createCompletedShipment(originCentre, destinationCentre),
        "lost"      -> factory.createLostShipment(originCentre, destinationCentre)
      )
    }
  }

  def createFrom(shipment: Shipment): DomainValidation[CreatedShipment] =
    CreatedShipment.create(id                    = shipment.id,
                           version               = shipment.version,
                           timeAdded             = shipment.timeAdded,
                           courierName           = shipment.courierName,
                           trackingNumber        = shipment.trackingNumber,
                           originCentreId        = shipment.originCentreId,
                           originLocationId      = shipment.originLocationId,
                           destinationCentreId   = shipment.destinationCentreId,
                           destinationLocationId = shipment.destinationLocationId)

  describe("A shipment") {

    describe("can be created") {

      it("when valid arguments are used") {
        val shipment = factory.createShipment.copy(version = 0L)
        createFrom(shipment).mustSucceed {
          _ must matchShipment(shipment)
        }
      }

    }

    describe("can change state") {

      it("to packed") {
        val shipment       = factory.createShipment
        val timePacked     = OffsetDateTime.now.minusDays(10)
        val packedShipment = shipment.pack(timePacked)

        packedShipment mustBe a[PackedShipment]
        packedShipment.state must be(Shipment.packedState)
        packedShipment.timePacked must be(Some(timePacked))
        packedShipment.version must be(shipment.version + 1)

        packedShipment must beEntityWithTimeStamps(shipment.timeAdded, Some(OffsetDateTime.now), 5L)
      }

      it("to sent") {
        val f        = centresFixture
        val shipment = factory.createPackedShipment(f.originCentre, f.destinationCentre)
        val timeSent = shipment.timePacked.get.plusDays(1)
        shipment.send(timeSent) mustSucceed { s =>
          s mustBe a[SentShipment]
          s.state must be(Shipment.sentState)
          s.timeSent must be(Some(timeSent))
          s.version must be(shipment.version + 1)

          s must beEntityWithTimeStamps(shipment.timeAdded, Some(OffsetDateTime.now), 5L)
        }
      }

      it("to received") {
        val f            = centresFixture
        val shipment     = factory.createSentShipment(f.originCentre, f.destinationCentre)
        val timeReceived = shipment.timeSent.get.plusDays(1)
        shipment.receive(timeReceived) mustSucceed { s =>
          s mustBe a[ReceivedShipment]
          s.state must be(Shipment.receivedState)
          s.timeReceived must be(Some(timeReceived))
          s.version must be(shipment.version + 1)

          s must beEntityWithTimeStamps(shipment.timeAdded, Some(OffsetDateTime.now), 5L)
        }
      }

      it("to unpacked") {
        val f            = centresFixture
        val shipment     = factory.createReceivedShipment(f.originCentre, f.destinationCentre)
        val timeUnpacked = shipment.timeReceived.get.plusDays(1)
        shipment.unpack(timeUnpacked) mustSucceed { s =>
          s mustBe a[UnpackedShipment]
          s.state must be(Shipment.unpackedState)
          s.timeUnpacked must be(Some(timeUnpacked))
          s.version must be(shipment.version + 1)

          s must beEntityWithTimeStamps(shipment.timeAdded, Some(OffsetDateTime.now), 5L)
        }
      }

      it("to completed") {
        val f             = centresFixture
        val shipment      = factory.createUnpackedShipment(f.originCentre, f.destinationCentre)
        val timeCompleted = shipment.timeUnpacked.get.plusDays(1)
        shipment.complete(timeCompleted) mustSucceed { s =>
          s mustBe a[CompletedShipment]
          s.state must be(Shipment.completedState)
          s.timeCompleted must be(Some(timeCompleted))
          s.version must be(shipment.version + 1)

          s must beEntityWithTimeStamps(shipment.timeAdded, Some(OffsetDateTime.now), 5L)
        }
      }

      it("to lost") {
        val f            = centresFixture
        val shipment     = factory.createSentShipment(f.originCentre, f.destinationCentre)
        val lostShipment = shipment.lost

        lostShipment mustBe a[LostShipment]
        lostShipment.state must be(Shipment.lostState)
        lostShipment.version must be(shipment.version + 1)

        lostShipment must beEntityWithTimeStamps(shipment.timeAdded, Some(OffsetDateTime.now), 5L)
      }

    }

    describe("can go to previous state") {

      it("from packed to created") {
        val f               = centresFixture
        val shipment        = factory.createPackedShipment(f.originCentre, f.destinationCentre)
        val createdShipment = shipment.created
        createdShipment mustBe a[CreatedShipment]
      }

      it("from sent to packed") {
        val f              = centresFixture
        val shipment       = factory.createSentShipment(f.originCentre, f.destinationCentre)
        val packedShipment = shipment.backToPacked
        packedShipment mustBe a[PackedShipment]
      }

      it("from received to sent") {
        val f            = centresFixture
        val shipment     = factory.createReceivedShipment(f.originCentre, f.destinationCentre)
        val sentShipment = shipment.backToSent
        sentShipment mustBe a[SentShipment]
      }

      it("from unpacked to received") {
        val f                = centresFixture
        val shipment         = factory.createUnpackedShipment(f.originCentre, f.destinationCentre)
        val receivedShipment = shipment.backToReceived
        receivedShipment mustBe a[ReceivedShipment]
      }

      it("from completed to unpacked") {
        val f                = centresFixture
        val shipment         = factory.createCompletedShipment(f.originCentre, f.destinationCentre)
        val unpackedShipment = shipment.backToUnpacked
        unpackedShipment mustBe a[UnpackedShipment]
      }

      it("from lost to sent") {
        val f            = centresFixture
        val shipment     = factory.createLostShipment(f.originCentre, f.destinationCentre)
        val sentShipment = shipment.backToSent
        sentShipment mustBe a[SentShipment]
      }

    }

    describe("can skip state") {

      it("from created to sent") {
        val shipment   = factory.createShipment
        val timePacked = OffsetDateTime.now.minusDays(10)
        val timeSent   = timePacked.plusDays(1)
        shipment.skipToSent(timePacked, timeSent) mustSucceed { s =>
          s mustBe a[SentShipment]
          s.timePacked must be(Some(timePacked))
          s.timeSent must be(Some(timeSent))
          s.version must be(shipment.version + 1)

          s must beEntityWithTimeStamps(shipment.timeAdded, Some(OffsetDateTime.now), 5L)
        }
      }

      it("from sent to unpacked") {
        val f        = centresFixture
        val shipment = factory.createSentShipment(f.originCentre, f.destinationCentre)
        val timeReceived = shipment.timeSent.fold { OffsetDateTime.now } { t =>
          t
        }
        val timeUnpacked = timeReceived.plusDays(1)
        shipment.skipToUnpacked(timeReceived, timeUnpacked) mustSucceed { s =>
          s mustBe a[UnpackedShipment]
          s.timeReceived must be(Some(timeReceived))
          s.timeUnpacked must be(Some(timeUnpacked))
          s.version must be(shipment.version + 1)

          s must beEntityWithTimeStamps(shipment.timeAdded, Some(OffsetDateTime.now), 5L)
        }
      }

    }

    describe("must not skip state") {

      it("when time sent is before time packed") {
        val shipment   = factory.createShipment
        val timePacked = OffsetDateTime.now.minusDays(10)
        val timeSent   = timePacked.minusDays(1)
        shipment.skipToSent(timePacked, timeSent) mustFailContains "TimeSentBeforePacked"
      }

      it("when time unpacked is before time received") {
        val f        = centresFixture
        val shipment = factory.createSentShipment(f.originCentre, f.destinationCentre)
        val timeReceived = shipment.timeSent.fold { OffsetDateTime.now } { t =>
          t
        }
        val timeUnpacked = timeReceived.minusDays(1)
        shipment.skipToUnpacked(timeReceived, timeUnpacked) mustFailContains "TimeUnpackedBeforeReceived"
      }

      it("when time received is before time sent") {
        val f        = centresFixture
        val shipment = factory.createSentShipment(f.originCentre, f.destinationCentre)
        val timeReceived = shipment.timeSent.fold { OffsetDateTime.now } { t =>
          t.minusDays(1)
        }
        val timeUnpacked = timeReceived.plusDays(2)
        shipment.skipToUnpacked(timeReceived, timeUnpacked) mustFailContains "TimeReceivedBeforeSent"
      }

    }

    describe("cannot be created") {

      it("with an invalid ID") {
        val shipment = factory.createShipment.copy(id = ShipmentId(""))
        createFrom(shipment) mustFail "IdRequired"
      }

      it("with an invalid version") {
        val shipment = factory.createShipment.copy(version = -2L)
        createFrom(shipment) mustFail "InvalidVersion"
      }

      it("with an invalid courier name") {
        val shipment = factory.createShipment.copy(courierName = "")
        createFrom(shipment) mustFail "CourierNameInvalid"
      }

      it("with an invalid tracking number") {
        val shipment = factory.createShipment.copy(trackingNumber = "")
        createFrom(shipment) mustFail "TrackingNumberInvalid"
      }

      it("with an invalid origin location") {
        val shipment = factory.createShipment.copy(originLocationId = LocationId(""))
        createFrom(shipment) mustFail "OriginLocationIdInvalid"
      }

      it("with an invalid destination location") {
        val shipment = factory.createShipment.copy(destinationLocationId = LocationId(""))
        createFrom(shipment) mustFail "DestinationLocationIdInvalid"
      }

    }

  }
}
