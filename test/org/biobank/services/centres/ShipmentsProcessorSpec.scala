package org.biobank.services.centres

import akka.actor._
import akka.pattern._
import javax.inject.{Inject, Named}
import org.biobank.Global
import org.biobank.domain.centres.ShipmentSpecFixtures
import org.biobank.domain.studies.StudyRepository
import org.biobank.domain.centres._
import org.biobank.domain.participants._
import org.biobank.fixtures._
import org.biobank.services._
import org.biobank.services.participants.SpecimensService
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito
import play.api.libs.json._
import scala.concurrent.duration._

final case class NamedShipmentsProcessor @Inject()(@Named("shipmentsProcessor") processor: ActorRef)

class ShipmentsProcessorSpec extends ProcessorTestFixture with ShipmentSpecFixtures {

  import org.biobank.TestUtils._
  import org.biobank.infrastructure.commands.ShipmentCommands._
  import org.biobank.infrastructure.events.ShipmentEvents._
  import org.scalatest.matchers.must.Matchers._

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  private var shipmentsProcessor = app.injector.instanceOf[NamedShipmentsProcessor].processor

  private val studyRepository = app.injector.instanceOf[StudyRepository]

  private val centreRepository = app.injector.instanceOf[CentreRepository]

  private val shipmentRepository = app.injector.instanceOf[ShipmentsWriteRepository]

  override def beforeEach() = {
    studyRepository.removeAll
    shipmentRepository.removeAll
    centreRepository.removeAll
    super.beforeEach()
  }

  override def centresFixture = {
    val f = super.centresFixture
    centreRepository.put(f.originCentre)
    centreRepository.put(f.destinationCentre)
    f
  }

  override def createdShipmentsFixture(numShipments: Int) = {
    val f = super.createdShipmentsFixture(numShipments)
    f.shipmentMap.values.foreach(shipmentRepository.put)
    f
  }

  private def restartProcessor(processor: ActorRef) = {
    gracefulStop(processor, 5 seconds, PoisonPill).map { _ =>
      val actor = system.actorOf(
        Props(
          new ShipmentsProcessor(app.injector.instanceOf[ShipmentsWriteRepository],
                                 app.injector.instanceOf[ShipmentSpecimensWriteRepository],
                                 centreRepository,
                                 app.injector.instanceOf[SpecimenRepository],
                                 app.injector.instanceOf[SpecimensService],
                                 app.injector.instanceOf[SnapshotWriter])
        ),
        "shipments-processor-id-2"
      )
      Thread.sleep(250)
      actor
    }
  }

  describe("A shipments processor must") {

    it("allow recovery from journal", PersistenceTest) {
      val f = createdShipmentFixture
      val cmd = AddShipmentCmd(sessionUserId = Global.DefaultUserId.id,
                               courierName           = f.shipment.courierName,
                               trackingNumber        = f.shipment.trackingNumber,
                               originLocationId      = f.shipment.originLocationId.id,
                               destinationLocationId = f.shipment.destinationLocationId.id)

      val v = ask(shipmentsProcessor, cmd).mapTo[ServiceValidation[ShipmentEvent]].futureValue
      v.isSuccess must be(true)
      shipmentRepository.getValues.map { s =>
        s.courierName
      } must contain(f.shipment.courierName)

      shipmentRepository.removeAll
      shipmentsProcessor = restartProcessor(shipmentsProcessor).futureValue

      shipmentRepository.getValues.size must be(1)
      shipmentRepository.getValues.map { s =>
        s.courierName
      } must contain(f.shipment.courierName)
    }

    it("recovers a snapshot", PersistenceTest) {
      val f                = createdShipmentsFixture(2)
      val snapshotFilename = "testfilename"
      val snapshotShipment = f.shipmentMap.values.toList(1)
      val snapshotState    = ShipmentsProcessor.SnapshotState(Set(snapshotShipment), Set.empty)

      Mockito.when(snapshotWriterMock.save(anyString, anyString)).thenReturn(snapshotFilename);
      Mockito
        .when(snapshotWriterMock.load(snapshotFilename))
        .thenReturn(Json.toJson(snapshotState).toString);
      f.shipmentMap.values.foreach(shipmentRepository.put)

      (shipmentsProcessor ? "snap").mapTo[String].futureValue
      shipmentRepository.removeAll
      shipmentsProcessor = restartProcessor(shipmentsProcessor).futureValue

      shipmentRepository.getValues.size must be(1)
      shipmentRepository.getByKey(snapshotShipment.id) mustSucceed { repoShipment =>
        repoShipment.courierName must be(snapshotShipment.courierName)
        ()
      }
    }

  }

}
