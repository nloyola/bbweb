package org.biobank.query.centres

import akka.actor._
import akka.testkit.TestKit
import akka.pattern._
import akka.util.Timeout
import cats.data._
import cats.scalatest.ValidatedValues._
import java.time.OffsetDateTime
import org.biobank.controllers.CacheForTesting
import org.biobank.fixtures._
import org.biobank.domain.Factory
import org.biobank.domain.centres._
import org.biobank.dto.centres._
import org.biobank.services.centres.NamedShipmentsProcessor
import org.biobank.validation.Validation._
import org.mockito.ArgumentCaptor
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatestplus.mockito.MockitoSugar
import play.api.cache.{AsyncCacheApi, DefaultSyncCacheApi, SyncCacheApi}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import scala.concurrent.Future
import scala.concurrent.duration._

class ShipmentsQuerySpec
    extends AnyFunSpec with BeforeAndAfterEach with ScalaFutures with BeforeAndAfterAll with MockitoSugar
    with ShipmentSpecFixtures {

  import org.mockito.Mockito._
  import org.biobank.Global._
  import org.biobank.infrastructure.commands.ShipmentCommands._
  import org.scalatest.matchers.must.Matchers._
  import org.biobank.TestUtils._
  import org.biobank.matchers.DtoMatchers._
  import org.biobank.matchers.EntityMatchers._

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  private val shipmentsRepositoryMock = mock[ShipmentsReadRepository]

  private val app = new GuiceApplicationBuilder()
    .overrides(bind[SyncCacheApi].to[DefaultSyncCacheApi])
    .overrides(bind[AsyncCacheApi].to[CacheForTesting])
    .overrides(bind[ShipmentsReadRepository].toInstance(shipmentsRepositoryMock))
    .build

  implicit val system: ActorSystem = app.injector.instanceOf[ActorSystem]

  implicit val timeout: Timeout = 5.seconds

  implicit val myDefaultPatience =
    PatienceConfig(timeout = Span(5, Seconds), interval = Span(100, Millis))

  protected val nameGenerator = new NameGenerator(this.getClass)

  protected val factory = new Factory

  private val centreRepository = app.injector.instanceOf[CentreRepository]

  private val shipmentsProcessor = app.injector.instanceOf[NamedShipmentsProcessor].processor

  private val shipmentsWriteRepository = app.injector.instanceOf[ShipmentsWriteRepository]

  protected val shipmentSpecimensWriteRepository = app.injector.instanceOf[ShipmentSpecimensWriteRepository]

  override def beforeEach() = {
    centreRepository.removeAll
  }

  /**
   * Shuts down the actor system.
   */
  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  override protected def centresFixture = {
    val f = super.centresFixture
    centreRepository.put(f.originCentre)
    centreRepository.put(f.destinationCentre)
    f
  }

  describe("ShipmentsQuerySpec") {

    it("must handle shipment added event") {
      val f = createdShipmentFixture
      reset(shipmentsRepositoryMock)

      val cmd = AddShipmentCmd(sessionUserId = DefaultUserId.id,
                               courierName           = f.shipment.courierName,
                               trackingNumber        = f.shipment.trackingNumber,
                               originLocationId      = f.shipment.originLocationId.id,
                               destinationLocationId = f.shipment.destinationLocationId.id)

      val r = ask(shipmentsProcessor, cmd).mapTo[ValidationResult[Shipment]].futureValue.value

      Thread.sleep(200)
      val argument = ArgumentCaptor.forClass(classOf[ShipmentDto])
      verify(shipmentsRepositoryMock).put(argument.capture)

      // all but ID should match
      val expectedShipment = f.shipment.copy(id = argument.getValue.id)
      r must matchShipment(expectedShipment)
      argument.getValue must matchDtoToShipment(expectedShipment)
    }

    describe("for the courier name updated event") {
      updateTest { () =>
        val f = createdShipmentFixture
        shipmentsWriteRepository.put(f.shipment)

        val newCourier = nameGenerator.next[Shipment]
        val cmd = UpdateShipmentCourierNameCmd(sessionUserId = DefaultUserId.id,
                                               id              = f.shipment.id.id,
                                               expectedVersion = f.shipment.version,
                                               courierName     = newCourier)
        val expectedShipment = f.shipment.withCourier(newCourier, OffsetDateTime.now).value
        (f.shipmentDto, expectedShipment, cmd)
      }
    }

    describe("for the tracking number updated event") {
      updateTest { () =>
        val f = createdShipmentFixture
        shipmentsWriteRepository.put(f.shipment)

        val trackingNo = nameGenerator.next[Shipment]
        val cmd = UpdateShipmentTrackingNumberCmd(sessionUserId = DefaultUserId.id,
                                                  id              = f.shipment.id.id,
                                                  expectedVersion = f.shipment.version,
                                                  trackingNumber  = trackingNo)
        val expectedShipment = f.shipment.withTrackingNumber(trackingNo, OffsetDateTime.now).value
        (f.shipmentDto, expectedShipment, cmd)
      }
    }

    describe("for the origin updated event") {
      updateTest { () =>
        val f = createdShipmentFixture
        shipmentsWriteRepository.put(f.shipment)

        val newLocation = factory.createLocation
        val newCentre   = factory.createEnabledCentre.copy(locations = Set(newLocation))
        centreRepository.put(newCentre)

        val cmd = UpdateShipmentOriginCmd(sessionUserId = DefaultUserId.id,
                                          id              = f.shipment.id.id,
                                          expectedVersion = f.shipment.version,
                                          locationId      = newLocation.id.id)
        val expectedShipment = f.shipment.withOrigin(newCentre.id, newLocation.id, OffsetDateTime.now).value
        (f.shipmentDto, expectedShipment, cmd)
      }
    }

    describe("for the destination updated event") {
      updateTest { () =>
        val f = createdShipmentFixture
        shipmentsWriteRepository.put(f.shipment)

        val newLocation = factory.createLocation
        val newCentre   = factory.createEnabledCentre.copy(locations = Set(newLocation))
        centreRepository.put(newCentre)

        val cmd = UpdateShipmentDestinationCmd(sessionUserId = DefaultUserId.id,
                                               id              = f.shipment.id.id,
                                               expectedVersion = f.shipment.version,
                                               locationId      = newLocation.id.id)
        val expectedShipment =
          f.shipment.withDestination(newCentre.id, newLocation.id, OffsetDateTime.now).value
        (f.shipmentDto, expectedShipment, cmd)
      }
    }

    describe("for the shipment CREATED state event") {
      updateTest { () =>
        val f                = packedShipmentFixture
        val expectedShipment = f.shipment.created(OffsetDateTime.now)
        shipmentsWriteRepository.put(f.shipment)

        val cmd = CreatedShipmentCmd(sessionUserId = DefaultUserId.id,
                                     id              = f.shipment.id.id,
                                     expectedVersion = f.shipment.version)
        (f.shipmentDto, expectedShipment, cmd)
      }
    }

    describe("for the shipment PACKED state event") {
      updateTest { () =>
        val f        = allShipmentsFixture
        val shipment = f.shipments(Shipment.createdState).asInstanceOf[CreatedShipment]
        val ss       = addSpecimenToShipment(shipment, f.originCentre)
        shipmentSpecimensWriteRepository.put(ss.shipmentSpecimen)

        val dto              = f.dtoFrom(shipment)
        val timePacked       = OffsetDateTime.now
        val expectedShipment = shipment.pack(timePacked, OffsetDateTime.now)
        shipmentsWriteRepository.put(shipment)

        val cmd = PackShipmentCmd(sessionUserId = DefaultUserId.id,
                                  id              = shipment.id.id,
                                  expectedVersion = shipment.version,
                                  datetime        = timePacked)
        (dto, expectedShipment, cmd)
      }
    }

    describe("for the shipment SENT state event") {
      updateTest { () =>
        val f        = allShipmentsFixture
        val shipment = f.shipments(Shipment.packedState).asInstanceOf[PackedShipment]
        val ss       = addSpecimenToShipment(shipment, f.originCentre)
        shipmentSpecimensWriteRepository.put(ss.shipmentSpecimen)

        val dto              = f.dtoFrom(shipment)
        val timeSent         = OffsetDateTime.now
        val expectedShipment = shipment.send(timeSent, OffsetDateTime.now).value
        shipmentsWriteRepository.put(shipment)

        val cmd = SendShipmentCmd(sessionUserId = DefaultUserId.id,
                                  id              = shipment.id.id,
                                  expectedVersion = shipment.version,
                                  datetime        = timeSent)
        (dto, expectedShipment, cmd)
      }
    }

    describe("for the shipment RECEIVED state event") {
      updateTest { () =>
        val f        = allShipmentsFixture
        val shipment = f.shipments(Shipment.sentState).asInstanceOf[SentShipment]
        val ss       = addSpecimenToShipment(shipment, f.originCentre)
        shipmentSpecimensWriteRepository.put(ss.shipmentSpecimen)

        val dto              = f.dtoFrom(shipment)
        val timeReceived     = OffsetDateTime.now
        val expectedShipment = shipment.receive(timeReceived, OffsetDateTime.now).value
        shipmentsWriteRepository.put(shipment)

        val cmd = ReceiveShipmentCmd(sessionUserId = DefaultUserId.id,
                                     id              = shipment.id.id,
                                     expectedVersion = shipment.version,
                                     datetime        = timeReceived)
        (dto, expectedShipment, cmd)
      }
    }

    describe("for the shipment UNPACKED state event") {
      updateTest { () =>
        val f        = allShipmentsFixture
        val shipment = f.shipments(Shipment.receivedState).asInstanceOf[ReceivedShipment]
        val ss       = addSpecimenToShipment(shipment, f.originCentre)
        shipmentSpecimensWriteRepository.put(ss.shipmentSpecimen)

        val dto              = f.dtoFrom(shipment)
        val timeUnpacked     = OffsetDateTime.now
        val expectedShipment = shipment.unpack(timeUnpacked, OffsetDateTime.now).value
        shipmentsWriteRepository.put(shipment)

        val cmd = UnpackShipmentCmd(sessionUserId = DefaultUserId.id,
                                    id              = shipment.id.id,
                                    expectedVersion = shipment.version,
                                    datetime        = timeUnpacked)
        (dto, expectedShipment, cmd)
      }
    }

    describe("for the shipment COMPLETED state event") {
      updateTest { () =>
        val f        = allShipmentsFixture
        val shipment = f.shipments(Shipment.unpackedState).asInstanceOf[UnpackedShipment]
        val ss       = addSpecimenToShipment(shipment, f.originCentre)
        shipmentSpecimensWriteRepository.put(ss.shipmentSpecimen.received(OffsetDateTime.now).value)

        val dto              = f.dtoFrom(shipment)
        val timeCompleted    = OffsetDateTime.now
        val expectedShipment = shipment.complete(timeCompleted, OffsetDateTime.now).value
        shipmentsWriteRepository.put(shipment)

        val cmd = CompleteShipmentCmd(sessionUserId = DefaultUserId.id,
                                      id              = shipment.id.id,
                                      expectedVersion = shipment.version,
                                      datetime        = timeCompleted)
        (dto, expectedShipment, cmd)
      }
    }

    describe("for the shipment LOST state event") {
      updateTest { () =>
        val f        = allShipmentsFixture
        val shipment = f.shipments(Shipment.sentState).asInstanceOf[SentShipment]
        val ss       = addSpecimenToShipment(shipment, f.originCentre)
        shipmentSpecimensWriteRepository.put(ss.shipmentSpecimen)

        val dto              = f.dtoFrom(shipment)
        val expectedShipment = shipment.lost(OffsetDateTime.now)
        shipmentsWriteRepository.put(shipment)

        val cmd = LostShipmentCmd(sessionUserId = DefaultUserId.id,
                                  id              = shipment.id.id,
                                  expectedVersion = shipment.version)
        (dto, expectedShipment, cmd)
      }
    }

    describe("for the shipment SKIP TO SENT state event") {
      updateTest { () =>
        val f        = allShipmentsFixture
        val shipment = f.shipments(Shipment.createdState).asInstanceOf[CreatedShipment]
        val ss       = addSpecimenToShipment(shipment, f.originCentre)
        shipmentSpecimensWriteRepository.put(ss.shipmentSpecimen)

        val dto              = f.dtoFrom(shipment)
        val timePacked       = OffsetDateTime.now
        val timeSent         = OffsetDateTime.now
        val expectedShipment = shipment.skipToSent(timePacked, timeSent, OffsetDateTime.now).value
        shipmentsWriteRepository.put(shipment)

        val cmd = ShipmentSkipStateToSentCmd(sessionUserId = DefaultUserId.id,
                                             id              = shipment.id.id,
                                             expectedVersion = shipment.version,
                                             timePacked      = timePacked,
                                             timeSent        = timeSent)
        (dto, expectedShipment, cmd)
      }
    }

    describe("for the shipment SKIP TO Unpacked state event") {
      updateTest { () =>
        val f        = allShipmentsFixture
        val shipment = f.shipments(Shipment.sentState).asInstanceOf[SentShipment]
        val ss       = addSpecimenToShipment(shipment, f.originCentre)
        shipmentSpecimensWriteRepository.put(ss.shipmentSpecimen)

        val dto              = f.dtoFrom(shipment)
        val timeReceived     = OffsetDateTime.now
        val timeUnpacked     = OffsetDateTime.now
        val expectedShipment = shipment.skipToUnpacked(timeReceived, timeUnpacked, OffsetDateTime.now).value
        shipmentsWriteRepository.put(shipment)

        val cmd = ShipmentSkipStateToUnpackedCmd(sessionUserId = DefaultUserId.id,
                                                 id              = shipment.id.id,
                                                 expectedVersion = shipment.version,
                                                 timeReceived    = timeReceived,
                                                 timeUnpacked    = timeUnpacked)
        (dto, expectedShipment, cmd)
      }
    }

    it("for the shipment removed event") {
      val f        = allShipmentsFixture
      val shipment = f.shipments(Shipment.createdState).asInstanceOf[CreatedShipment]
      val dto      = f.dtoFrom(shipment)

      shipmentsWriteRepository.put(shipment)

      val cmd = ShipmentRemoveCmd(sessionUserId = DefaultUserId.id,
                                  id              = shipment.id.id,
                                  expectedVersion = shipment.version)

      reset(shipmentsRepositoryMock)
      val r: Future[Either[NonEmptyChain[ValidationError], ShipmentDto]] = Future.successful(Right(dto))
      when(shipmentsRepositoryMock.getByKey(shipment.id)).thenReturn(EitherT(r))

      ask(shipmentsProcessor, cmd).mapTo[ValidationResult[Shipment]].futureValue mustSucceed {
        _ must matchShipment(shipment)
      }

      Thread.sleep(50)
      val argument = ArgumentCaptor.forClass(classOf[ShipmentId])
      verify(shipmentsRepositoryMock).remove(argument.capture)
      argument.getValue must be(shipment.id)
    }
  }

  def updateTest(setupFunc: () => (ShipmentDto, Shipment, ShipmentModifyCommand)) = {

    it("must store the shipment in the repository") {
      val (dto, expectedShipment, cmd) = setupFunc()

      reset(shipmentsRepositoryMock)
      val r: Future[Either[NonEmptyChain[ValidationError], ShipmentDto]] = Future.successful(Right(dto))
      when(shipmentsRepositoryMock.getByKey(expectedShipment.id)).thenReturn(EitherT(r))

      ask(shipmentsProcessor, cmd).mapTo[ValidationResult[Shipment]].futureValue mustSucceed {
        _ must matchShipment(expectedShipment)
      }

      Thread.sleep(50)
      val argument = ArgumentCaptor.forClass(classOf[ShipmentDto])
      verify(shipmentsRepositoryMock).put(argument.capture)
      argument.getValue must matchDtoToShipment(expectedShipment)
    }
  }
}
