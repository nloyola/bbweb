package org.biobank.query.centres

import akka.actor._
import akka.persistence.jdbc.query.scaladsl.JdbcReadJournal
import akka.persistence.query.{EventEnvelope, PersistenceQuery}
import akka.stream.{ActorMaterializer, Materializer}
import java.time.OffsetDateTime
import javax.inject.Inject
import org.biobank.TestData
import org.biobank.domain._
import org.biobank.domain.centres._
import org.biobank.infrastructure.events.ShipmentEvents._
import org.biobank.query.db.{DatabaseSchema, SequenceNumber, SequenceNumbersDao}
import org.slf4j.LoggerFactory
import play.api.db.slick.DatabaseConfigProvider
import scala.concurrent.{ExecutionContext, Future}
//import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._
import scalaz._
import scala.collection.mutable.Queue

trait DbOperation

final case class AddOrUpdate(shipment: Shipment) extends DbOperation

final case class Remove(shipmentId: ShipmentId) extends DbOperation

@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter", "org.wartremover.warts.NonUnitStatements"))
class ShipmentsQuery @Inject()(
    private val system:               ActorSystem,
    private val testData:             TestData,
    protected val dbConfigProvider:   DatabaseConfigProvider,
    protected val shipmentsDao:       ShipmentsDao,
    protected val sequenceNumbersDao: SequenceNumbersDao
  )(
    implicit
    val ec: ExecutionContext)
    extends DatabaseSchema {
  import dbConfig.profile.api._

  type DbOperationResult = Future[DomainValidation[DbOperation]]

  val persistenceId = "shipments-processor-id"

  implicit val mat: Materializer = ActorMaterializer()(system)

  protected val log = LoggerFactory.getLogger(this.getClass)

  val readJournal: JdbcReadJournal =
    PersistenceQuery(system).readJournalFor[JdbcReadJournal](JdbcReadJournal.Identifier)

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  var currentOperation: Option[Future[Unit]] = None

  val eventQueue = new Queue[EventEnvelope]

  private def init(): Unit = {
    val f = for {
      _ <- db.run(shipments.delete)
      _ <- shipmentsDao.addAll(testData.testShipments)
    } yield ()

    f.onComplete {
      case Failure(err) => log.error("failed to initialize query side for shipments: " + err.getMessage)
      case Success(_)   =>
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def handleEvents(): Unit = {
    // stream does not complete
    def handleEventsFrom(startSeqNo: Long): Unit = {
      readJournal
        .eventsByPersistenceId(persistenceId, startSeqNo, Long.MaxValue)
        .runForeach { envelope =>
          eventQueue += envelope
          processEventQueue
          log.info(s"sequence Nr: ${envelope.sequenceNr}, queue size: ${eventQueue.size}")
        }
      ()
    }

    sequenceNumbersDao.sequenceNumberForId(persistenceId).onComplete {
      case Success(v) =>
        v match {
          case scalaz.Success(sn) => handleEventsFrom(sn.sequenceNumber + 1L)
          case scalaz.Failure(_) =>
            init
            handleEventsFrom(0L)
        }
      case Failure(err) =>
        log.error(s"error retrieving sequence number: ${err.getMessage}")
    }
    ()
  }

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  private def processEventQueue(): Unit = {
    if (currentOperation.isEmpty && !eventQueue.isEmpty) {
      val envelope = eventQueue.dequeue
      log.info(s"---> ${envelope.event}, queue size: ${eventQueue.size}")

      val f = handleEvent(envelope.event)
      currentOperation = Some(f)
      f.andThen {
          case Success(_) =>
            sequenceNumbersDao.insertOrUpdate(SequenceNumber(persistenceId, envelope.sequenceNr))
        }
        .onComplete {
          case Failure(err) =>
            log.error(s"error processing event queue: {err.getMessage}")
          case Success(_) =>
            currentOperation = None
            processEventQueue
        }
    }
  }

  private def handleEvent(event: Any): Future[Unit] = {
    processEvent(event).flatMap {
      _ match {
        case scalaz.Success(s) =>
          s match {
            case AddOrUpdate(s) => shipmentsDao.insertOrUpdate(s)
            case Remove(id)     => shipmentsDao.remove(id)
          }
        case scalaz.Failure(err) => Future { log.error(s"$err") }
      }
    }
  }

  private def processEvent(event: Any): DbOperationResult = {
    event match {
      case event: ShipmentEvent =>
        event.eventType match {
          case et: ShipmentEvent.EventType.Added                      => added(event)
          case et: ShipmentEvent.EventType.CourierNameUpdated         => courierNameUpdated(event)
          case et: ShipmentEvent.EventType.TrackingNumberUpdated      => trackingNumberUpdated(event)
          case et: ShipmentEvent.EventType.OriginLocationUpdated      => originUpdated(event)
          case et: ShipmentEvent.EventType.DestinationLocationUpdated => destinationUpdated(event)
          case et: ShipmentEvent.EventType.Created                    => created(event)
          case et: ShipmentEvent.EventType.Packed                     => packed(event)
          case et: ShipmentEvent.EventType.Sent                       => sent(event)
          case et: ShipmentEvent.EventType.Received                   => received(event)
          case et: ShipmentEvent.EventType.Unpacked                   => unpacked(event)
          case et: ShipmentEvent.EventType.Completed                  => completed(event)
          case et: ShipmentEvent.EventType.Lost                       => lost(event)
          case et: ShipmentEvent.EventType.Removed                    => removed(event)
          case et: ShipmentEvent.EventType.SkippedToSentState         => skippedToSentState(event)
          case et: ShipmentEvent.EventType.SkippedToUnpackedState     => skippedToUnpackedState(event)

          case event =>
            Future { DomainError(s"shipment event not handled: $event").failureNel[DbOperation] }
        }
      case event => Future { DomainError(s"event not handled: $event").failureNel[DbOperation] }
    }
  }

  private def added(event: ShipmentEvent): DbOperationResult = {
    Future {
      val companion = event.getAdded
      val eventTime = OffsetDateTime.parse(event.getTime)
      val v = for {
        valid <- eventIsOfType(event, event.eventType.isAdded)
        shipment <- CreatedShipment.create(id = ShipmentId(event.id),
                                           version             = 0L,
                                           timeAdded           = eventTime,
                                           courierName         = companion.getCourierName,
                                           trackingNumber      = companion.getTrackingNumber,
                                           originCentreId      = CentreId(companion.getOriginCentreId),
                                           originLocationId    = LocationId(companion.getOriginLocationId),
                                           destinationCentreId = CentreId(companion.getDestinationCentreId),
                                           destinationLocationId =
                                             LocationId(companion.getDestinationLocationId))
      } yield shipment

      v.map(AddOrUpdate(_))
    }
  }

  private def courierNameUpdated(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getCourierNameUpdated
    onValidEvent(event, event.eventType.isCourierNameUpdated, companion.getVersion) {
      case (shipment, _, eventTime) =>
        for {
          created <- shipment.isCreated
          updated <- created.withCourier(companion.getCourierName)
        } yield AddOrUpdate(updated.copy(timeModified = Some(eventTime)))
    }
  }

  private def trackingNumberUpdated(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getTrackingNumberUpdated
    onValidEvent(event, event.eventType.isTrackingNumberUpdated, companion.getVersion) {
      case (shipment, _, eventTime) =>
        for {
          created <- shipment.isCreated
          updated <- created.withTrackingNumber(companion.getTrackingNumber)
        } yield AddOrUpdate(updated.copy(timeModified = Some(eventTime)))
    }
  }

  private def originUpdated(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getOriginLocationUpdated
    onValidEvent(event, event.eventType.isOriginLocationUpdated, companion.getVersion) {
      case (shipment, _, eventTime) =>
        for {
          created <- shipment.isCreated
          updated <- created.withOrigin(CentreId(companion.getCentreId), LocationId(companion.getLocationId))
        } yield AddOrUpdate(updated.copy(timeModified = Some(eventTime)))
    }
  }

  private def destinationUpdated(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getDestinationLocationUpdated
    onValidEvent(event, event.eventType.isDestinationLocationUpdated, companion.getVersion) {
      case (shipment, _, eventTime) =>
        for {
          created <- shipment.isCreated
          updated <- created.withDestination(CentreId(companion.getCentreId),
                                             LocationId(companion.getLocationId))
        } yield AddOrUpdate(updated.copy(timeModified = Some(eventTime)))
    }
  }

  private def created(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getCreated
    onValidEvent(event, event.eventType.isCreated, companion.getVersion) {
      case (shipment, _, eventTime) =>
        for {
          packed  <- shipment.isPacked
          created <- packed.created.successNel[String]
        } yield AddOrUpdate(created.copy(timeModified = Some(eventTime)))
    }
  }

  private def packed(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getPacked
    onValidEvent(event, event.eventType.isPacked, companion.getVersion) {
      case (shipment, _, eventTime) =>
        val v = shipment match {
          case s: CreatedShipment =>
            s.pack(OffsetDateTime.parse(companion.getStateChangeTime)).successNel[String]
          case s: SentShipment => s.backToPacked.successNel[String]
          case _ => s"shipment state invalid for event: $event".failureNel[PackedShipment]
        }
        v.map(s => AddOrUpdate(s.copy(timeModified = Some(eventTime))))
    }
  }

  private def sent(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getSent
    onValidEvent(event, event.eventType.isSent, companion.getVersion) {
      case (shipment, _, eventTime) =>
        val v = shipment match {
          case s: PackedShipment   => s.send(OffsetDateTime.parse(companion.getStateChangeTime))
          case s: ReceivedShipment => s.backToSent.successNel[String]
          case s: LostShipment     => s.backToSent.successNel[String]
          case _ => s"shipment state invalid for event: $event".failureNel[SentShipment]
        }
        v.map(s => AddOrUpdate(s.copy(timeModified = Some(eventTime))))
    }
  }

  private def received(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getReceived
    onValidEvent(event, event.eventType.isReceived, companion.getVersion) {
      case (shipment, _, eventTime) =>
        val v = shipment match {
          case s: SentShipment     => s.receive(OffsetDateTime.parse(companion.getStateChangeTime))
          case s: UnpackedShipment => s.backToReceived.successNel[String]
          case _ => s"shipment state invalid for event: $event".failureNel[ReceivedShipment]
        }
        v.map(s => AddOrUpdate(s.copy(timeModified = Some(eventTime))))
    }
  }

  private def unpacked(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getUnpacked
    onValidEvent(event, event.eventType.isUnpacked, companion.getVersion) {
      case (shipment, _, eventTime) =>
        val v = shipment match {
          case s: ReceivedShipment  => s.unpack(OffsetDateTime.parse(companion.getStateChangeTime))
          case s: CompletedShipment => s.backToUnpacked.successNel[String]
          case _ => s"shipment state invalid for event: $event".failureNel[UnpackedShipment]
        }
        v.map(s => AddOrUpdate(s.copy(timeModified = Some(eventTime))))
    }
  }

  private def completed(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getCompleted
    onValidEvent(event, event.eventType.isCompleted, companion.getVersion) {
      case (shipment, _, eventTime) =>
        for {
          unpacked  <- shipment.isUnpacked
          completed <- unpacked.complete(OffsetDateTime.parse(companion.getStateChangeTime))
        } yield AddOrUpdate(completed.copy(timeModified = Some(eventTime)))
    }
  }

  private def lost(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getLost
    onValidEvent(event, event.eventType.isLost, companion.getVersion) {
      case (shipment, _, eventTime) =>
        for {
          sent <- shipment.isSent
          lost <- sent.lost.successNel[String]
        } yield AddOrUpdate(lost.copy(timeModified = Some(eventTime)))
    }
  }

  private def removed(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getRemoved
    onValidEvent(event, event.eventType.isRemoved, companion.getVersion) {
      case (shipment, _, eventTime) =>
        shipment.isCreated.map(_ => Remove(shipment.id))
    }
  }

  private def skippedToSentState(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getSkippedToSentState
    onValidEvent(event, event.eventType.isSkippedToSentState, companion.getVersion) {
      case (shipment, _, eventTime) =>
        for {
          created <- shipment.isCreated
          sent <- created.skipToSent(OffsetDateTime.parse(companion.getTimePacked),
                                     OffsetDateTime.parse(companion.getTimeSent))
        } yield AddOrUpdate(sent.copy(timeModified = Some(eventTime)))
    }
  }

  private def skippedToUnpackedState(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getSkippedToUnpackedState
    onValidEvent(event, event.eventType.isSkippedToUnpackedState, companion.getVersion) {
      case (shipment, _, eventTime) =>
        for {
          sent <- shipment.isSent
          unpacked <- sent.skipToUnpacked(OffsetDateTime.parse(companion.getTimeReceived),
                                          OffsetDateTime.parse(companion.getTimeUnpacked))
        } yield AddOrUpdate(unpacked.copy(timeModified = Some(eventTime)))
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.AnyVal"))
  protected def onValidEvent(
      event:        ShipmentEvent,
      eventType:    Boolean,
      eventVersion: Long
    )(applyEvent:   (Shipment, ShipmentEvent, OffsetDateTime) => DomainValidation[DbOperation]
    ): DbOperationResult = {
    if (!eventType) {
      Future { s"invalid event type: $event".failureNel[DbOperation] }
    } else {
      val shipmentId = ShipmentId(event.id)

      shipmentsDao.shipmentWithId(shipmentId).map { result =>
        for {
          shipment <- result
          validVersion <- shipment
                           .requireVersion(eventVersion).leftMap(
                             _ =>
                               NonEmptyList(
                                 s"invalid version for event: shipment version: ${shipment.version}, event: $event"
                               )
                           )
          updated <- applyEvent(shipment, event, OffsetDateTime.parse(event.getTime))
        } yield updated
      }
    }
  }

  private def eventIsOfType(event: ShipmentEvent, eventIsOfType: Boolean) = {
    if (eventIsOfType) ().successNel[String]
    else DomainError(s"event is wrong type: $event").failureNel[Unit]
  }

  //private def shipmentLogInfo(shipment: Shipment): String = s"${shipment.id} | ${shipment.version}"

  handleEvents
}
