package org.biobank.query.centres

import akka.actor._
import akka.persistence.jdbc.query.scaladsl.JdbcReadJournal
import akka.persistence.query.{EventEnvelope, PersistenceQuery}
import java.time.OffsetDateTime
import javax.inject.Inject
import org.biobank._
import org.biobank.domain._
import org.biobank.domain.centres._
import org.biobank.domain.participants._
import org.biobank.infrastructure.events.ShipmentEvents._
import org.biobank.infrastructure.events.ShipmentSpecimenEvents._
import org.biobank.query.db._
import org.slf4j.LoggerFactory
import play.api.db.slick.DatabaseConfigProvider
import scala.concurrent.{ExecutionContext, Future}
//import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._
import scalaz._
import scala.collection.mutable.Queue

final case class AddOrUpdate(shipment: Shipment) extends DbOperation

final case class Remove(shipmentId: ShipmentId) extends DbOperation

final case class SpecimensAdded(shipmentSepecimens: List[ShipmentSpecimen]) extends DbOperation

final case class SpecimenAddOrUpdate(shipmentSpecimen: ShipmentSpecimen) extends DbOperation

final case class SpecimenRemove(shipmentSpecimenId: ShipmentSpecimenId) extends DbOperation

@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter", "org.wartremover.warts.NonUnitStatements"))
class ShipmentsQuery @Inject()(
    private val testData:                      TestData,
    protected val dbConfigProvider:            DatabaseConfigProvider,
    protected val shipmentsRepository:         ShipmentsReadRepository,
    protected val shipmentSpecimensRepository: ShipmentSpecimensReadRepository,
    protected val sequenceNumbersDao:          SequenceNumbersDao
  )(
    implicit
    val system: ActorSystem,
    val ec:     ExecutionContext)
    extends DatabaseSchema {
  import dbConfig.profile.api._

  val persistenceId = "shipments-processor-id"

  protected val log = LoggerFactory.getLogger(this.getClass)

  val readJournal: JdbcReadJournal =
    PersistenceQuery(system).readJournalFor[JdbcReadJournal](JdbcReadJournal.Identifier)

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  var currentOperation: Option[Future[Unit]] = None

  val eventQueue = new Queue[EventEnvelope]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def init(): Unit = {
    val f = for {
      _ <- db.run(shipments.delete)
      _ <- shipmentsRepository.putAll(testData.testShipments)
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

    sequenceNumbersDao.sequenceNumberForId(persistenceId).futval.onComplete {
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
      f.map(_ => sequenceNumbersDao.insertOrUpdate(SequenceNumber(persistenceId, envelope.sequenceNr)))
      f.onComplete {
        case Failure(err) =>
          log.error(s"error processing event queue: {err.getMessage}")
        case Success(_) =>
          currentOperation = None
          processEventQueue
      }
    }
  }

  private def handleEvent(event: Any): Future[Unit] = {
    val f = processEvent(event)
    f.map {
      _ match {
        case AddOrUpdate(s)     => shipmentsRepository.put(s)
        case Remove(id)         => shipmentsRepository.remove(id)
        case SpecimensAdded(ss) => shipmentSpecimensRepository.putAll(ss.toSeq)
      }
    }
    f.futval.map(_ => ())
  }

  private def processEvent(event: Any): FutureValidation[DbOperation] = {
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

          case et =>
            FutureValidation(DomainError(s"shipment event not handled: $event").failureNel[DbOperation])
        }

      case event: ShipmentSpecimenEvent =>
        event.eventType match {
          case et: ShipmentSpecimenEvent.EventType.Added            => specimensAdded(event)
          case et: ShipmentSpecimenEvent.EventType.Removed          => specimenRemoved(event)
          case et: ShipmentSpecimenEvent.EventType.ContainerUpdated => specimenContainerUpdated(event)
          case et: ShipmentSpecimenEvent.EventType.Present          => specimenPresent(event)
          case et: ShipmentSpecimenEvent.EventType.Received         => specimenReceived(event)
          case et: ShipmentSpecimenEvent.EventType.Missing          => specimenMissing(event)
          case et: ShipmentSpecimenEvent.EventType.Extra            => specimenExtra(event)

          case et =>
            FutureValidation(
              DomainError(s"shipment specimen event not handled: $event").failureNel[DbOperation]
            )

        }

      case event =>
        FutureValidation(DomainError(s"shipment event not handled: $event").failureNel[DbOperation])
    }
  }

  private def added(event: ShipmentEvent): FutureValidation[DbOperation] = {
    FutureValidation(Future {
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
    })
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

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def specimensAdded(event: ShipmentSpecimenEvent): DbOperationResult = {
    FutureValidation {
      val companionEvent      = event.getAdded
      val eventTime           = OffsetDateTime.parse(event.getTime)
      val shipmentId          = ShipmentId(event.shipmentId)
      val shipmentContainerId = companionEvent.shipmentContainerId.map(ShipmentContainerId.apply)

      companionEvent.shipmentSpecimenAddData
        .map { info =>
          ShipmentSpecimen
            .create(id                  = ShipmentSpecimenId(info.getShipmentSpecimenId),
                    version             = 0L,
                    shipmentId          = shipmentId,
                    specimenId          = SpecimenId(info.getSpecimenId),
                    state               = ShipmentItemState.Present,
                    shipmentContainerId = shipmentContainerId)
            .map(_.copy(timeAdded = eventTime))
        }
        .toList.sequenceU
        .map(SpecimensAdded(_))
    }
  }

  private def specimenRemoved(event: ShipmentSpecimenEvent) = {
    ???
  }

  private def specimenContainerUpdated(event: ShipmentSpecimenEvent) = {
    ???
  }

  private def specimenPresent(event: ShipmentSpecimenEvent) = {
    ???
  }

  private def specimenReceived(event: ShipmentSpecimenEvent) = {
    ???
  }

  private def specimenMissing(event: ShipmentSpecimenEvent) = {
    ???
  }

  private def specimenExtra(event: ShipmentSpecimenEvent) = {
    ???
  }

  @SuppressWarnings(Array("org.wartremover.warts.AnyVal"))
  protected def onValidEvent(
      event:        ShipmentEvent,
      eventType:    Boolean,
      eventVersion: Long
    )(applyEvent:   (Shipment, ShipmentEvent, OffsetDateTime) => SystemValidation[DbOperation]
    ): FutureValidation[DbOperation] = {
    if (!eventType) {
      FutureValidation(Future { s"invalid event type: $event".failureNel[DbOperation] })
    } else {
      val shipmentId = ShipmentId(event.id)

      for {
        shipment <- shipmentsRepository.getByKey(shipmentId)
        valid <- FutureValidation(Future {
                  for {
                    validVersion <- shipment
                                     .requireVersion(eventVersion).leftMap(
                                       _ =>
                                         NonEmptyList(
                                           s"invalid version for event: shipment version: ${shipment.version}, event: $event"
                                         )
                                     )
                    updated <- applyEvent(shipment, event, OffsetDateTime.parse(event.getTime))
                  } yield updated
                })
      } yield valid
    }
  }

  private def eventIsOfType(event: ShipmentEvent, eventIsOfType: Boolean) = {
    if (eventIsOfType) ().successNel[String]
    else DomainError(s"event is wrong type: $event").failureNel[Unit]
  }

  //private def shipmentLogInfo(shipment: Shipment): String = s"${shipment.id} | ${shipment.version}"

  handleEvents
}
