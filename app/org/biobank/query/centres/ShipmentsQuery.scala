package org.biobank.query.centres

import akka.actor._
import akka.persistence.jdbc.query.scaladsl.JdbcReadJournal
import akka.persistence.query.{EventEnvelope, PersistenceQuery}
import java.time.OffsetDateTime
import javax.inject.Inject
import org.biobank._
import org.biobank.domain._
import org.biobank.domain.centres._
import org.biobank.domain.participants.SpecimenId
import org.biobank.dto.centres.{CentreLocationInfo, ShipmentDto, ShipmentSpecimenDto}
import org.biobank.infrastructure.events.ShipmentEvents._
import org.biobank.infrastructure.events.ShipmentSpecimenEvents._
import org.biobank.query.db._
import org.slf4j.LoggerFactory
import play.api.db.slick.DatabaseConfigProvider
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._
import scalaz._
import scala.collection.mutable.Queue

final case class AddOrUpdate(shipment: ShipmentDto) extends DbOperation

final case class Remove(shipmentId: ShipmentId) extends DbOperation

final case class SpecimensAdded(shipmentSepecimens: List[ShipmentSpecimenDto]) extends DbOperation

final case class SpecimensUpdated(shipmentSepecimens: List[ShipmentSpecimenDto]) extends DbOperation

final case class SpecimenAddOrUpdate(shipmentSpecimen: ShipmentSpecimenDto) extends DbOperation

final case class SpecimenRemove(shipmentSpecimenId: ShipmentSpecimenId) extends DbOperation

@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter", "org.wartremover.warts.NonUnitStatements"))
class ShipmentsQuery @Inject()(
    private val testData:                      TestData,
    protected val dbConfigProvider:            DatabaseConfigProvider,
    protected val centreRepository:            CentreRepository,
    protected val shipmentsRepository:         ShipmentsReadRepository,
    protected val shipmentSpecimensRepository: ShipmentSpecimensReadRepository,
    protected val sequenceNumbersDao:          SequenceNumbersDao
  )(
    implicit
    val system: ActorSystem,
    val ec:     ExecutionContext)
    extends DatabaseSchema {
  import dbConfig.profile.api._
  import org.biobank.CommonValidations._

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
      _ <- shipmentsRepository.putAll(testData.testShipmentDtos)
      _ <- db.run(shipmentSpecimens.delete)
      _ <- shipmentSpecimensRepository.putAll(testData.testShipmentSpecimenDtos)
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
      log.debug(s"handleEventsFrom: startSeqNo: $startSeqNo")
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
        //log.error(s"error retrieving sequence number: ${err.getMessage}")
        handleEventsFrom(0L)
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
          log.error(s"error processing event queue: ${err.getMessage}")
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
        case AddOrUpdate(s)       => shipmentsRepository.put(s)
        case Remove(id)           => shipmentsRepository.remove(id)
        case SpecimensAdded(ss)   => shipmentSpecimensRepository.putAll(ss.toSeq)
        case SpecimensUpdated(ss) => shipmentSpecimensRepository.putAll(ss.toSeq)
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
          case et: ShipmentSpecimenEvent.EventType.Received         => specimensReceived(event)
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
    FutureValidation {
      val companion = event.getAdded
      val v = for {
        valid               <- eventIsOfType(event, event.eventType.isAdded)
        origin              <- centreRepository.getByKey(CentreId(companion.getOriginCentreId))
        originLocation      <- origin.locationWithId(LocationId(companion.getOriginLocationId))
        destination         <- centreRepository.getByKey(CentreId(companion.getDestinationCentreId))
        destinationLocation <- origin.locationWithId(LocationId(companion.getDestinationLocationId))
      } yield ShipmentDto(id                   = ShipmentId(event.id),
                          version              = 0L,
                          timeAdded            = OffsetDateTime.parse(event.getTime),
                          timeModified         = None,
                          state                = Shipment.createdState,
                          courierName          = companion.getCourierName,
                          trackingNumber       = companion.getTrackingNumber,
                          origin               = CentreLocationInfo(origin, originLocation),
                          destination          = CentreLocationInfo(destination, destinationLocation),
                          timePacked           = None,
                          timeSent             = None,
                          timeReceived         = None,
                          timeUnpacked         = None,
                          timeCompleted        = None,
                          specimenCount        = 0,
                          presentSpecimenCount = 0,
                          containerCount       = 0)

      v.map(AddOrUpdate(_))
    }
  }

  private def courierNameUpdated(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getCourierNameUpdated
    onValidEvent(event, event.eventType.isCourierNameUpdated, companion.getVersion) {
      case (shipment, _, eventTime) =>
        AddOrUpdate(
          shipment.copy(courierName  = companion.getCourierName,
                        version      = shipment.version + 1L,
                        timeModified = Some(eventTime))
        ).successNel[String]
    }
  }

  private def trackingNumberUpdated(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getTrackingNumberUpdated
    onValidEvent(event, event.eventType.isTrackingNumberUpdated, companion.getVersion) {
      case (shipment, _, eventTime) =>
        AddOrUpdate(
          shipment.copy(trackingNumber = companion.getTrackingNumber,
                        version        = shipment.version + 1L,
                        timeModified   = Some(eventTime))
        ).successNel[String]
    }
  }

  private def originUpdated(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getOriginLocationUpdated
    onValidEvent(event, event.eventType.isOriginLocationUpdated, companion.getVersion) {
      case (shipment, _, eventTime) =>
        for {
          origin         <- centreRepository.getByKey(CentreId(companion.getCentreId))
          originLocation <- origin.locationWithId(LocationId(companion.getLocationId))
        } yield AddOrUpdate(
          shipment.copy(origin       = CentreLocationInfo(origin, originLocation),
                        version      = shipment.version + 1L,
                        timeModified = Some(eventTime))
        )
    }
  }

  private def destinationUpdated(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getDestinationLocationUpdated
    onValidEvent(event, event.eventType.isDestinationLocationUpdated, companion.getVersion) {
      case (shipment, _, eventTime) =>
        for {
          destination         <- centreRepository.getByKey(CentreId(companion.getCentreId))
          destinationLocation <- destination.locationWithId(LocationId(companion.getLocationId))
        } yield AddOrUpdate(
          shipment.copy(destination  = CentreLocationInfo(destination, destinationLocation),
                        version      = shipment.version + 1L,
                        timeModified = Some(eventTime))
        )
    }
  }

  private def changeState(
      shipment:  ShipmentDto,
      newState:  EntityState,
      eventTime: OffsetDateTime
    ): SystemValidation[DbOperation] = {
    AddOrUpdate(
      shipment.copy(state = newState, version = shipment.version + 1L, timeModified = Some(eventTime))
    ).successNel[String]
  }

  private def created(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getCreated
    onValidEvent(event, event.eventType.isCreated, companion.getVersion) {
      case (shipment, _, eventTime) =>
        changeState(shipment, Shipment.createdState, eventTime)
    }
  }

  private def packed(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getPacked
    onValidEvent(event, event.eventType.isPacked, companion.getVersion) {
      case (shipment, _, eventTime) =>
        changeState(shipment, Shipment.packedState, eventTime)
    }
  }

  private def sent(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getSent
    onValidEvent(event, event.eventType.isSent, companion.getVersion) {
      case (shipment, _, eventTime) =>
        changeState(shipment, Shipment.sentState, eventTime)
    }
  }

  private def received(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getReceived
    onValidEvent(event, event.eventType.isReceived, companion.getVersion) {
      case (shipment, _, eventTime) =>
        changeState(shipment, Shipment.receivedState, eventTime)
    }
  }

  private def unpacked(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getUnpacked
    onValidEvent(event, event.eventType.isUnpacked, companion.getVersion) {
      case (shipment, _, eventTime) =>
        changeState(shipment, Shipment.unpackedState, eventTime)
    }
  }

  private def completed(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getCompleted
    onValidEvent(event, event.eventType.isCompleted, companion.getVersion) {
      case (shipment, _, eventTime) =>
        changeState(shipment, Shipment.completedState, eventTime)
    }
  }

  private def lost(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getLost
    onValidEvent(event, event.eventType.isLost, companion.getVersion) {
      case (shipment, _, eventTime) =>
        changeState(shipment, Shipment.lostState, eventTime)
    }
  }

  private def removed(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getRemoved
    onValidEvent(event, event.eventType.isRemoved, companion.getVersion) {
      case (shipment, _, eventTime) =>
        if (shipment.state == Shipment.createdState) Remove(shipment.id).successNel[String]
        else InvalidState(s"shipment not in created state: ${shipment.id}").failureNel[DbOperation]
    }
  }

  private def skippedToSentState(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getSkippedToSentState
    onValidEvent(event, event.eventType.isSkippedToSentState, companion.getVersion) {
      case (shipment, _, eventTime) =>
        changeState(shipment, Shipment.sentState, eventTime)
    }
  }

  private def skippedToUnpackedState(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getSkippedToUnpackedState
    onValidEvent(event, event.eventType.isSkippedToUnpackedState, companion.getVersion) {
      case (shipment, _, eventTime) =>
        changeState(shipment, Shipment.unpackedState, eventTime)
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def specimensAdded(event: ShipmentSpecimenEvent): DbOperationResult = {
    FutureValidation {
      val companionEvent = event.getAdded
      val dtos = companionEvent.shipmentSpecimenAddData.map { info =>
        ShipmentSpecimenDto(id           = ShipmentSpecimenId(info.getShipmentSpecimenId),
                            version      = 0L,
                            timeAdded    = OffsetDateTime.parse(event.getTime),
                            timeModified = None,
                            state        = ShipmentItemState.Present,
                            shipmentId   = ShipmentId(event.shipmentId),
                            specimenId   = SpecimenId(info.getSpecimenId),
                            shipmentContainerId =
                              companionEvent.shipmentContainerId.map(ShipmentContainerId(_)))
      }.toList

      SpecimensAdded(dtos).successNel[String]
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

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def specimensReceived(event: ShipmentSpecimenEvent): DbOperationResult = {
    val companionEvent      = event.getReceived.shipmentSpecimenData
    val versionsData        = companionEvent.map(e => e.getShipmentSpecimenId -> e.getVersion).toMap
    val shipmentSpecimenIds = versionsData.keys.map(ShipmentSpecimenId(_)).toSeq
    val shipmentId          = ShipmentId(event.shipmentId)
    for {
      shipment          <- shipmentsRepository.getByKey(shipmentId)
      shipmentSpecimens <- shipmentSpecimensRepository.forShipment(shipmentId, shipmentSpecimenIds: _*)
    } yield {
      val timeModified = Some(OffsetDateTime.parse(event.getTime))
      val updated = shipmentSpecimens.map { ss =>
        ss.copy(state = ShipmentItemState.Received, version = ss.version + 1L, timeModified = timeModified)
      }
      SpecimensUpdated(updated.toList)
    }
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
    )(applyEvent:   (ShipmentDto, ShipmentEvent, OffsetDateTime) => SystemValidation[DbOperation]
    ): FutureValidation[DbOperation] = {
    if (!eventType) {
      FutureValidation(Future { s"invalid event type: $event".failureNel[DbOperation] })
    } else {
      val shipmentId = ShipmentId(event.id)

      for {
        shipment <- shipmentsRepository.getByKey(shipmentId)
        valid    <- FutureValidation { applyEvent(shipment, event, OffsetDateTime.parse(event.getTime)) }
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
