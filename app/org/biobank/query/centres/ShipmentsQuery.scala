package org.biobank.query.centres

import akka.NotUsed
import akka.actor._
import akka.stream.scaladsl.Source
import akka.persistence.jdbc.query.scaladsl.JdbcReadJournal
import akka.persistence.query.{EventEnvelope, PersistenceQuery}
import cats.data._
import cats.implicits._
import com.google.inject.ImplementedBy
import java.time.OffsetDateTime
import javax.inject.Inject
import org.biobank._
import org.biobank.domain._
import org.biobank.domain.centres._
import org.biobank.domain.participants.{SpecimenId, SpecimenRepository}
import org.biobank.dto.centres.{CentreLocationInfo, ShipmentDto, ShipmentSpecimenDto}
import org.biobank.infrastructure.events.ShipmentEvents._
import org.biobank.infrastructure.events.ShipmentSpecimenEvents._
import org.biobank.query.db._
import org.biobank.validation.Validation._
import org.slf4j.LoggerFactory
import play.api.db.slick.DatabaseConfigProvider
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import scala.collection.mutable.Queue
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

final case class AddOrUpdate(shipment: ShipmentDto) extends DbOperation

final case class Remove(shipmentId: ShipmentId) extends DbOperation

final case class SpecimensAdded(shipmentSepecimens: List[ShipmentSpecimenDto]) extends DbOperation

final case class SpecimensUpdated(shipmentSepecimens: List[ShipmentSpecimenDto]) extends DbOperation

final case class SpecimenAddOrUpdate(shipmentSpecimen: ShipmentSpecimenDto) extends DbOperation

final case class SpecimenRemove(shipmentSpecimenId: ShipmentSpecimenId) extends DbOperation

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
@ImplementedBy(classOf[ShipmentsQueryJdbc])
trait ShipmentsQuery extends DatabaseSchema {

  val persistenceId = "shipments-processor-id"

  protected val log = LoggerFactory.getLogger(this.getClass)

  implicit protected val system: ActorSystem
  implicit protected val ec:     ExecutionContext

  protected val dbConfig: DatabaseConfig[JdbcProfile]
  import dbConfig.profile.api._

  protected val centreRepository:            CentreRepository
  protected val shipmentsRepository:         ShipmentsReadRepository
  protected val shipmentSpecimensRepository: ShipmentSpecimensReadRepository
  protected val sequenceNumbersDao:          SequenceNumbersDao

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  var currentOperation: Option[Future[Unit]] = None

  val eventQueue = new Queue[EventEnvelope]

  def eventsByPersistenceId(
      persistenceId:  String,
      fromSequenceNr: Long,
      toSequenceNr:   Long
    ): Source[EventEnvelope, NotUsed]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  protected def init(): Future[Unit] = {
    val f = for {
      _ <- db.run(shipments.delete)
      _ <- db.run(shipmentSpecimens.delete)
    } yield ()

    f.onComplete {
      case Failure(err) => log.error("failed to initialize query side for shipments: " + err.getMessage)
      case _            =>
    }
    f
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def handleEvents(): Unit = {
    // stream does not complete
    def handleEventsFrom(startSeqNo: Long): Unit = {
      log.debug(s"handleEventsFrom: startSeqNo: $startSeqNo")
      eventsByPersistenceId(persistenceId, startSeqNo, Long.MaxValue)
        .runForeach { envelope =>
          eventQueue += envelope
          processEventQueue
          log.info(s"sequence Nr: ${envelope.sequenceNr}, queue size: ${eventQueue.size}")
        }
      ()
    }

    sequenceNumbersDao.sequenceNumberForId(persistenceId).value.onComplete {
      case Success(v) =>
        v match {
          case Right(sn) => handleEventsFrom(sn.sequenceNumber + 1L)
          case Left(_) =>
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
    f.value.map(_ => ())
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

          case et =>
            EitherT.leftT[Future, DbOperation](
              NonEmptyChain(IllegalStateError(s"invalid shipment event: $event"))
            )
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
            EitherT.leftT[Future, DbOperation](
              NonEmptyChain(IllegalStateError(s"invalid shipment specimen event: $event"))
            )

        }

      case event =>
        EitherT.leftT[Future, DbOperation](NonEmptyChain(IllegalStateError(s"invalid process event: $event")))
    }
  }

  private def added(event: ShipmentEvent): DbOperationResult = {
    import scalaz.Validation.FlatMap._

    val companion = event.getAdded
    val v: DomainValidation[Tuple4[Centre, Location, Centre, Location]] = for {
      origin              <- centreRepository.getByKey(CentreId(companion.getOriginCentreId))
      originLocation      <- origin.locationWithId(LocationId(companion.getOriginLocationId))
      destination         <- centreRepository.getByKey(CentreId(companion.getDestinationCentreId))
      destinationLocation <- destination.locationWithId(LocationId(companion.getDestinationLocationId))
    } yield (origin, originLocation, destination, destinationLocation)

    v match {
      case scalaz.Success((origin, originLocation, destination, destinationLocation)) =>
        val dbOp = eventIsOfType(event, event.eventType.isAdded).map { _ =>
          AddOrUpdate(
            ShipmentDto(id                   = ShipmentId(event.id),
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
          )
        }
        EitherT(Future.successful(dbOp.toEither))

      case scalaz.Failure(e) =>
        EitherT.leftT[Future, DbOperation](NonEmptyChain(IllegalStateError(e.list.toList.mkString(","))))
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
        ).validNec
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
        ).validNec
    }
  }

  private def originUpdated(event: ShipmentEvent): DbOperationResult = {
    import scalaz.Validation.FlatMap._

    val companion = event.getOriginLocationUpdated
    onValidEvent(event, event.eventType.isOriginLocationUpdated, companion.getVersion) {
      case (shipment, _, eventTime) =>
        val v = for {
          origin         <- centreRepository.getByKey(CentreId(companion.getCentreId))
          originLocation <- origin.locationWithId(LocationId(companion.getLocationId))
        } yield (origin, originLocation)

        v match {
          case scalaz.Success((origin, originLocation)) =>
            val dbOp = AddOrUpdate(
              shipment.copy(origin       = CentreLocationInfo(origin, originLocation),
                            version      = shipment.version + 1L,
                            timeModified = Some(eventTime))
            )
            dbOp.validNec

          case scalaz.Failure(e) => IllegalStateError(e.list.toList.mkString(",")).invalidNec
        }
    }
  }

  private def destinationUpdated(event: ShipmentEvent): DbOperationResult = {
    import scalaz.Validation.FlatMap._

    val companion = event.getDestinationLocationUpdated
    onValidEvent(event, event.eventType.isDestinationLocationUpdated, companion.getVersion) {
      case (shipment, _, eventTime) =>
        val v = for {
          destination         <- centreRepository.getByKey(CentreId(companion.getCentreId))
          destinationLocation <- destination.locationWithId(LocationId(companion.getLocationId))
        } yield (destination, destinationLocation)

        v match {
          case scalaz.Success((destination, destinationLocation)) =>
            val dbOp = AddOrUpdate(
              shipment.copy(destination  = CentreLocationInfo(destination, destinationLocation),
                            version      = shipment.version + 1L,
                            timeModified = Some(eventTime))
            )
            dbOp.validNec

          case scalaz.Failure(e) => IllegalStateError(e.list.toList.mkString(",")).invalidNec
        }
    }
  }

  private def changeState(
      shipment:  ShipmentDto,
      newState:  EntityState,
      eventTime: OffsetDateTime
    ): DbOperation = {
    AddOrUpdate(
      shipment.copy(state = newState, version = shipment.version + 1L, timeModified = Some(eventTime))
    )
  }

  private def created(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getCreated
    onValidEvent(event, event.eventType.isCreated, companion.getVersion) {
      case (shipment, _, eventTime) =>
        changeState(shipment, Shipment.createdState, eventTime).validNec
    }
  }

  private def packed(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getPacked
    onValidEvent(event, event.eventType.isPacked, companion.getVersion) {
      case (shipment, _, eventTime) =>
        changeState(shipment, Shipment.packedState, eventTime).validNec
    }
  }

  private def sent(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getSent
    onValidEvent(event, event.eventType.isSent, companion.getVersion) {
      case (shipment, _, eventTime) =>
        changeState(shipment, Shipment.sentState, eventTime).validNec
    }
  }

  private def received(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getReceived
    onValidEvent(event, event.eventType.isReceived, companion.getVersion) {
      case (shipment, _, eventTime) =>
        changeState(shipment, Shipment.receivedState, eventTime).validNec
    }
  }

  private def unpacked(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getUnpacked
    onValidEvent(event, event.eventType.isUnpacked, companion.getVersion) {
      case (shipment, _, eventTime) =>
        changeState(shipment, Shipment.unpackedState, eventTime).validNec
    }
  }

  private def completed(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getCompleted
    onValidEvent(event, event.eventType.isCompleted, companion.getVersion) {
      case (shipment, _, eventTime) =>
        changeState(shipment, Shipment.completedState, eventTime).validNec
    }
  }

  private def lost(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getLost
    onValidEvent(event, event.eventType.isLost, companion.getVersion) {
      case (shipment, _, eventTime) =>
        changeState(shipment, Shipment.lostState, eventTime).validNec
    }
  }

  private def removed(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getRemoved
    onValidEvent(event, event.eventType.isRemoved, companion.getVersion) {
      case (shipment, _, eventTime) =>
        if (shipment.state == Shipment.createdState) Remove(shipment.id).validNec
        else IllegalStateError(s"shipment not in created state: ${shipment.id}").invalidNec
    }
  }

  private def skippedToSentState(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getSkippedToSentState
    onValidEvent(event, event.eventType.isSkippedToSentState, companion.getVersion) {
      case (shipment, _, eventTime) =>
        changeState(shipment, Shipment.sentState, eventTime).validNec
    }
  }

  private def skippedToUnpackedState(event: ShipmentEvent): DbOperationResult = {
    val companion = event.getSkippedToUnpackedState
    onValidEvent(event, event.eventType.isSkippedToUnpackedState, companion.getVersion) {
      case (shipment, _, eventTime) =>
        changeState(shipment, Shipment.unpackedState, eventTime).validNec
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def specimensAdded(event: ShipmentSpecimenEvent): DbOperationResult = {
    val companionEvent = event.getAdded

    val dtos = companionEvent.shipmentSpecimenAddData.map { info =>
      ShipmentSpecimenDto(id                  = ShipmentSpecimenId(info.getShipmentSpecimenId),
                          version             = 0L,
                          timeAdded           = OffsetDateTime.parse(event.getTime),
                          timeModified        = None,
                          state               = ShipmentSpecimen.presentState,
                          shipmentId          = ShipmentId(event.shipmentId),
                          specimenId          = SpecimenId(info.getSpecimenId),
                          shipmentContainerId = companionEvent.shipmentContainerId.map(ShipmentContainerId(_)))
    }.toList

    EitherT.rightT[Future, NonEmptyChain[ValidationError]](SpecimensAdded(dtos))
  }

  private def specimenRemoved(event: ShipmentSpecimenEvent) = {
    ???
  }

  private def specimenContainerUpdated(event: ShipmentSpecimenEvent) = {
    ???
  }

  private def specimenPresent(event: ShipmentSpecimenEvent) = {
    val timeModified   = OffsetDateTime.parse(event.getTime)
    val companionEvent = event.getPresent.shipmentSpecimenData
    val ssVersions =
      companionEvent.map(ce => ShipmentSpecimenId(ce.getShipmentSpecimenId) -> ce.getVersion).toMap

    specimenUpdateState(ShipmentId(event.shipmentId), ssVersions, ShipmentSpecimen.presentState, timeModified)
  }

  private def specimensReceived(event: ShipmentSpecimenEvent): DbOperationResult = {
    val timeModified   = OffsetDateTime.parse(event.getTime)
    val companionEvent = event.getReceived.shipmentSpecimenData
    val ssVersions =
      companionEvent.map(ce => ShipmentSpecimenId(ce.getShipmentSpecimenId) -> ce.getVersion).toMap

    specimenUpdateState(ShipmentId(event.shipmentId),
                        ssVersions,
                        ShipmentSpecimen.receivedState,
                        timeModified)
  }

  private def specimenMissing(event: ShipmentSpecimenEvent) = {
    val timeModified   = OffsetDateTime.parse(event.getTime)
    val companionEvent = event.getMissing.shipmentSpecimenData
    val ssVersions =
      companionEvent.map(ce => ShipmentSpecimenId(ce.getShipmentSpecimenId) -> ce.getVersion).toMap

    specimenUpdateState(ShipmentId(event.shipmentId), ssVersions, ShipmentSpecimen.missingState, timeModified)
  }

  private def specimenExtra(event: ShipmentSpecimenEvent) = {
    ???
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def specimenUpdateState(
      shipmentId:   ShipmentId,
      ssVersions:   Map[ShipmentSpecimenId, Long],
      newState:     EntityState,
      timeModified: OffsetDateTime
    ): DbOperationResult = {
    val shipmentSpecimenIds = ssVersions.keys.toSeq

    shipmentsRepository.getByKey(shipmentId).flatMap { shipment =>
      val r = shipmentSpecimensRepository.forShipment(shipmentId, shipmentSpecimenIds: _*).map { specimens =>
        val updated = specimens
          .map { ss =>
            if (ss.version == ssVersions(ss.id)) {
              ss.copy(state = newState, version = ss.version + 1L, timeModified = Some(timeModified)).validNec
            } else {
              IllegalStateError(s"shipment specimen version is invalid: id: ${ss.id}").invalidNec
            }
          }.toList.sequence
        updated.map(SpecimensUpdated(_)).toEither
      }
      EitherT(r)
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.AnyVal"))
  protected def onValidEvent(
      event:        ShipmentEvent,
      eventType:    Boolean,
      eventVersion: Long
    )(applyEvent:   (ShipmentDto, ShipmentEvent, OffsetDateTime) => ValidationResult[DbOperation]
    ): DbOperationResult = {
    if (!eventType) {
      EitherT(Future.successful {
        Either.left(NonEmptyChain(IllegalStateError(s"invalid event type: $event")))
      })
    } else {
      val shipmentId = ShipmentId(event.id)

      for {
        shipment <- shipmentsRepository.getByKey(shipmentId)
        valid <- EitherT(
                  Future.successful(applyEvent(shipment, event, OffsetDateTime.parse(event.getTime)).toEither)
                )

      } yield valid
    }
  }

  private def eventIsOfType(event: ShipmentEvent, eventIsOfType: Boolean): ValidationResult[Unit] = {
    if (eventIsOfType) ().validNec
    else IllegalStateError(s"event is wrong type: $event").invalidNec
  }

  //private def shipmentLogInfo(shipment: Shipment): String = s"${shipment.id} | ${shipment.version}"

  handleEvents
}

@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter", "org.wartremover.warts.NonUnitStatements"))
class ShipmentsQueryJdbc @Inject()(
    private val testData:                      TestData,
    protected val dbConfigProvider:            DatabaseConfigProvider,
    protected val centreRepository:            CentreRepository,
    protected val specimenRepository:          SpecimenRepository,
    protected val shipmentsRepository:         ShipmentsReadRepository,
    protected val shipmentSpecimensRepository: ShipmentSpecimensReadRepository,
    protected val sequenceNumbersDao:          SequenceNumbersDao
  )(
    implicit
    val system: ActorSystem,
    val ec:     ExecutionContext)
    extends ShipmentsQuery {

  def eventsByPersistenceId(
      persistenceId:  String,
      fromSequenceNr: Long,
      toSequenceNr:   Long
    ): Source[EventEnvelope, NotUsed] =
    PersistenceQuery(system)
      .readJournalFor[JdbcReadJournal](JdbcReadJournal.Identifier)
      .eventsByPersistenceId(persistenceId, fromSequenceNr, toSequenceNr)

  override protected def init(): Future[Unit] = {
    val f = for {
      _ <- super.init
      _ <- shipmentsRepository.putAll(testData.testShipmentDtos)
      _ <- shipmentSpecimensRepository.putAll(testData.testShipmentSpecimenDtos)
    } yield ()

    f.onComplete {
      case Failure(err) => log.error("failed to initialize query side for shipments: " + err.getMessage)
      case _            =>
    }
    f
  }

}
