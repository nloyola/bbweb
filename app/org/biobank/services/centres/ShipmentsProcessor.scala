package org.biobank.services.centres

import akka.actor._
import akka.persistence.{RecoveryCompleted, SaveSnapshotFailure, SaveSnapshotSuccess, SnapshotOffer}
import cats.data._
import cats.implicits._
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import javax.inject.Inject
import org.biobank.domain.LocationId
import org.biobank.domain.centres._
import org.biobank.domain.participants.{Specimen, SpecimenId, SpecimenRepository}
import org.biobank.domain.users.UserId
import org.biobank.infrastructure.commands.ShipmentCommands._
import org.biobank.infrastructure.commands.ShipmentSpecimenCommands._
import org.biobank.infrastructure.events.EventUtils
import org.biobank.infrastructure.events.ShipmentEvents._
import org.biobank.infrastructure.events.ShipmentSpecimenEvents._
import org.biobank.services.{Processor, SnapshotWriter}
import org.biobank.services.participants.SpecimensService
import org.biobank.validation.Validation._
import play.api.libs.json._

object ShipmentsProcessor {

  def props: Props = Props[ShipmentsProcessor]

  final case class SnapshotState(shipments: Set[Shipment], shipmentSpecimens: Set[ShipmentSpecimen])

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val snapshotStateFormat: Format[SnapshotState] = Json.format[SnapshotState]

}

/**
 * Handles commands related to shipments.
 */
class ShipmentsProcessor @Inject()(
    val shipmentRepository:         ShipmentsWriteRepository,
    val shipmentSpecimenRepository: ShipmentSpecimensWriteRepository,
    val centreRepository:           CentreRepository,
    val specimenRepository:         SpecimenRepository,
    val specimensService:           SpecimensService,
    val snapshotWriter:             SnapshotWriter)
    extends Processor with ShipmentValidations {
  import ShipmentsProcessor._

  override def persistenceId: String = "shipments-processor-id"

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  private var replyTo: Option[ActorRef] = None

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  val receiveRecover: Receive = {
    case event: ShipmentEvent =>
      log.debug(s"ShipmentsProcessor: receiveRecover: $event")

      val r = event.eventType match {
        case et: ShipmentEvent.EventType.Added => applyAddedEvent(event)
        case et: ShipmentEvent.EventType.CourierNameUpdated => applyCourierNameUpdatedEvent(event)
        case et: ShipmentEvent.EventType.TrackingNumberUpdated => applyTrackingNumberUpdatedEvent(event)
        case et: ShipmentEvent.EventType.OriginLocationUpdated => applyOriginLocationUpdatedEvent(event)
        case et: ShipmentEvent.EventType.DestinationLocationUpdated =>
          applyDestinationLocationUpdatedEvent(event)
        case et: ShipmentEvent.EventType.Created                => applyCreatedEvent(event)
        case et: ShipmentEvent.EventType.Packed                 => applyPackedEvent(event)
        case et: ShipmentEvent.EventType.Sent                   => applySentEvent(event)
        case et: ShipmentEvent.EventType.Received               => applyReceivedEvent(event)
        case et: ShipmentEvent.EventType.Unpacked               => applyUnpackedEvent(event)
        case et: ShipmentEvent.EventType.Completed              => applyCompletedEvent(event)
        case et: ShipmentEvent.EventType.Lost                   => applyLostEvent(event)
        case et: ShipmentEvent.EventType.Removed                => applyRemovedEvent(event)
        case et: ShipmentEvent.EventType.SkippedToSentState     => applySkippedToSentStateEvent(event)
        case et: ShipmentEvent.EventType.SkippedToUnpackedState => applySkippedToUnpackedStateEvent(event)

        case event => Error(s"shipment event not handled: $event").invalidNec
      }

      // to make wart remover happy
      if (r.isInvalid) log.error(r.toString)

    case event: ShipmentSpecimenEvent =>
      log.debug(s"ShipmentsProcessor: $event")

      val r = event.eventType match {
        case et: ShipmentSpecimenEvent.EventType.Added            => applySpecimensAddedEvent(event)
        case et: ShipmentSpecimenEvent.EventType.Removed          => applySpecimenRemovedEvent(event)
        case et: ShipmentSpecimenEvent.EventType.ContainerUpdated => applySpecimenContainerUpdatedEvent(event)
        case et: ShipmentSpecimenEvent.EventType.Present          => applySpecimenPresentEvent(event)
        case et: ShipmentSpecimenEvent.EventType.Received         => applySpecimenReceivedEvent(event)
        case et: ShipmentSpecimenEvent.EventType.Missing          => applySpecimenMissingEvent(event)
        case et: ShipmentSpecimenEvent.EventType.Extra            => applySpecimenExtraEvent(event)

        case event => Error(s"shipment specimen event not handled: $event").invalidNec
      }

      // to make wart remover happy
      if (r.isInvalid) log.error(r.toString)

    case SnapshotOffer(_, snapshotFilename: String) =>
      applySnapshot(snapshotFilename)

    case RecoveryCompleted =>
      log.debug(s"ShipmentsProcessor: recovery completed")
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any", "org.wartremover.warts.Throw"))
  val receiveCommand: Receive = {

    case cmd: AddShipmentCmd =>
      processWithResultCats(addCmdToEvent(cmd))(applyAddedEvent)

    case cmd: UpdateShipmentCourierNameCmd =>
      processUpdateCmdOnCreated(cmd, updateCourierNameCmdToEvent, applyCourierNameUpdatedEvent)

    case cmd: UpdateShipmentTrackingNumberCmd =>
      processUpdateCmdOnCreated(cmd, updateTrackingNumberCmdToEvent, applyTrackingNumberUpdatedEvent)

    case cmd: UpdateShipmentOriginCmd =>
      processUpdateCmdOnCreated(cmd, updateoriginLocationCmdToEvent, applyOriginLocationUpdatedEvent)

    case cmd: UpdateShipmentDestinationCmd =>
      processUpdateCmdOnCreated(cmd,
                                updatedestinationLocationCmdToEvent,
                                applyDestinationLocationUpdatedEvent)

    case cmd: CreatedShipmentCmd =>
      processUpdateCmd(cmd, createdCmdToEvent, applyCreatedEvent)

    case cmd: PackShipmentCmd =>
      processUpdateCmd(cmd, packCmdToEvent, applyPackedEvent)

    case cmd: SendShipmentCmd =>
      processUpdateCmd(cmd, sendCmdToEvent, applySentEvent)

    case cmd: ReceiveShipmentCmd =>
      processUpdateCmd(cmd, receiveCmdToEvent, applyReceivedEvent)

    case cmd: UnpackShipmentCmd =>
      processUpdateCmd(cmd, unpackCmdToEvent, applyUnpackedEvent)

    case cmd: CompleteShipmentCmd =>
      processUpdateCmd(cmd, completeCmdToEvent, applyCompletedEvent)

    case cmd: LostShipmentCmd =>
      processUpdateCmd(cmd, lostCmdToEvent, applyLostEvent)

    case cmd: ShipmentSkipStateToSentCmd =>
      processUpdateCmdOnCreated(cmd, skipStateToSentCmdToEvent, applySkippedToSentStateEvent)

    case cmd: ShipmentSkipStateToUnpackedCmd =>
      processUpdateCmd(cmd, skipStateToUnpackedCmdToEvent, applySkippedToUnpackedStateEvent)

    case cmd: ShipmentRemoveCmd =>
      processUpdateCmdOnCreated(cmd, removeCmdToEvent, applyRemovedEvent)

    case cmd: ShipmentAddSpecimensCmd =>
      processWithResultCats(addSpecimenCmdToEvent(cmd))(applySpecimensAddedEvent)

    case cmd: ShipmentSpecimenUpdateContainerCmd =>
      processSpecimensCmd(cmd, updateSpecimenContainerCmdToEvent, applySpecimenContainerUpdatedEvent)

    case cmd: ShipmentSpecimenExtraCmd =>
      processSpecimensCmd(cmd, specimenExtraCmdToEvent, applySpecimenExtraEvent)

    case cmd: ShipmentSpecimenRemoveCmd =>
      processSpecimenUpdateCmd(cmd, removeSpecimenCmdToEvent, applySpecimenRemovedEvent)

    case cmd: ShipmentSpecimensPresentCmd =>
      processSpecimenUpdateCmd(cmd, presentSpecimensCmdToEvent, applySpecimenPresentEvent)

    case cmd: ShipmentSpecimensReceiveCmd =>
      processSpecimenUpdateCmd(cmd, receiveSpecimensCmdToEvent, applySpecimenReceivedEvent)

    case cmd: ShipmentSpecimenMissingCmd =>
      processSpecimenUpdateCmd(cmd, specimenMissingCmdToEvent, applySpecimenMissingEvent)

    case "persistence_restart" =>
      throw new Exception("Intentionally throwing exception to test persistence by restarting the actor")

    case "snap" =>
      mySaveSnapshot
      replyTo = Some(sender())

    case SaveSnapshotSuccess(metadata) =>
      log.debug(s"SaveSnapshotSuccess: $metadata")
      replyTo.foreach(_ ! akka.actor.Status.Success(s"snapshot saved: $metadata"))
      replyTo = None

    case SaveSnapshotFailure(metadata, reason) =>
      log.debug(s"SaveSnapshotFailure: $metadata, reason: $reason")
      replyTo.foreach(_ ! akka.actor.Status.Failure(reason))
      replyTo = None

    case cmd => log.error(s"shipmentsProcessor: message not handled: $cmd")
  }

  private def mySaveSnapshot(): Unit = {
    val snapshotState =
      SnapshotState(shipmentRepository.getValues.toSet, shipmentSpecimenRepository.getValues.toSet)
    val filename = snapshotWriter.save(persistenceId, Json.toJson(snapshotState).toString)
    saveSnapshot(filename)
  }

  private def applySnapshot(filename: String): Unit = {
    log.debug(s"snapshot recovery file: $filename")
    val fileContents = snapshotWriter.load(filename);
    Json
      .parse(fileContents).validate[SnapshotState].fold(
        errors => log.error(s"could not apply snapshot: $filename: $errors"),
        snapshot => {
          log.debug(s"snapshot contains ${snapshot.shipments.size} shipments")
          log.debug(s"snapshot contains ${snapshot.shipmentSpecimens.size} shipment specimens")
          snapshot.shipments.foreach(shipmentRepository.put)
          snapshot.shipmentSpecimens.foreach(shipmentSpecimenRepository.put)
        }
      )
  }

  private def addCmdToEvent(cmd: AddShipmentCmd) = {
    import scalaz.Validation.FlatMap._

    val v = for {
      originCentre      <- centreRepository.getByLocationId(LocationId(cmd.originLocationId))
      destinationCentre <- centreRepository.getByLocationId(LocationId(cmd.destinationLocationId))
    } yield (originCentre, destinationCentre)

    v match {
      case scalaz.Success((originCentre, destinationCentre)) =>
        validNewIdentityCats(shipmentRepository.nextIdentity, shipmentRepository).andThen { id =>
          (trackingNumberAvailable(cmd.trackingNumber),
           CreatedShipment.create(id                    = id,
                                  version               = 0L,
                                  timeAdded             = OffsetDateTime.now,
                                  courierName           = cmd.courierName,
                                  trackingNumber        = cmd.trackingNumber,
                                  originCentreId        = originCentre.id,
                                  originLocationId      = LocationId(cmd.originLocationId),
                                  destinationCentreId   = destinationCentre.id,
                                  destinationLocationId = LocationId(cmd.destinationLocationId))).mapN {
            (trackingNo, shipment) =>
              ShipmentEvent(id.id).update(_.sessionUserId := cmd.sessionUserId,
                                          _.time := OffsetDateTime.now
                                            .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                          _.added.courierName           := shipment.courierName,
                                          _.added.trackingNumber        := shipment.trackingNumber,
                                          _.added.originCentreId        := shipment.originCentreId.id,
                                          _.added.originLocationId      := shipment.originLocationId.id,
                                          _.added.destinationCentreId   := shipment.destinationCentreId.id,
                                          _.added.destinationLocationId := shipment.destinationLocationId.id)
          }
        }

      case scalaz.Failure(e) => IllegalStateError(e.list.toList.mkString(",")).invalidNec
    }
  }

  private def updateCourierNameCmdToEvent(
      cmd:      UpdateShipmentCourierNameCmd,
      shipment: CreatedShipment
    ): ValidationResult[ShipmentEvent] =
    shipment.withCourier(cmd.courierName).map { s =>
      ShipmentEvent(shipment.id.id).update(_.sessionUserId := cmd.sessionUserId,
                                           _.time := OffsetDateTime.now
                                             .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                           _.courierNameUpdated.version     := cmd.expectedVersion,
                                           _.courierNameUpdated.courierName := cmd.courierName)
    }

  private def updateTrackingNumberCmdToEvent(
      cmd:      UpdateShipmentTrackingNumberCmd,
      shipment: CreatedShipment
    ): ValidationResult[ShipmentEvent] =
    trackingNumberAvailable(cmd.trackingNumber, shipment.id)
      .andThen(_ => shipment.withTrackingNumber(cmd.trackingNumber)).map { _ =>
        ShipmentEvent(shipment.id.id)
          .update(_.sessionUserId                        := cmd.sessionUserId,
                  _.time                                 := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                  _.trackingNumberUpdated.version        := cmd.expectedVersion,
                  _.trackingNumberUpdated.trackingNumber := cmd.trackingNumber)
      }

  private def updateoriginLocationCmdToEvent(
      cmd:      UpdateShipmentOriginCmd,
      shipment: CreatedShipment
    ): ValidationResult[ShipmentEvent] = {
    import scalaz.Validation.FlatMap._

    val v = for {
      centre   <- centreRepository.getByLocationId(LocationId(cmd.locationId))
      location <- centre.locationWithId(LocationId(cmd.locationId))
    } yield (centre, location)

    v match {
      case scalaz.Success((centre, location)) =>
        shipment.withOrigin(centre.id, location.id).map { _ =>
          ShipmentEvent(shipment.id.id).update(_.sessionUserId := cmd.sessionUserId,
                                               _.time := OffsetDateTime.now
                                                 .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                               _.originLocationUpdated.version    := cmd.expectedVersion,
                                               _.originLocationUpdated.centreId   := centre.id.id,
                                               _.originLocationUpdated.locationId := cmd.locationId)
        }

      case scalaz.Failure(err) => Error(err.list.toList.mkString(",")).invalidNec
    }
  }

  private def updatedestinationLocationCmdToEvent(
      cmd:      UpdateShipmentDestinationCmd,
      shipment: CreatedShipment
    ): ValidationResult[ShipmentEvent] = {
    import scalaz.Validation.FlatMap._

    val v = for {
      centre   <- centreRepository.getByLocationId(LocationId(cmd.locationId))
      location <- centre.locationWithId(LocationId(cmd.locationId))
    } yield (centre, location)

    v match {
      case scalaz.Success((centre, location)) =>
        shipment.withDestination(centre.id, location.id).map { _ =>
          ShipmentEvent(shipment.id.id).update(_.sessionUserId := cmd.sessionUserId,
                                               _.time := OffsetDateTime.now
                                                 .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                               _.destinationLocationUpdated.version    := cmd.expectedVersion,
                                               _.destinationLocationUpdated.centreId   := centre.id.id,
                                               _.destinationLocationUpdated.locationId := cmd.locationId)
        }
      case scalaz.Failure(err) => Error(err.list.toList.mkString(",")).invalidNec
    }
  }

  private def createdCmdToEvent(
      cmd:      CreatedShipmentCmd,
      shipment: Shipment
    ): ValidationResult[ShipmentEvent] =
    shipment.isPacked.fold(err => InvalidState(s"shipment is not packed: ${shipment.id}").invalidNec,
                           s =>
                             ShipmentEvent(shipment.id.id)
                               .update(
                                 _.sessionUserId   := cmd.sessionUserId,
                                 _.time            := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                 _.created.version := cmd.expectedVersion
                               ).validNec)

  private def packCmdToEvent(cmd: PackShipmentCmd, shipment: Shipment): ValidationResult[ShipmentEvent] = {
    val numPresentSpecimens =
      shipmentSpecimenRepository.shipmentSpecimenCount(shipment.id, ShipmentSpecimen.presentState)
    if (numPresentSpecimens <= 0) {
      InvalidState(s"shipment has no specimens: ${shipment.id}").invalidNec
    } else {
      shipment match {
        case _: CreatedShipment | _: SentShipment =>
          ShipmentEvent(shipment.id.id)
            .update(_.sessionUserId  := cmd.sessionUserId,
                    _.time           := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                    _.packed.version := cmd.expectedVersion,
                    _.packed.stateChangeTime := cmd.datetime
                      .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)).validNec
        case _ =>
          InvalidState(s"cannot change to packed state: ${shipment.id}").invalidNec
      }
    }
  }

  private def sendCmdToEvent(cmd: SendShipmentCmd, shipment: Shipment): ValidationResult[ShipmentEvent] = {
    val valid = shipment match {
      case ps: PackedShipment   => ps.send(cmd.datetime)
      case rs: ReceivedShipment => rs.backToSent().validNec
      case ls: LostShipment     => ls.backToSent().validNec
      case _ =>
        InvalidState(s"cannot change to sent state: ${shipment.id}").invalidNec
    }

    valid.map { s =>
      ShipmentEvent(shipment.id.id).update(_.sessionUserId := cmd.sessionUserId,
                                           _.time := OffsetDateTime.now
                                             .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                           _.sent.version := cmd.expectedVersion,
                                           _.sent.stateChangeTime := cmd.datetime.format(
                                             DateTimeFormatter.ISO_OFFSET_DATE_TIME
                                           ))
    }
  }

  private def receiveCmdToEvent(
      cmd:      ReceiveShipmentCmd,
      shipment: Shipment
    ): ValidationResult[ShipmentEvent] = {
    val valid = shipment match {
      case ss: SentShipment =>
        ss.receive(cmd.datetime)
      case us: UnpackedShipment =>
        // all items must be in present state to allow this state transition
        val nonPresentExist = shipmentSpecimenRepository.forShipment(us.id).exists { ss =>
          ss.state != ShipmentSpecimen.presentState
        }
        if (nonPresentExist)
          InvalidState(s"cannot change to received state, items have already been processed: ${us.id}").invalidNec
        else
          us.backToReceived().validNec
      case _ =>
        InvalidState(s"cannot change to received state: ${shipment.id}").invalidNec
    }
    valid.map { s =>
      ShipmentEvent(shipment.id.id).update(_.sessionUserId := cmd.sessionUserId,
                                           _.time := OffsetDateTime.now
                                             .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                           _.received.version := cmd.expectedVersion,
                                           _.received.stateChangeTime := cmd.datetime.format(
                                             DateTimeFormatter.ISO_OFFSET_DATE_TIME
                                           ))
    }
  }

  private def unpackCmdToEvent(
      cmd:      UnpackShipmentCmd,
      shipment: Shipment
    ): ValidationResult[ShipmentEvent] = {
    val valid = shipment match {
      case rs: ReceivedShipment  => rs.unpack(cmd.datetime)
      case cs: CompletedShipment => cs.backToUnpacked().validNec
      case _ =>
        InvalidState(s"cannot change to unpacked state: ${shipment.id}").invalidNec
    }
    valid.map { s =>
      ShipmentEvent(shipment.id.id).update(_.sessionUserId := cmd.sessionUserId,
                                           _.time := OffsetDateTime.now
                                             .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                           _.unpacked.version := cmd.expectedVersion,
                                           _.unpacked.stateChangeTime := cmd.datetime.format(
                                             DateTimeFormatter.ISO_OFFSET_DATE_TIME
                                           ))
    }
  }

  private def completeCmdToEvent(
      cmd:      CompleteShipmentCmd,
      shipment: Shipment
    ): ValidationResult[ShipmentEvent] = {
    val numPresentSpecimens =
      shipmentSpecimenRepository.shipmentSpecimenCount(shipment.id, ShipmentSpecimen.presentState)
    if (numPresentSpecimens > 0) {
      InvalidState(s"shipment has specimens in present state: ${shipment.id}").invalidNec
    } else {
      val valid = shipment match {
        case us: UnpackedShipment => us.complete(cmd.datetime)
        case _ =>
          InvalidState(s"cannot change to completed state: ${shipment.id}").invalidNec
      }
      valid.map { s =>
        ShipmentEvent(shipment.id.id).update(_.sessionUserId := cmd.sessionUserId,
                                             _.time := OffsetDateTime.now
                                               .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                             _.completed.version := cmd.expectedVersion,
                                             _.completed.stateChangeTime := cmd.datetime.format(
                                               DateTimeFormatter.ISO_OFFSET_DATE_TIME
                                             ))
      }
    }
  }

  private def lostCmdToEvent(cmd: LostShipmentCmd, shipment: Shipment): ValidationResult[ShipmentEvent] =
    shipment.isSent.fold(err => InvalidState(s"cannot change to lost state: ${shipment.id}").invalidNec,
                         s =>
                           ShipmentEvent(shipment.id.id)
                             .update(
                               _.sessionUserId := cmd.sessionUserId,
                               _.time          := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                               _.lost.version  := cmd.expectedVersion
                             ).validNec)

  private def skipStateToSentCmdToEvent(
      cmd:      ShipmentSkipStateToSentCmd,
      shipment: CreatedShipment
    ): ValidationResult[ShipmentEvent] =
    shipment.skipToSent(cmd.timePacked, cmd.timeSent).map { _ =>
      ShipmentEvent(shipment.id.id).update(_.sessionUserId := cmd.sessionUserId,
                                           _.time := OffsetDateTime.now
                                             .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                           _.skippedToSentState.version := cmd.expectedVersion,
                                           _.skippedToSentState.timePacked := cmd.timePacked
                                             .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                           _.skippedToSentState.timeSent := cmd.timeSent
                                             .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME))
    }

  private def skipStateToUnpackedCmdToEvent(
      cmd:      ShipmentSkipStateToUnpackedCmd,
      shipment: Shipment
    ): ValidationResult[ShipmentEvent] =
    shipment match {
      case s: SentShipment =>
        s.skipToUnpacked(cmd.timeReceived, cmd.timeUnpacked).map { _ =>
          ShipmentEvent(s.id.id).update(_.sessionUserId := cmd.sessionUserId,
                                        _.time := OffsetDateTime.now
                                          .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                        _.skippedToUnpackedState.version := cmd.expectedVersion,
                                        _.skippedToUnpackedState.timeReceived := cmd.timeReceived
                                          .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                        _.skippedToUnpackedState.timeUnpacked := cmd.timeUnpacked
                                          .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME))
        }
      case _ =>
        InvalidState(s"shipment not sent: ${shipment.state}").invalidNec
    }

  private def removeCmdToEvent(
      cmd:      ShipmentRemoveCmd,
      shipment: CreatedShipment
    ): ValidationResult[ShipmentEvent] = {
    val shipmentId = ShipmentId(cmd.id)
    shipmentRepository.getCreated(ShipmentId(cmd.id)).andThen { _ =>
      if (shipmentSpecimenRepository.forShipment(shipmentId).isEmpty) {
        ShipmentEvent(shipment.id.id)
          .update(_.sessionUserId   := cmd.sessionUserId,
                  _.time            := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                  _.removed.version := cmd.expectedVersion).validNec
      } else {
        Error(s"shipment has specimens, remove specimens first").invalidNec
      }
    }
  }

  private def addSpecimenCmdToEvent(cmd: ShipmentAddSpecimensCmd): ValidationResult[ShipmentSpecimenEvent] = {
    val sessionUserId       = UserId(cmd.sessionUserId)
    val shipmentId          = ShipmentId(cmd.shipmentId)
    val shipmentContainerId = cmd.shipmentContainerId.map(ShipmentContainerId.apply)

    specimensService.getByInventoryIds(sessionUserId, cmd.specimenInventoryIds: _*) match {
      case scalaz.Failure(err) => Error(err.list.toList.mkString(",")).invalidNec
      case scalaz.Success(specimens) =>
        shipmentRepository
          .getCreated(shipmentId).andThen { shipment =>
            createShipmentSpecimens(shipment, shipmentContainerId, specimens)
          }.map { shipmentSpecimens =>
            val shipmentSpecimenAddData = shipmentSpecimens.map { ss =>
              ShipmentSpecimenEvent
                .ShipmentSpecimenAddInfo().update(_.specimenId         := ss.specimenId.id,
                                                  _.shipmentSpecimenId := ss.id.id)
            }
            ShipmentSpecimenEvent(cmd.shipmentId).update(_.sessionUserId := cmd.sessionUserId,
                                                         _.time := OffsetDateTime.now
                                                           .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                                         _.added.optionalShipmentContainerId := cmd.shipmentContainerId,
                                                         _.added.shipmentSpecimenAddData     := shipmentSpecimenAddData)
          }
    }
  }

  private def validShipment(shipmentId: String, expectedVersion: Long): ValidationResult[Shipment] = {
    shipmentRepository.exists(ShipmentId(shipmentId)).andThen { shipment =>
      shipment.requireVersionCats(expectedVersion).map(_ => shipment)
    }
  }

  private def validShipmentSpecimen(
      shipmentSpecimenId: String,
      expectedVersion:    Long
    ): ValidationResult[ShipmentSpecimen] = {
    shipmentSpecimenRepository.exists(ShipmentSpecimenId(shipmentSpecimenId)).andThen { ss =>
      ss.requireVersionCats(expectedVersion).map(_ => ss)
    }
  }

  private def removeSpecimenCmdToEvent(
      cmd:      ShipmentSpecimenRemoveCmd,
      shipment: Shipment
    ): ValidationResult[ShipmentSpecimenEvent] = {
    val v: ValidationResult[Shipment] =
      validShipmentSpecimen(cmd.shipmentSpecimenId, cmd.expectedVersion).andThen { ss: ShipmentSpecimen =>
        if (ss.state == ShipmentSpecimen.presentState) {
          shipmentRepository.getCreated(ss.shipmentId)
        } else if (ss.state == ShipmentSpecimen.extraState) {
          shipmentRepository.getUnpacked(ss.shipmentId)
        } else {
          EntityCriteriaError(s"cannot remove, shipment specimen state is invalid: ${ss.state}").invalidNec
        }
      }

    v.map { _ =>
      ShipmentSpecimenEvent(shipment.id.id).update(_.sessionUserId := cmd.sessionUserId,
                                                   _.time := OffsetDateTime.now
                                                     .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                                   _.removed.version            := cmd.expectedVersion,
                                                   _.removed.shipmentSpecimenId := cmd.shipmentSpecimenId)
    }
  }

  private def updateSpecimenContainerCmdToEvent(
      cmd:      ShipmentSpecimenUpdateContainerCmd,
      shipment: Shipment
    ): ValidationResult[ShipmentSpecimenEvent] = {
    // FIXME: validate that shipmentContainerId is a container in the repository
    //
    //val shipmentContainerId = cmd.shipmentContainerId.map(ShipmentContainerId.apply)
    shipmentRepository
      .getCreated(ShipmentId(cmd.shipmentId)).andThen { shipment =>
        shipmentSpecimensPresent(shipment.id, cmd.specimenInventoryIds: _*)
          .andThen { ss =>
            ???
          // val v = Error(s"shipping specimens with containers has not been implemented yet").invalidNec
          // v.map { _ =>
          //   val shipmentSpecimenData = ss.map(EventUtils.shipmentSpecimenInfoToEvent).toSeq
          //   ShipmentSpecimenEvent(cmd.shipmentId).update(_.sessionUserId := cmd.sessionUserId,
          //                                                _.time := OffsetDateTime.now
          //                                                  .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
          //                                                _.containerUpdated.optionalShipmentContainerId := cmd.shipmentContainerId,
          //                                                _.containerUpdated.shipmentSpecimenData        := shipmentSpecimenData)
          // }
          }
      }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def presentSpecimensCmdToEvent(
      cmd:      ShipmentSpecimensPresentCmd,
      shipment: Shipment
    ): ValidationResult[ShipmentSpecimenEvent] = {

    shipment.isUnpacked.andThen { shipment =>
      shipmentSpecimensNotPresent(shipment.id, cmd.specimenInventoryIds: _*).andThen { ss =>
        ShipmentSpecimen.makePresent(ss).map { _ =>
          val eventData = ss.map(EventUtils.shipmentSpecimenInfoToEvent).toSeq
          ShipmentSpecimenEvent(cmd.shipmentId)
            .update(_.sessionUserId := cmd.sessionUserId,
                    _.time := OffsetDateTime.now
                      .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                    _.present.shipmentSpecimenData := eventData)
        }
      }
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def receiveSpecimensCmdToEvent(
      cmd:      ShipmentSpecimensReceiveCmd,
      shipment: Shipment
    ): ValidationResult[ShipmentSpecimenEvent] = {
    shipment.isUnpacked.andThen { shipment =>
      shipmentSpecimensPresent(shipment.id, cmd.specimenInventoryIds: _*).andThen { ss =>
        ShipmentSpecimen.makeReceived(ss).map { _ =>
          val eventData = ss.map(EventUtils.shipmentSpecimenInfoToEvent).toSeq
          ShipmentSpecimenEvent(cmd.shipmentId)
            .update(_.sessionUserId := cmd.sessionUserId,
                    _.time := OffsetDateTime.now
                      .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                    _.received.shipmentSpecimenData := eventData)
        }
      }
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def specimenMissingCmdToEvent(
      cmd:      ShipmentSpecimenMissingCmd,
      shipment: Shipment
    ): ValidationResult[ShipmentSpecimenEvent] = {
    shipment.isUnpacked.andThen { shipment =>
      shipmentSpecimensPresent(shipment.id, cmd.specimenInventoryIds: _*).andThen { ss =>
        ShipmentSpecimen.makeMissing(ss).map { _ =>
          val eventData = ss.map(EventUtils.shipmentSpecimenInfoToEvent).toSeq
          ShipmentSpecimenEvent(cmd.shipmentId).update(_.sessionUserId := cmd.sessionUserId,
                                                       _.time := OffsetDateTime.now
                                                         .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                                       _.missing.shipmentSpecimenData := eventData)
        }
      }
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def getSpecimens(specimenInventoryIds: String*): ValidationResult[List[Specimen]] = {
    import scalaz.Scalaz._

    val v = specimenInventoryIds
      .map { inventoryId =>
        specimenRepository.getByInventoryId(inventoryId).leftMap(err => scalaz.NonEmptyList(inventoryId))
      }.toList.sequenceU.leftMap(
        err =>
          org.biobank.CommonValidations
            .EntityCriteriaError("invalid inventory Ids: " + err.list.toList.mkString(", ")).nel
      )

    v match {
      case scalaz.Success(specimens) => specimens.validNec
      case scalaz.Failure(err)       => Error(err.list.toList.mkString(",")).invalidNec
    }
  }

  /**
   * Specimens that were not recorded as being in this shipment were actually received in the shipment.
   *
   * The specimens must be moved to this centre if and only if they are already at the centre the shipment is
   * coming from.
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def specimenExtraCmdToEvent(
      cmd:      ShipmentSpecimenExtraCmd,
      shipment: Shipment
    ): ValidationResult[ShipmentSpecimenEvent] = {
    val sessionUserId = UserId(cmd.sessionUserId)

    specimensService.getByInventoryIds(sessionUserId, cmd.specimenInventoryIds: _*) match {
      case scalaz.Failure(err) => Error(err.list.toList.mkString(",")).invalidNec
      case scalaz.Success(specimens) =>
        (shipment.isUnpacked,
         specimensNotInShipment(shipment.id, specimens: _*),
         specimensNotPresentInShipment(specimens:       _*),
         createShipmentSpecimens(shipment, None, specimens))
          .mapN { (_, _, _, shipmentSpecimens) =>
            shipmentSpecimens
          }.andThen { shipmentSpecimens =>
            ShipmentSpecimen.makeExtra(shipmentSpecimens.toList)
          }
          .map { ssList: List[ShipmentSpecimen] =>
            val shipmentSpecimenData = ssList.map { ss =>
              ShipmentSpecimenEvent
                .ShipmentSpecimenAddInfo().update(_.specimenId         := ss.specimenId.id,
                                                  _.shipmentSpecimenId := ss.id.id)
            }
            ShipmentSpecimenEvent(cmd.shipmentId).update(_.sessionUserId := cmd.sessionUserId,
                                                         _.time := OffsetDateTime.now
                                                           .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                                         _.extra.shipmentSpecimenData := shipmentSpecimenData)
          }
    }
  }

  private def applyAddedEvent(event: ShipmentEvent): ValidationResult[Shipment] = {
    if (!event.eventType.isAdded) {
      Error(s"invalid event type: $event").invalidNec
    } else {
      val addedEvent = event.getAdded
      val eventTime  = OffsetDateTime.parse(event.getTime)
      val add = CreatedShipment.create(id = ShipmentId(event.id),
                                       version               = 0L,
                                       timeAdded             = eventTime,
                                       courierName           = addedEvent.getCourierName,
                                       trackingNumber        = addedEvent.getTrackingNumber,
                                       originCentreId        = CentreId(addedEvent.getOriginCentreId),
                                       originLocationId      = LocationId(addedEvent.getOriginLocationId),
                                       destinationCentreId   = CentreId(addedEvent.getDestinationCentreId),
                                       destinationLocationId = LocationId(addedEvent.getDestinationLocationId))
      add.foreach(shipmentRepository.put)
      add
    }
  }

  private def applyCourierNameUpdatedEvent(event: ShipmentEvent) = {
    onValidEventAndVersion(event,
                           event.eventType.isCourierNameUpdated,
                           event.getCourierNameUpdated.getVersion) { (shipment, _, time) =>
      val v = shipment.isCreated.andThen(_.withCourier(event.getCourierNameUpdated.getCourierName, time))
      v.foreach(shipmentRepository.put)
      v
    }
  }

  private def applyTrackingNumberUpdatedEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event,
                           event.eventType.isTrackingNumberUpdated,
                           event.getTrackingNumberUpdated.getVersion) { (shipment, _, time) =>
      val v = shipment.isCreated.andThen(
        _.withTrackingNumber(event.getTrackingNumberUpdated.getTrackingNumber, time)
      )
      v.foreach(shipmentRepository.put)
      v
    }

  private def applyOriginLocationUpdatedEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event,
                           event.eventType.isOriginLocationUpdated,
                           event.getOriginLocationUpdated.getVersion) { (shipment, _, time) =>
      val centreId   = CentreId(event.getOriginLocationUpdated.getCentreId)
      val locationId = LocationId(event.getOriginLocationUpdated.getLocationId)
      val v          = shipment.isCreated.andThen(_.withOrigin(centreId, locationId, time))
      v.foreach(shipmentRepository.put)
      v
    }

  private def applyDestinationLocationUpdatedEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event,
                           event.eventType.isDestinationLocationUpdated,
                           event.getDestinationLocationUpdated.getVersion) { (shipment, _, time) =>
      val centreId   = CentreId(event.getDestinationLocationUpdated.getCentreId)
      val locationId = LocationId(event.getDestinationLocationUpdated.getLocationId)
      val v          = shipment.isCreated.andThen(_.withDestination(centreId, locationId, time))
      v.foreach(shipmentRepository.put)
      v
    }

  private def applyCreatedEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event, event.eventType.isCreated, event.getCreated.getVersion) {
      (shipment, _, time) =>
        shipment.isPacked.map { p =>
          val created = p.created(time)
          shipmentRepository.put(created)
          created
        }
    }

  private def applyPackedEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event, event.eventType.isPacked, event.getPacked.getVersion) {
      (shipment, _, time) =>
        val stateChangeTime = OffsetDateTime.parse(event.getPacked.getStateChangeTime)

        val packed = shipment match {
          case created: CreatedShipment => created.pack(stateChangeTime, time).validNec
          case sent:    SentShipment    => sent.backToPacked(time).validNec
          case _ => InvalidState(s"cannot change to packed state: ${shipment.id}").invalidNec
        }

        packed.foreach(shipmentRepository.put)
        packed
    }

  private def applySentEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event, event.eventType.isSent, event.getSent.getVersion) { (shipment, _, time) =>
      val stateChangeTime = OffsetDateTime.parse(event.getSent.getStateChangeTime)
      val sent = shipment match {
        case packed:   PackedShipment   => packed.send(stateChangeTime, time)
        case received: ReceivedShipment => received.backToSent(time).validNec
        case lost:     LostShipment     => lost.backToSent(time).validNec
        case _ => InvalidState(s"cannot change to sent state: ${shipment.id}").invalidNec
      }

      sent.foreach(shipmentRepository.put)
      sent
    }

  private def applyReceivedEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event, event.eventType.isReceived, event.getReceived.getVersion) {
      (shipment, _, time) =>
        val stateChangeTime = OffsetDateTime.parse(event.getReceived.getStateChangeTime)
        val received = shipment match {
          case sent:     SentShipment     => sent.receive(stateChangeTime, time)
          case unpacked: UnpackedShipment => unpacked.backToReceived(time).validNec
          case _ =>
            InvalidState(s"cannot change to received state: ${shipment.id}").invalidNec
        }

        received.foreach(shipmentRepository.put)
        received
    }

  private def applyUnpackedEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event, event.eventType.isUnpacked, event.getUnpacked.getVersion) {
      (shipment, _, time) =>
        val stateChangeTime = OffsetDateTime.parse(event.getUnpacked.getStateChangeTime)

        val unpacked = shipment match {
          case received:  ReceivedShipment  => received.unpack(stateChangeTime, time)
          case completed: CompletedShipment => completed.backToUnpacked(time).validNec
          case _ =>
            InvalidState(s"cannot change to received state: ${shipment.id}").invalidNec
        }

        unpacked.foreach(shipmentRepository.put)
        unpacked
    }

  private def applyCompletedEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event, event.eventType.isCompleted, event.getCompleted.getVersion) {
      (shipment, _, time) =>
        val stateChangeTime = OffsetDateTime.parse(event.getCompleted.getStateChangeTime)
        val completed       = shipment.isUnpacked.andThen(_.complete(stateChangeTime))

        completed.foreach(shipmentRepository.put)
        completed
    }

  private def applyLostEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event, event.eventType.isLost, event.getLost.getVersion) { (shipment, _, time) =>
      shipment.isSent.map { sent =>
        val lost = sent.lost
        shipmentRepository.put(lost)
        lost
      }
    }

  private def applySkippedToSentStateEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event,
                           event.eventType.isSkippedToSentState,
                           event.getSkippedToSentState.getVersion) { (shipment, _, time) =>
      val timePacked = OffsetDateTime.parse(event.getSkippedToSentState.getTimePacked)
      val timeSent   = OffsetDateTime.parse(event.getSkippedToSentState.getTimeSent)
      val v          = shipment.isCreated.andThen(_.skipToSent(timePacked, timeSent, time))
      v.foreach(shipmentRepository.put)
      v
    }

  private def applySkippedToUnpackedStateEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event,
                           event.eventType.isSkippedToUnpackedState,
                           event.getSkippedToUnpackedState.getVersion) { (shipment, _, time) =>
      val timeReceived = OffsetDateTime.parse(event.getSkippedToUnpackedState.getTimeReceived)
      val timeUnpacked = OffsetDateTime.parse(event.getSkippedToUnpackedState.getTimeUnpacked)

      val v = shipment.isSent.andThen(_.skipToUnpacked(timeReceived, timeUnpacked, time))
      v.foreach(shipmentRepository.put)
      v
    }

  private def applyRemovedEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event, event.eventType.isRemoved, event.getRemoved.getVersion) {
      (shipment, _, time) =>
        shipmentRepository.remove(shipment)
        shipment.validNec
    }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def applySpecimensAddedEvent(event: ShipmentSpecimenEvent): ValidationResult[Shipment] = {
    import cats.implicits.toTraverseOps

    if (!event.eventType.isAdded) {
      Error(s"invalid event type: $event").invalidNec
    } else {
      val addedEvent          = event.getAdded
      val eventTime           = OffsetDateTime.parse(event.getTime)
      val shipmentId          = ShipmentId(event.shipmentId)
      val shipmentContainerId = addedEvent.shipmentContainerId.map(ShipmentContainerId.apply)

      shipmentRepository.exists(shipmentId).andThen { shipment =>
        addedEvent.shipmentSpecimenAddData
          .map { info =>
            val created = ShipmentSpecimen.create(id = ShipmentSpecimenId(info.getShipmentSpecimenId),
                                                  version             = 0L,
                                                  shipmentId          = shipmentId,
                                                  specimenId          = SpecimenId(info.getSpecimenId),
                                                  state               = ShipmentSpecimen.presentState,
                                                  shipmentContainerId = shipmentContainerId)
            created.foreach(ss => shipmentSpecimenRepository.put(ss.copy(timeAdded = eventTime)))
            created
          }
          .toList.sequence.map(_ => shipment)
      }
    }
  }

  private def applySpecimenRemovedEvent(event: ShipmentSpecimenEvent) =
    onValidSpecimenEvent(event, event.eventType.isRemoved) { (shipment, _, time) =>
      val removedEvent = event.getRemoved
      validShipmentSpecimen(removedEvent.getShipmentSpecimenId, removedEvent.getVersion)
        .map { shipmentSpecimen =>
          shipmentSpecimenRepository.remove(shipmentSpecimen)
          shipment
        }
    }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def applySpecimenContainerUpdatedEvent(event: ShipmentSpecimenEvent): ValidationResult[Shipment] = {
    if (!event.eventType.isContainerUpdated) {
      Error(s"invalid event type: $event").invalidNec
    } else {
      val companionEvent      = event.getContainerUpdated
      val eventTime           = OffsetDateTime.parse(event.getTime)
      val shipmentId          = ShipmentId(event.shipmentId)
      val shipmentContainerId = companionEvent.shipmentContainerId.map(ShipmentContainerId.apply)

      (shipmentRepository.exists(shipmentId),
       companionEvent.shipmentSpecimenData
         .map { info =>
           validShipmentSpecimen(info.getShipmentSpecimenId, info.getVersion).andThen(
             _.withShipmentContainer(shipmentContainerId, eventTime)
           )
         }.toList.sequence).mapN { (shipment, shipmentSpecimens) =>
        shipmentSpecimens.foreach(shipmentSpecimenRepository.put)
        shipment
      }
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def specimenStateUpdate(
      event:                ShipmentSpecimenEvent,
      shipmentSpecimenData: Seq[ShipmentSpecimenEvent.ShipmentSpecimenInfo],
      eventTime:            OffsetDateTime
    )(stateUpdateFn:        ShipmentSpecimen => ValidationResult[ShipmentSpecimen]
    ): ValidationResult[Shipment] = {
    if (shipmentSpecimenData.isEmpty) {
      Error(s"shipmentSpecimenData is empty").invalidNec
    } else {
      (shipmentRepository.exists(ShipmentId(event.shipmentId)),
       shipmentSpecimenData
         .map { info =>
           validShipmentSpecimen(info.getShipmentSpecimenId, info.getVersion)
             .andThen(ss => stateUpdateFn(ss))
         }.toList.sequence)
        .mapN { (shipment, shipmentSpecimens) =>
          shipmentSpecimens.foreach(shipmentSpecimenRepository.put)
          shipment
        }
    }
  }

  private def applySpecimenPresentEvent(event: ShipmentSpecimenEvent) =
    onValidSpecimenEvent(event, event.eventType.isPresent) { (shipment, _, time) =>
      specimenStateUpdate(event, event.getPresent.shipmentSpecimenData, time) { shipmentSpecimen =>
        shipmentSpecimen.present()
      }
    }

  private def applySpecimenReceivedEvent(event: ShipmentSpecimenEvent) = {
    onValidSpecimenEvent(event, event.eventType.isReceived) { (shipment, _, time) =>
      specimenStateUpdate(event, event.getReceived.shipmentSpecimenData, time) { shipmentSpecimen =>
        shipmentSpecimen.received()
      }

    }
  }

  private def applySpecimenMissingEvent(event: ShipmentSpecimenEvent) =
    onValidSpecimenEvent(event, event.eventType.isMissing) { (shipment, _, time) =>
      specimenStateUpdate(event, event.getMissing.shipmentSpecimenData, time) { shipmentSpecimen =>
        shipmentSpecimen.missing()
      }
    }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def applySpecimenExtraEvent(event: ShipmentSpecimenEvent): ValidationResult[Shipment] = {
    if (!event.eventType.isExtra) {
      Error(s"invalid event type: $event").invalidNec
    } else {
      val companionEvent = event.getExtra
      val eventTime      = OffsetDateTime.parse(event.getTime)
      val shipmentId     = ShipmentId(event.shipmentId)

      (shipmentRepository.exists(ShipmentId(event.shipmentId)),
       companionEvent.shipmentSpecimenData
         .map { info =>
           ShipmentSpecimen
             .create(id                  = ShipmentSpecimenId(info.getShipmentSpecimenId),
                     version             = 0L,
                     timeAdded           = eventTime,
                     shipmentId          = ShipmentId(shipmentId.id),
                     specimenId          = SpecimenId(info.getSpecimenId),
                     state               = ShipmentSpecimen.extraState,
                     shipmentContainerId = None)
         }
         .toList.sequence).mapN { (shipment, shipmentSpecimens) =>
        shipmentSpecimens.foreach(shipmentSpecimenRepository.put)
        shipment
      }
    }
  }
  // onValidSpecimenEvent(event, event.eventType.isExtra) { (shipment, _, time) =>
  //   val extraEvent = event.getExtra
  //   val eventTime  = OffsetDateTime.parse(event.getTime)
  //   extraEvent.shipmentSpecimenData.foreach { info =>
  //     val add = ShipmentSpecimen.create(id = ShipmentSpecimenId(info.getShipmentSpecimenId),
  //                                       version             = 0L,
  //                                       shipmentId          = ShipmentId(shipment.id.id),
  //                                       specimenId          = SpecimenId(info.getSpecimenId),
  //                                       state               = ShipmentSpecimen.extraState,
  //                                       shipmentContainerId = None)
  //     add.foreach { s =>
  //       shipmentSpecimenRepository.put(s.copy(timeAdded = eventTime))
  //     }
  //   }
  //   ().validNec
  // }

  private def processUpdateCmd[T <: ShipmentModifyCommand](
      cmd:        T,
      cmdToEvent: (T, Shipment) => ValidationResult[ShipmentEvent],
      applyEvent: ShipmentEvent => ValidationResult[Shipment]
    ): Unit = {
    val event = validShipment(cmd.id, cmd.expectedVersion).andThen(cmdToEvent(cmd, _))
    processWithResultCats(event)(applyEvent)
  }

  private def processUpdateCmdOnCreated[T <: ShipmentModifyCommand](
      cmd:        T,
      cmdToEvent: (T, CreatedShipment) => ValidationResult[ShipmentEvent],
      applyEvent: ShipmentEvent => ValidationResult[Shipment]
    ): Unit = {

    def internal(cmd: T, shipment: Shipment): ValidationResult[ShipmentEvent] =
      shipment match {
        case s: CreatedShipment => cmdToEvent(cmd, s)
        case s => InvalidState(s"shipment not created: ${shipment.id}").invalidNec
      }

    processUpdateCmd(cmd, internal, applyEvent)
  }

  private def processSpecimensCmd[T <: ShipmentSpecimenModifyCommand](
      cmd:        T,
      cmdToEvent: (T, Shipment) => ValidationResult[ShipmentSpecimenEvent],
      applyEvent: ShipmentSpecimenEvent => ValidationResult[Shipment]
    ): Unit = {
    val event = shipmentRepository
      .exists(ShipmentId(cmd.shipmentId))
      .andThen(_.isCreatedOrUnpacked)
      .andThen(cmdToEvent(cmd, _))

    processWithResultCats(event)(applyEvent)
  }

  private def processSpecimenUpdateCmd[T <: ShipmentSpecimenModifyCommand](
      cmd:        T,
      cmdToEvent: (T, Shipment) => ValidationResult[ShipmentSpecimenEvent],
      applyEvent: ShipmentSpecimenEvent => ValidationResult[Shipment]
    ): Unit = {
    val event = shipmentRepository
      .exists(ShipmentId(cmd.shipmentId))
      .andThen(cmdToEvent(cmd, _))

    processWithResultCats(event)(applyEvent)
  }

  private def onValidEventAndVersion(
      event:        ShipmentEvent,
      eventType:    Boolean,
      eventVersion: Long
    )(applyEvent:   (Shipment, ShipmentEvent, OffsetDateTime) => ValidationResult[Shipment]
    ): ValidationResult[Shipment] = {
    if (!eventType) {
      Error(s"invalid event type: $event").invalidNec
    } else {
      validShipment(event.id, eventVersion).andThen { shipment =>
        applyEvent(shipment, event, OffsetDateTime.parse(event.getTime))
      }
    }
  }

  private def onValidSpecimenEvent(
      event:      ShipmentSpecimenEvent,
      eventType:  Boolean
    )(applyEvent: (Shipment, ShipmentSpecimenEvent, OffsetDateTime) => ValidationResult[Shipment]
    ): ValidationResult[Shipment] = {
    if (!eventType) {
      Error(s"invalid event type: $event").invalidNec
    } else {
      shipmentRepository.exists(ShipmentId(event.shipmentId)).andThen { shipment =>
        applyEvent(shipment, event, OffsetDateTime.parse(event.getTime))
      }
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def createShipmentSpecimens(
      shipment:            Shipment,
      shipmentContainerId: Option[ShipmentContainerId],
      specimens:           List[Specimen]
    ): ValidationResult[Seq[ShipmentSpecimen]] = {
    specimensAtCentre(shipment.originLocationId, specimens: _*).andThen { x =>
      specimens
        .map { specimen =>
          validNewIdentityCats(shipmentSpecimenRepository.nextIdentity, shipmentSpecimenRepository)
            .andThen { id =>
              ShipmentSpecimen.create(id                  = id,
                                      version             = 0L,
                                      shipmentId          = shipment.id,
                                      specimenId          = specimen.id,
                                      state               = ShipmentSpecimen.presentState,
                                      shipmentContainerId = shipmentContainerId)
            }
        }.sequence.map(_.toList)
    }
  }

  private def trakingNumberExistsError(trackingNumber: String): EntityCriteriaError =
    EntityCriteriaError(s"shipment with tracking number already exists: $trackingNumber")

  private def trackingNumberAvailable(trackingNumber: String): ValidationResult[Unit] = {
    shipmentRepository.find(_.trackingNumber == trackingNumber) match {
      case Some(_) => trakingNumberExistsError(trackingNumber).invalidNec
      case None    => ().validNec
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  private def trackingNumberAvailable(
      trackingNumber: String,
      excludeId:      ShipmentId
    ): ValidationResult[Unit] = {
    shipmentRepository.find(c => (c.trackingNumber == trackingNumber) && (c.id != excludeId)) match {
      case Some(_) => trakingNumberExistsError(trackingNumber).invalidNec
      case None    => ().validNec
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  protected def specimensAtCentre(
      locationId: LocationId,
      specimens:  Specimen*
    ): ValidationResult[List[Specimen]] = {
    specimens
      .map { specimen =>
        if (locationId == specimen.locationId) specimen.validNec
        else specimen.inventoryId.invalidNec
      }.toList.sequence.leftMap(
        err =>
          NonEmptyChain(
            EntityCriteriaError("invalid centre for specimen inventory IDs:" + err.toList.mkString(", "))
          )
      )
  }

  /**
   *
   * @param shipSpecimenMap map of inventory ID to Shipment Specimen.
   *
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def getShipmentSpecimens(
      shipmentId: ShipmentId,
      specimens:  List[Specimen]
    ): ValidationResult[List[(String, ShipmentSpecimen)]] =
    specimens
      .map { specimen =>
        shipmentSpecimenRepository
          .getBySpecimen(shipmentId, specimen).map { shipSpecimen =>
            (specimen.inventoryId -> shipSpecimen)
          }.leftMap(err => NonEmptyChain(specimen.inventoryId))
      }.toList.sequence.leftMap(
        err =>
          NonEmptyChain(EntityCriteriaError("specimens not in this shipment: " + err.toList.mkString(",")))
      )

  /**
   * Checks that a specimen is not present in any shipment.
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  protected def specimensNotPresentInShipment(specimens: Specimen*): ValidationResult[List[Specimen]] =
    specimens
      .map { specimen =>
        val present = shipmentSpecimenRepository.allForSpecimen(specimen.id).filter { ss =>
          ss.state == ShipmentSpecimen.presentState
        }

        if (present.isEmpty) specimen.validNec
        else specimen.inventoryId.invalidNec
      }.toList.sequence.leftMap(
        err =>
          NonEmptyChain(
            EntityCriteriaError(s"specimens are already in an active shipment: " + err.toList.mkString(", "))
          )
      )

  /**
   * Checks that a specimen is not present in a shipment.
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def specimensNotInShipment(
      shipmentId: ShipmentId,
      specimens:  Specimen*
    ): ValidationResult[List[Specimen]] =
    specimens
      .map { specimen =>
        shipmentSpecimenRepository
          .getBySpecimen(shipmentId, specimen).fold(err => specimen.validNec,
                                                    _ => specimen.inventoryId.invalidNec)
      }.toList.sequence.leftMap(
        err =>
          NonEmptyChain(
            EntityCriteriaError(
              s"specimen inventory IDs already in this shipment: " + err.toList.mkString(", ")
            )
          )
      )

  /**
   *
   * @param shipSpecimenMap map of inventory ID to Shipment Specimen.
   *
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def getPackedShipmentSpecimens(
      shipSpecimenMap: List[(String, ShipmentSpecimen)]
    ): ValidationResult[List[ShipmentSpecimen]] =
    shipSpecimenMap
      .map {
        case (inventoryId, shipSpecimen) =>
          shipSpecimen.isStatePresent
            .map { _ =>
              shipSpecimen
            }.leftMap(err => NonEmptyChain(inventoryId))
      }.toList.sequence
      .leftMap(
        err =>
          NonEmptyChain(EntityCriteriaError("shipment specimens not present: " + err.toList.mkString(",")))
      )

  /**
   *
   * @param shipSpecimenMap map of inventory ID to Shipment Specimen.
   *
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def getNonPackedShipmentSpecimens(
      shipSpecimenMap: List[(String, ShipmentSpecimen)]
    ): ValidationResult[List[ShipmentSpecimen]] =
    shipSpecimenMap
      .map {
        case (inventoryId, shipSpecimen) =>
          shipSpecimen.isStateNotPresent
            .map { _ =>
              shipSpecimen
            }.leftMap(err => NonEmptyChain(inventoryId))
      }.toList.sequence
      .leftMap(
        err =>
          NonEmptyChain(EntityCriteriaError("shipment specimens are present: " + err.toList.mkString(",")))
      )

  private def shipmentSpecimensPresent(
      shipmentId:           ShipmentId,
      specimenInventoryIds: String*
    ): ValidationResult[List[ShipmentSpecimen]] = {
    getSpecimens(specimenInventoryIds: _*)
      .andThen { specimens =>
        getShipmentSpecimens(shipmentId, specimens)
      }
      .andThen { shipSpecimens =>
        getPackedShipmentSpecimens(shipSpecimens)
      }
  }

  private def shipmentSpecimensNotPresent(
      shipmentId:           ShipmentId,
      specimenInventoryIds: String*
    ): ValidationResult[List[ShipmentSpecimen]] = {
    getSpecimens(specimenInventoryIds: _*)
      .andThen { specimens =>
        getShipmentSpecimens(shipmentId, specimens)
      }
      .andThen { shipSpecimens =>
        getNonPackedShipmentSpecimens(shipSpecimens)
      }
  }

  private def init(): Unit = {
    shipmentRepository.init
    shipmentSpecimenRepository.init
  }

  init

}
