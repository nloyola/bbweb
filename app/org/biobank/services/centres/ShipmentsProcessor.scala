package org.biobank.services.centres

import akka.actor._
import akka.persistence.{RecoveryCompleted, SaveSnapshotFailure, SaveSnapshotSuccess, SnapshotOffer}
//import com.github.ghik.silencer.silent
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
import org.biobank.services.{Processor, ServiceError, ServiceValidation, SnapshotWriter}
import org.biobank.services.participants.SpecimensService
import play.api.libs.json._
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._
import scalaz._

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
  import org.biobank.CommonValidations._

  override def persistenceId: String = "shipments-processor-id"

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  private var replyTo: Option[ActorRef] = None

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  val receiveRecover: Receive = {
    case event: ShipmentEvent =>
      log.debug(s"ShipmentsProcessor: receiveRecover: $event")

      val result = event.eventType match {
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

        case event => ServiceError(s"event not handled: $event").failureNel[Shipment]
      }

      result match {
        case Failure(err) => log.error(err.toString)
        case _            =>
      }

    case event: ShipmentSpecimenEvent =>
      log.debug(s"ShipmentsProcessor: $event")

      val result = event.eventType match {
        case et: ShipmentSpecimenEvent.EventType.Added            => applySpecimensAddedEvent(event)
        case et: ShipmentSpecimenEvent.EventType.Removed          => applySpecimenRemovedEvent(event)
        case et: ShipmentSpecimenEvent.EventType.ContainerUpdated => applySpecimenContainerUpdatedEvent(event)
        case et: ShipmentSpecimenEvent.EventType.Present          => applySpecimenPresentEvent(event)
        case et: ShipmentSpecimenEvent.EventType.Received         => applySpecimenReceivedEvent(event)
        case et: ShipmentSpecimenEvent.EventType.Missing          => applySpecimenMissingEvent(event)
        case et: ShipmentSpecimenEvent.EventType.Extra            => applySpecimenExtraEvent(event)

        case event => ServiceError(s"event not handled: $event").failureNel[Shipment]
      }

      result match {
        case Failure(err) => log.error(err.toString)
        case _            =>
      }

    case SnapshotOffer(_, snapshotFilename: String) =>
      applySnapshot(snapshotFilename)

    case RecoveryCompleted =>
      log.debug(s"ShipmentsProcessor: recovery completed")
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any", "org.wartremover.warts.Throw"))
  val receiveCommand: Receive = {

    case cmd: AddShipmentCmd =>
      processWithResult(addCmdToEvent(cmd))(applyAddedEvent)

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

    case cmd: ShipmentSpecimenUpdateContainerCmd =>
      processSpecimensCmd(cmd, updateSpecimenContainerCmdToEvent, applySpecimenContainerUpdatedEvent)

    case cmd: ShipmentSpecimenExtraCmd =>
      processSpecimensCmd(cmd, specimenExtraCmdToEvent, applySpecimenExtraEvent)

    case cmd: ShipmentAddSpecimensCmd =>
      processWithResult(addSpecimenCmdToEvent(cmd))(applySpecimensAddedEvent)

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

  private def addCmdToEvent(cmd: AddShipmentCmd) =
    for {
      id                <- validNewIdentity(shipmentRepository.nextIdentity, shipmentRepository)
      originCentre      <- centreRepository.getByLocationId(LocationId(cmd.originLocationId))
      destinationCentre <- centreRepository.getByLocationId(LocationId(cmd.destinationLocationId))
      trackingNo        <- trackingNumberAvailable(cmd.trackingNumber)
      shipment <- CreatedShipment.create(id = id,
                                         version               = 0L,
                                         timeAdded             = OffsetDateTime.now,
                                         courierName           = cmd.courierName,
                                         trackingNumber        = cmd.trackingNumber,
                                         originCentreId        = originCentre.id,
                                         originLocationId      = LocationId(cmd.originLocationId),
                                         destinationCentreId   = destinationCentre.id,
                                         destinationLocationId = LocationId(cmd.destinationLocationId))
    } yield ShipmentEvent(id.id).update(_.sessionUserId := cmd.sessionUserId,
                                        _.time := OffsetDateTime.now
                                          .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                        _.added.courierName           := shipment.courierName,
                                        _.added.trackingNumber        := shipment.trackingNumber,
                                        _.added.originCentreId        := shipment.originCentreId.id,
                                        _.added.originLocationId      := shipment.originLocationId.id,
                                        _.added.destinationCentreId   := shipment.destinationCentreId.id,
                                        _.added.destinationLocationId := shipment.destinationLocationId.id)

  private def updateCourierNameCmdToEvent(
      cmd:      UpdateShipmentCourierNameCmd,
      shipment: CreatedShipment
    ): ServiceValidation[ShipmentEvent] =
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
    ): ServiceValidation[ShipmentEvent] =
    for {
      trackingNo <- trackingNumberAvailable(cmd.trackingNumber, shipment.id)
      updated    <- shipment.withTrackingNumber(cmd.trackingNumber)
    } yield ShipmentEvent(shipment.id.id)
      .update(_.sessionUserId := cmd.sessionUserId,
              _.time := OffsetDateTime.now
                .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.trackingNumberUpdated.version        := cmd.expectedVersion,
              _.trackingNumberUpdated.trackingNumber := cmd.trackingNumber)

  private def updateoriginLocationCmdToEvent(
      cmd:      UpdateShipmentOriginCmd,
      shipment: CreatedShipment
    ): ServiceValidation[ShipmentEvent] =
    for {
      centre      <- centreRepository.getByLocationId(LocationId(cmd.locationId))
      location    <- centre.locationWithId(LocationId(cmd.locationId))
      newShipment <- shipment.withOrigin(centre.id, location.id)
    } yield ShipmentEvent(shipment.id.id).update(_.sessionUserId := cmd.sessionUserId,
                                                 _.time := OffsetDateTime.now
                                                   .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                                 _.originLocationUpdated.version    := cmd.expectedVersion,
                                                 _.originLocationUpdated.centreId   := centre.id.id,
                                                 _.originLocationUpdated.locationId := cmd.locationId)

  private def updatedestinationLocationCmdToEvent(
      cmd:      UpdateShipmentDestinationCmd,
      shipment: CreatedShipment
    ): ServiceValidation[ShipmentEvent] =
    for {
      centre      <- centreRepository.getByLocationId(LocationId(cmd.locationId))
      location    <- centre.locationWithId(LocationId(cmd.locationId))
      newShipment <- shipment.withDestination(centre.id, location.id)
    } yield ShipmentEvent(shipment.id.id).update(_.sessionUserId := cmd.sessionUserId,
                                                 _.time := OffsetDateTime.now
                                                   .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                                 _.destinationLocationUpdated.version    := cmd.expectedVersion,
                                                 _.destinationLocationUpdated.centreId   := centre.id.id,
                                                 _.destinationLocationUpdated.locationId := cmd.locationId)

  private def createdCmdToEvent(
      cmd:      CreatedShipmentCmd,
      shipment: Shipment
    ): ServiceValidation[ShipmentEvent] =
    shipment.isPacked.fold(
      err => InvalidState(s"shipment is not packed: ${shipment.id}").failureNel[ShipmentEvent],
      s =>
        ShipmentEvent(shipment.id.id)
          .update(_.sessionUserId   := cmd.sessionUserId,
                  _.time            := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                  _.created.version := cmd.expectedVersion).successNel[String]
    )

  private def packCmdToEvent(cmd: PackShipmentCmd, shipment: Shipment): ServiceValidation[ShipmentEvent] = {
    val numPresentSpecimens =
      shipmentSpecimenRepository.shipmentSpecimenCount(shipment.id, ShipmentItemState.Present)
    if (numPresentSpecimens <= 0) {
      InvalidState(s"shipment has no specimens: ${shipment.id}").failureNel[ShipmentEvent]
    } else {
      shipment match {
        case _: CreatedShipment | _: SentShipment =>
          ShipmentEvent(shipment.id.id)
            .update(_.sessionUserId  := cmd.sessionUserId,
                    _.time           := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                    _.packed.version := cmd.expectedVersion,
                    _.packed.stateChangeTime := cmd.datetime
                      .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)).successNel[String]
        case _ =>
          InvalidState(s"cannot change to packed state: ${shipment.id}").failureNel[ShipmentEvent]
      }
    }
  }

  private def sendCmdToEvent(cmd: SendShipmentCmd, shipment: Shipment): ServiceValidation[ShipmentEvent] = {
    val valid = shipment match {
      case ps: PackedShipment   => ps.send(cmd.datetime)
      case rs: ReceivedShipment => rs.backToSent.successNel[String]
      case ls: LostShipment     => ls.backToSent.successNel[String]
      case _ =>
        InvalidState(s"cannot change to sent state: ${shipment.id}").failureNel[Shipment]
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
    ): ServiceValidation[ShipmentEvent] = {
    val valid = shipment match {
      case ss: SentShipment =>
        ss.receive(cmd.datetime)
      case us: UnpackedShipment =>
        // all items must be in present state to allow this state transition
        val nonPresentExist = shipmentSpecimenRepository.forShipment(us.id).exists { ss =>
          ss.state != ShipmentItemState.Present
        }
        if (nonPresentExist)
          InvalidState(s"cannot change to received state, items have already been processed: ${us.id}")
            .failureNel[Shipment]
        else
          us.backToReceived.successNel[String]
      case _ =>
        InvalidState(s"cannot change to received state: ${shipment.id}").failureNel[Shipment]
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
    ): ServiceValidation[ShipmentEvent] = {
    val valid = shipment match {
      case rs: ReceivedShipment  => rs.unpack(cmd.datetime)
      case cs: CompletedShipment => cs.backToUnpacked.successNel[String]
      case _ =>
        InvalidState(s"cannot change to unpacked state: ${shipment.id}").failureNel[Shipment]
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
    ): ServiceValidation[ShipmentEvent] = {
    val numPresentSpecimens =
      shipmentSpecimenRepository.shipmentSpecimenCount(shipment.id, ShipmentItemState.Present)
    if (numPresentSpecimens > 0) {
      InvalidState(s"shipment has specimens in present state: ${shipment.id}").failureNel[ShipmentEvent]
    } else {
      val valid = shipment match {
        case us: UnpackedShipment => us.complete(cmd.datetime)
        case _ =>
          InvalidState(s"cannot change to completed state: ${shipment.id}").failureNel[Shipment]
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

  private def lostCmdToEvent(cmd: LostShipmentCmd, shipment: Shipment): ServiceValidation[ShipmentEvent] =
    shipment.isSent.fold(
      err => InvalidState(s"cannot change to lost state: ${shipment.id}").failureNel[ShipmentEvent],
      s =>
        ShipmentEvent(shipment.id.id)
          .update(_.sessionUserId := cmd.sessionUserId,
                  _.time          := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                  _.lost.version  := cmd.expectedVersion).successNel[String]
    )

  private def skipStateToSentCmdToEvent(
      cmd:      ShipmentSkipStateToSentCmd,
      shipment: CreatedShipment
    ): ServiceValidation[ShipmentEvent] =
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
    ): ServiceValidation[ShipmentEvent] =
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
        InvalidState(s"shipment not sent: ${shipment.state}").failureNel[ShipmentEvent]
    }

  private def removeCmdToEvent(
      cmd:      ShipmentRemoveCmd,
      shipment: CreatedShipment
    ): ServiceValidation[ShipmentEvent] = {
    val shipmentId = ShipmentId(cmd.id)
    for {
      shipment  <- shipmentRepository.getByKey(shipmentId)
      isCreated <- shipment.isCreated
      hasSpecimens <- {
        if (shipmentSpecimenRepository.forShipment(shipmentId).isEmpty) ().successNel[String]
        else ServiceError(s"shipment has specimens, remove specimens first").failureNel[Unit]
      }
    } yield ShipmentEvent(shipment.id.id).update(
      _.sessionUserId   := cmd.sessionUserId,
      _.time            := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
      _.removed.version := cmd.expectedVersion
    )
  }

  private def addSpecimenCmdToEvent(
      cmd: ShipmentAddSpecimensCmd
    ): ServiceValidation[ShipmentSpecimenEvent] = {
    val sessionUserId       = UserId(cmd.sessionUserId)
    val shipmentId          = ShipmentId(cmd.shipmentId)
    val shipmentContainerId = cmd.shipmentContainerId.map(ShipmentContainerId.apply)

    for {
      shipment          <- shipmentRepository.getCreated(shipmentId)
      specimens         <- specimensService.getByInventoryIds(sessionUserId, cmd.specimenInventoryIds: _*)
      shipmentSpecimens <- createShipmentSpecimens(shipment, shipmentContainerId, specimens)
    } yield {
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

  private def validShipmentSpecimen(
      shipmentSpecimenId: String,
      expectedVersion:    Long
    ): ServiceValidation[ShipmentSpecimen] =
    for {
      shipmentSpecimen <- shipmentSpecimenRepository.getByKey(ShipmentSpecimenId(shipmentSpecimenId))
      validVersion     <- shipmentSpecimen.requireVersion(expectedVersion)
    } yield shipmentSpecimen

  private def removeSpecimenCmdToEvent(
      cmd:      ShipmentSpecimenRemoveCmd,
      shipment: Shipment
    ): ServiceValidation[ShipmentSpecimenEvent] = {
    val validateShipmentSpecimen =
      shipmentSpecimenRepository.getByKey(ShipmentSpecimenId(cmd.shipmentSpecimenId)).flatMap {
        shipmentSpecimen =>
          if (shipmentSpecimen.state == ShipmentItemState.Present) {
            for {
              shipment  <- shipmentRepository.getCreated(ShipmentId(cmd.shipmentId))
              isPresent <- shipmentSpecimen.isStatePresent
            } yield shipmentSpecimen

          } else if (shipmentSpecimen.state == ShipmentItemState.Extra) {
            for {
              shipment <- shipmentRepository.getUnpacked(ShipmentId(cmd.shipmentId))
              isExtra  <- shipmentSpecimen.isStateExtra
            } yield shipmentSpecimen
          } else {
            EntityCriteriaError(
              s"cannot remove, shipment specimen state is invalid: ${shipmentSpecimen.state}"
            ).failureNel[ShipmentSpecimen]
          }
      }

    for {
      shipmentSpecimen <- validateShipmentSpecimen
      validVersion     <- validShipmentSpecimen(cmd.shipmentSpecimenId, cmd.expectedVersion)
    } yield ShipmentSpecimenEvent(shipment.id.id).update(_.sessionUserId := cmd.sessionUserId,
                                                         _.time := OffsetDateTime.now
                                                           .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                                         _.removed.version            := cmd.expectedVersion,
                                                         _.removed.shipmentSpecimenId := cmd.shipmentSpecimenId)
  }

  private def updateSpecimenContainerCmdToEvent(
      cmd:      ShipmentSpecimenUpdateContainerCmd,
      shipment: Shipment
    ): ServiceValidation[ShipmentSpecimenEvent] =
    // FIXME: validate that shipmentContainerId is a container in the repository
    //
    //val shipmentContainerId = cmd.shipmentContainerId.map(ShipmentContainerId.apply)
    for {
      shipment          <- shipmentRepository.getCreated(ShipmentId(cmd.shipmentId))
      shipmentSpecimens <- shipmentSpecimensPresent(shipment.id, cmd.specimenInventoryIds: _*)
      container         <- s"shipping specimens with containers has not been implemented yet".failureNel[Unit]
    } yield {
      val shipmentSpecimenData = shipmentSpecimens.map(EventUtils.shipmentSpecimenInfoToEvent).toSeq
      ShipmentSpecimenEvent(cmd.shipmentId).update(_.sessionUserId := cmd.sessionUserId,
                                                   _.time := OffsetDateTime.now
                                                     .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                                   _.containerUpdated.optionalShipmentContainerId := cmd.shipmentContainerId,
                                                   _.containerUpdated.shipmentSpecimenData        := shipmentSpecimenData)
    }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def presentSpecimensCmdToEvent(
      cmd:      ShipmentSpecimensPresentCmd,
      shipment: Shipment
    ): ServiceValidation[ShipmentSpecimenEvent] =
    for {
      isUnpacked        <- shipment.isUnpacked
      shipmentSpecimens <- shipmentSpecimensNotPresent(shipment.id, cmd.specimenInventoryIds: _*)
      canMakePresent    <- shipmentSpecimens.map(_.present).sequenceU
    } yield {
      val shipmentSpecimenData = shipmentSpecimens.map(EventUtils.shipmentSpecimenInfoToEvent).toSeq
      ShipmentSpecimenEvent(cmd.shipmentId).update(_.sessionUserId := cmd.sessionUserId,
                                                   _.time := OffsetDateTime.now
                                                     .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                                   _.present.shipmentSpecimenData := shipmentSpecimenData)
    }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def receiveSpecimensCmdToEvent(
      cmd:      ShipmentSpecimensReceiveCmd,
      shipment: Shipment
    ): ServiceValidation[ShipmentSpecimenEvent] =
    for {
      isUnpacked        <- shipment.isUnpacked
      shipmentSpecimens <- shipmentSpecimensPresent(shipment.id, cmd.specimenInventoryIds: _*)
      canReceive        <- shipmentSpecimens.map(_.received).sequenceU
    } yield {
      val shipmentSpecimenData = shipmentSpecimens.map(EventUtils.shipmentSpecimenInfoToEvent).toSeq
      ShipmentSpecimenEvent(cmd.shipmentId).update(_.sessionUserId := cmd.sessionUserId,
                                                   _.time := OffsetDateTime.now
                                                     .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                                   _.received.shipmentSpecimenData := shipmentSpecimenData)
    }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def specimenMissingCmdToEvent(
      cmd:      ShipmentSpecimenMissingCmd,
      shipment: Shipment
    ): ServiceValidation[ShipmentSpecimenEvent] =
    for {
      isUnpacked        <- shipment.isUnpacked
      shipmentSpecimens <- shipmentSpecimensPresent(shipment.id, cmd.specimenInventoryIds: _*)
      canMakeMissing    <- shipmentSpecimens.map(_.missing).sequenceU
    } yield {
      val shipmentSpecimenData = shipmentSpecimens.map(EventUtils.shipmentSpecimenInfoToEvent).toSeq
      ShipmentSpecimenEvent(cmd.shipmentId).update(_.sessionUserId := cmd.sessionUserId,
                                                   _.time := OffsetDateTime.now
                                                     .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                                   _.missing.shipmentSpecimenData := shipmentSpecimenData)
    }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def getSpecimens(specimenInventoryIds: String*): ServiceValidation[List[Specimen]] =
    specimenInventoryIds
      .map { inventoryId =>
        specimenRepository.getByInventoryId(inventoryId).leftMap(err => NonEmptyList(inventoryId))
      }.toList.sequenceU.leftMap(
        err => EntityCriteriaError("invalid inventory Ids: " + err.list.toList.mkString(", ")).nel
      )

  /**
   * Specimens that were not recorded to be in this shipment were actually received in the shipment.
   *
   * The specimens must be moved to this centre if and only if they are already at the centre the shipment is
   * coming from.
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def specimenExtraCmdToEvent(
      cmd:      ShipmentSpecimenExtraCmd,
      shipment: Shipment
    ): ServiceValidation[ShipmentSpecimenEvent] = {
    val sessionUserId = UserId(cmd.sessionUserId)
    for {
      isUnpacked        <- shipment.isUnpacked
      specimens         <- getSpecimens(cmd.specimenInventoryIds: _*)
      notInThisShipment <- specimensNotInShipment(shipment.id, specimens: _*)
      notInAnyShipments <- specimensNotPresentInShipment(specimens: _*)
      specimens         <- specimensService.getByInventoryIds(sessionUserId, cmd.specimenInventoryIds: _*)
      shipmentSpecimens <- createShipmentSpecimens(shipment, None, specimens)
      canMakeExtra      <- shipmentSpecimens.map(_.extra).toList.sequenceU
    } yield {
      val shipmentSpecimenData = shipmentSpecimens.map { ss =>
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

  private def applyAddedEvent(event: ShipmentEvent): ServiceValidation[Shipment] = {
    if (!event.eventType.isAdded) {
      ServiceError(s"invalid event type: $event").failureNel[Shipment]
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
      add.foreach(s => shipmentRepository.put(s.copy(timeAdded = eventTime)))
      add
    }
  }

  private def applyCourierNameUpdatedEvent(event: ShipmentEvent) = {
    onValidEventAndVersion(event,
                           event.eventType.isCourierNameUpdated,
                           event.getCourierNameUpdated.getVersion) { (shipment, _, time) =>
      val v = for {
        created <- shipment.isCreated
        updated <- created.withCourier(event.getCourierNameUpdated.getCourierName)
      } yield updated.copy(timeModified = Some(time))
      v.foreach(shipmentRepository.put)
      v
    }
  }

  private def applyTrackingNumberUpdatedEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event,
                           event.eventType.isTrackingNumberUpdated,
                           event.getTrackingNumberUpdated.getVersion) { (shipment, _, time) =>
      val v = for {
        created <- shipment.isCreated
        updated <- created.withTrackingNumber(event.getTrackingNumberUpdated.getTrackingNumber)
      } yield updated.copy(timeModified = Some(time))
      v.foreach(shipmentRepository.put)
      v
    }

  private def applyOriginLocationUpdatedEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event,
                           event.eventType.isOriginLocationUpdated,
                           event.getOriginLocationUpdated.getVersion) { (shipment, _, time) =>
      val centreId   = CentreId(event.getOriginLocationUpdated.getCentreId)
      val locationId = LocationId(event.getOriginLocationUpdated.getLocationId)
      val v = for {
        created <- shipment.isCreated
        updated <- created.withOrigin(centreId, locationId)
      } yield updated.copy(timeModified = Some(time))
      v.foreach(shipmentRepository.put)
      v
    }

  private def applyDestinationLocationUpdatedEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event,
                           event.eventType.isDestinationLocationUpdated,
                           event.getDestinationLocationUpdated.getVersion) { (shipment, _, time) =>
      val centreId   = CentreId(event.getDestinationLocationUpdated.getCentreId)
      val locationId = LocationId(event.getDestinationLocationUpdated.getLocationId)
      val v = for {
        created <- shipment.isCreated
        updated <- created.withDestination(centreId, locationId)
      } yield updated.copy(timeModified = Some(time))
      v.foreach(shipmentRepository.put)
      v
    }

  private def applyCreatedEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event, event.eventType.isCreated, event.getCreated.getVersion) {
      (shipment, _, time) =>
        shipment.isPacked.map { p =>
          val created = p.created.copy(timeModified = Some(time))
          shipmentRepository.put(created)
          created
        }
    }

  private def applyPackedEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event, event.eventType.isPacked, event.getPacked.getVersion) {
      (shipment, _, time) =>
        val stateChangeTime = OffsetDateTime.parse(event.getPacked.getStateChangeTime)

        val packed = shipment match {
          case created: CreatedShipment => created.pack(stateChangeTime).successNel[String]
          case sent:    SentShipment    => sent.backToPacked.successNel[String]
          case _ => InvalidState(s"cannot change to packed state: ${shipment.id}").failureNel[PackedShipment]
        }

        packed.map { s =>
          val updated = s.copy(timeModified = Some(time))
          shipmentRepository.put(updated)
          updated
        }
    }

  private def applySentEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event, event.eventType.isSent, event.getSent.getVersion) { (shipment, _, time) =>
      val stateChangeTime = OffsetDateTime.parse(event.getSent.getStateChangeTime)
      val sent = shipment match {
        case packed:   PackedShipment   => packed.send(stateChangeTime)
        case received: ReceivedShipment => received.backToSent.successNel[String]
        case lost:     LostShipment     => lost.backToSent.successNel[String]
        case _ => InvalidState(s"cannot change to sent state: ${shipment.id}").failureNel[SentShipment]
      }

      sent.map { s =>
        val updated = s.copy(timeModified = Some(time))
        shipmentRepository.put(updated)
        updated
      }
    }

  private def applyReceivedEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event, event.eventType.isReceived, event.getReceived.getVersion) {
      (shipment, _, time) =>
        val stateChangeTime = OffsetDateTime.parse(event.getReceived.getStateChangeTime)
        val received = shipment match {
          case sent:     SentShipment     => sent.receive(stateChangeTime)
          case unpacked: UnpackedShipment => unpacked.backToReceived.successNel[String]
          case _ =>
            InvalidState(s"cannot change to received state: ${shipment.id}").failureNel[ReceivedShipment]
        }

        received.map { s =>
          val updated = s.copy(timeModified = Some(time))
          shipmentRepository.put(updated)
          updated
        }
    }

  private def applyUnpackedEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event, event.eventType.isUnpacked, event.getUnpacked.getVersion) {
      (shipment, _, time) =>
        val stateChangeTime = OffsetDateTime.parse(event.getUnpacked.getStateChangeTime)

        val unpacked = shipment match {
          case received:  ReceivedShipment  => received.unpack(stateChangeTime)
          case completed: CompletedShipment => completed.backToUnpacked.successNel[String]
          case _ =>
            InvalidState(s"cannot change to received state: ${shipment.id}").failureNel[UnpackedShipment]
        }

        unpacked.map { s =>
          val updated = s.copy(timeModified = Some(time))
          shipmentRepository.put(updated)
          updated
        }
    }

  private def applyCompletedEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event, event.eventType.isCompleted, event.getCompleted.getVersion) {
      (shipment, _, time) =>
        val stateChangeTime = OffsetDateTime.parse(event.getCompleted.getStateChangeTime)
        val completed = for {
          unpacked  <- shipment.isUnpacked
          completed <- unpacked.complete(stateChangeTime)
        } yield completed.copy(timeModified = Some(time))

        completed.foreach { s =>
          shipmentRepository.put(s)
        }
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
      val v = for {
        created <- shipment.isCreated
        updated <- created.skipToSent(timePacked, timeSent)
      } yield updated.copy(timeModified = Some(time))
      v.foreach(shipmentRepository.put)
      v
    }

  private def applySkippedToUnpackedStateEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event,
                           event.eventType.isSkippedToUnpackedState,
                           event.getSkippedToUnpackedState.getVersion) { (shipment, _, time) =>
      val timeReceived = OffsetDateTime.parse(event.getSkippedToUnpackedState.getTimeReceived)
      val timeUnpacked = OffsetDateTime.parse(event.getSkippedToUnpackedState.getTimeUnpacked)

      val v = for {
        sent    <- shipment.isSent
        updated <- sent.skipToUnpacked(timeReceived, timeUnpacked)
      } yield updated.copy(timeModified = Some(time))
      v.foreach(shipmentRepository.put)
      v
    }

  private def applyRemovedEvent(event: ShipmentEvent) =
    onValidEventAndVersion(event, event.eventType.isRemoved, event.getRemoved.getVersion) {
      (shipment, _, time) =>
        shipmentRepository.remove(shipment)
        shipment.successNel[String]
    }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def applySpecimensAddedEvent(event: ShipmentSpecimenEvent): ServiceValidation[Shipment] =
    if (!event.eventType.isAdded) {
      ServiceError(s"invalid event type: $event").failureNel[Shipment]
    } else {
      val addedEvent          = event.getAdded
      val eventTime           = OffsetDateTime.parse(event.getTime)
      val shipmentId          = ShipmentId(event.shipmentId)
      val shipmentContainerId = addedEvent.shipmentContainerId.map(ShipmentContainerId.apply)

      addedEvent.shipmentSpecimenAddData
        .map { info =>
          val created = ShipmentSpecimen.create(id = ShipmentSpecimenId(info.getShipmentSpecimenId),
                                                version             = 0L,
                                                shipmentId          = shipmentId,
                                                specimenId          = SpecimenId(info.getSpecimenId),
                                                state               = ShipmentItemState.Present,
                                                shipmentContainerId = shipmentContainerId)
          created.foreach(ss => shipmentSpecimenRepository.put(ss.copy(timeAdded = eventTime)))
          created
        }
        .toList.sequenceU
        .flatMap { _ =>
          shipmentRepository.getByKey(shipmentId)
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
  private def applySpecimenContainerUpdatedEvent(
      event: ShipmentSpecimenEvent
    ): ServiceValidation[Shipment] = {
    if (!event.eventType.isContainerUpdated) {
      ServiceError(s"invalid event type: $event").failureNel[Shipment]
    } else {
      val companionEvent      = event.getContainerUpdated
      val eventTime           = OffsetDateTime.parse(event.getTime)
      val shipmentId          = ShipmentId(event.shipmentId)
      val shipmentContainerId = companionEvent.shipmentContainerId.map(ShipmentContainerId.apply)

      companionEvent.shipmentSpecimenData
        .map { info =>
          for {
            shipmentSpecimen <- validShipmentSpecimen(info.getShipmentSpecimenId, info.getVersion)
            updated          <- shipmentSpecimen.withShipmentContainer(shipmentContainerId)
          } yield updated.copy(timeModified = Some(eventTime))
        }
        .toList.sequenceU
        .flatMap { shipmentSpecimens =>
          shipmentSpecimens.foreach(shipmentSpecimenRepository.put)
          shipmentRepository.getByKey(shipmentId)
        }
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def specimenStateUpdate(
      event:                ShipmentSpecimenEvent,
      shipmentSpecimenData: Seq[ShipmentSpecimenEvent.ShipmentSpecimenInfo],
      eventTime:            OffsetDateTime
    )(stateUpdateFn:        ShipmentSpecimen => ServiceValidation[ShipmentSpecimen]
    ): ServiceValidation[Shipment] =
    if (shipmentSpecimenData.isEmpty) {
      ServiceError(s"shipmentSpecimenData is empty").failureNel[Shipment]
    } else {
      val v = shipmentSpecimenData
        .map { info =>
          for {
            shipmentSpecimen <- validShipmentSpecimen(info.getShipmentSpecimenId, info.getVersion)
            updated          <- stateUpdateFn(shipmentSpecimen)
          } yield updated.copy(timeModified = Some(eventTime))
        }.toList.sequenceU

      v.flatMap { shipmentSpecimens =>
        shipmentSpecimens.foreach(shipmentSpecimenRepository.put)
        shipmentRepository.getByKey(ShipmentId(event.shipmentId))
      }
    }

  private def applySpecimenPresentEvent(event: ShipmentSpecimenEvent) =
    onValidSpecimenEvent(event, event.eventType.isPresent) { (shipment, _, time) =>
      specimenStateUpdate(event, event.getPresent.shipmentSpecimenData, time) { shipmentSpecimen =>
        shipmentSpecimen.present
      }
    }

  private def applySpecimenReceivedEvent(event: ShipmentSpecimenEvent) =
    onValidSpecimenEvent(event, event.eventType.isReceived) { (shipment, _, time) =>
      specimenStateUpdate(event, event.getReceived.shipmentSpecimenData, time) { shipmentSpecimen =>
        shipmentSpecimen.received
      }
    }

  private def applySpecimenMissingEvent(event: ShipmentSpecimenEvent) =
    onValidSpecimenEvent(event, event.eventType.isMissing) { (shipment, _, time) =>
      specimenStateUpdate(event, event.getMissing.shipmentSpecimenData, time) { shipmentSpecimen =>
        shipmentSpecimen.missing
      }
    }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def applySpecimenExtraEvent(event: ShipmentSpecimenEvent): ServiceValidation[Shipment] = {
    if (!event.eventType.isExtra) {
      ServiceError(s"invalid event type: $event").failureNel[Shipment]
    } else {
      val companionEvent = event.getExtra
      val eventTime      = OffsetDateTime.parse(event.getTime)
      val shipmentId     = ShipmentId(event.shipmentId)

      companionEvent.shipmentSpecimenData
        .map { info =>
          ShipmentSpecimen
            .create(id                  = ShipmentSpecimenId(info.getShipmentSpecimenId),
                    version             = 0L,
                    shipmentId          = ShipmentId(shipmentId.id),
                    specimenId          = SpecimenId(info.getSpecimenId),
                    state               = ShipmentItemState.Extra,
                    shipmentContainerId = None)
            .map(_.copy(timeAdded = eventTime))
        }
        .toList.sequenceU
        .flatMap { shipmentSpecimens =>
          shipmentSpecimens.foreach(shipmentSpecimenRepository.put)
          shipmentRepository.getByKey(shipmentId)
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
  //                                       state               = ShipmentItemState.Extra,
  //                                       shipmentContainerId = None)
  //     add.foreach { s =>
  //       shipmentSpecimenRepository.put(s.copy(timeAdded = eventTime))
  //     }
  //   }
  //   ().successNel[String]
  // }

  private def processUpdateCmd[T <: ShipmentModifyCommand](
      cmd:        T,
      cmdToEvent: (T, Shipment) => ServiceValidation[ShipmentEvent],
      applyEvent: ShipmentEvent => ServiceValidation[Shipment]
    ): Unit = {
    val event = for {
      shipment     <- shipmentRepository.getByKey(ShipmentId(cmd.id))
      validVersion <- shipment.requireVersion(cmd.expectedVersion)
      event        <- cmdToEvent(cmd, shipment)
    } yield event
    processWithResult(event)(applyEvent)
  }

  private def processUpdateCmdOnCreated[T <: ShipmentModifyCommand](
      cmd:        T,
      cmdToEvent: (T, CreatedShipment) => ServiceValidation[ShipmentEvent],
      applyEvent: ShipmentEvent => ServiceValidation[Shipment]
    ): Unit = {

    def internal(cmd: T, shipment: Shipment): ServiceValidation[ShipmentEvent] =
      shipment match {
        case s: CreatedShipment => cmdToEvent(cmd, s)
        case s => InvalidState(s"shipment not created: ${shipment.id}").failureNel[ShipmentEvent]
      }

    processUpdateCmd(cmd, internal, applyEvent)
  }

  private def processSpecimensCmd[T <: ShipmentSpecimenModifyCommand](
      cmd:        T,
      cmdToEvent: (T, Shipment) => ServiceValidation[ShipmentSpecimenEvent],
      applyEvent: ShipmentSpecimenEvent => ServiceValidation[Shipment]
    ): Unit = {
    val event = for {
      shipment <- shipmentRepository.getByKey(ShipmentId(cmd.shipmentId))
      valid    <- shipment.isCreatedOrUnpacked
      event    <- cmdToEvent(cmd, shipment)
    } yield event
    processWithResult(event)(applyEvent)
  }

  private def processSpecimenUpdateCmd[T <: ShipmentSpecimenModifyCommand](
      cmd:        T,
      cmdToEvent: (T, Shipment) => ServiceValidation[ShipmentSpecimenEvent],
      applyEvent: ShipmentSpecimenEvent => ServiceValidation[Shipment]
    ): Unit = {
    val event = for {
      shipment <- shipmentRepository.getByKey(ShipmentId(cmd.shipmentId))
      event    <- cmdToEvent(cmd, shipment)
    } yield event
    processWithResult(event)(applyEvent)
  }

  private def onValidEventAndVersion(
      event:        ShipmentEvent,
      eventType:    Boolean,
      eventVersion: Long
    )(applyEvent:   (Shipment, ShipmentEvent, OffsetDateTime) => ServiceValidation[Shipment]
    ): ServiceValidation[Shipment] = {
    if (!eventType) {
      ServiceError(s"invalid event type: $event").failureNel[Shipment]
    } else {
      for {
        shipment <- shipmentRepository
                     .getByKey(ShipmentId(event.id))
                     .leftMap(_ => NonEmptyList(s"shipment from event does not exist: $event"))
        validVersion <- shipment
                         .requireVersion(eventVersion)
                         .leftMap(
                           _ =>
                             NonEmptyList(
                               s"invalid version for event: shipment version: ${shipment.version}, event: $event"
                             )
                         )
        updated <- applyEvent(shipment, event, OffsetDateTime.parse(event.getTime))
      } yield updated
    }
  }

  private def onValidSpecimenEvent(
      event:      ShipmentSpecimenEvent,
      eventType:  Boolean
    )(applyEvent: (Shipment, ShipmentSpecimenEvent, OffsetDateTime) => ServiceValidation[Shipment]
    ): ServiceValidation[Shipment] = {
    if (!eventType) {
      ServiceError(s"invalid event type: $event").failureNel[Shipment]
    } else {
      for {
        shipment <- shipmentRepository.getByKey(ShipmentId(event.shipmentId))
        updated  <- applyEvent(shipment, event, OffsetDateTime.parse(event.getTime))
      } yield updated
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def createShipmentSpecimens(
      shipment:            Shipment,
      shipmentContainerId: Option[ShipmentContainerId],
      specimens:           List[Specimen]
    ): ServiceValidation[Seq[ShipmentSpecimen]] =
    for {
      validCentres <- specimensAtCentre(shipment.originLocationId, specimens: _*)
      canBeAdded   <- specimensNotPresentInShipment(specimens:                _*)
      shipmentSpecimens <- specimens.map { specimen =>
                            for {
                              id <- validNewIdentity(shipmentSpecimenRepository.nextIdentity,
                                                     shipmentSpecimenRepository)
                              ss <- ShipmentSpecimen.create(id = id,
                                                            version             = 0L,
                                                            shipmentId          = shipment.id,
                                                            specimenId          = specimen.id,
                                                            state               = ShipmentItemState.Present,
                                                            shipmentContainerId = shipmentContainerId)
                            } yield ss
                          }.sequenceU
    } yield shipmentSpecimens

  private def trakingNumberExistsError(trackingNumber: String): EntityCriteriaError =
    EntityCriteriaError(s"shipment with tracking number already exists: $trackingNumber")

  private def trackingNumberAvailable(trackingNumber: String): ServiceValidation[Unit] = {
    val exists = shipmentRepository.exists(c => (c.trackingNumber == trackingNumber))
    if (exists) trakingNumberExistsError(trackingNumber).failureNel[Unit]
    else ().successNel[String]
  }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  private def trackingNumberAvailable(
      trackingNumber: String,
      excludeId:      ShipmentId
    ): ServiceValidation[Unit] = {
    val exists = shipmentRepository.exists(c => (c.trackingNumber == trackingNumber) && (c.id != excludeId))
    if (exists) trakingNumberExistsError(trackingNumber).failureNel[Unit]
    else ().successNel[String]
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  protected def specimensAtCentre(
      locationId: LocationId,
      specimens:  Specimen*
    ): ServiceValidation[List[Specimen]] =
    specimens
      .map { specimen =>
        if (locationId == specimen.locationId) specimen.successNel[String]
        else specimen.inventoryId.failureNel[Specimen]
      }.toList.sequenceU.leftMap(
        err =>
          EntityCriteriaError(s"invalid centre for specimen inventory IDs: " + err.list.toList.mkString(", ")).nel
      )

  /**
   *
   * @param shipSpecimenMap map of inventory ID to Shipment Specimen.
   *
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def getShipmentSpecimens(
      shipmentId: ShipmentId,
      specimens:  List[Specimen]
    ): ServiceValidation[List[(String, ShipmentSpecimen)]] =
    specimens
      .map { specimen =>
        shipmentSpecimenRepository
          .getBySpecimen(shipmentId, specimen).map { shipSpecimen =>
            (specimen.inventoryId -> shipSpecimen)
          }.leftMap(err => NonEmptyList(specimen.inventoryId))
      }.toList.sequenceU.leftMap(
        err => EntityCriteriaError("specimens not in this shipment: " + err.list.toList.mkString(",")).nel
      )

  /**
   * Checks that a specimen is not present in any shipment.
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  protected def specimensNotPresentInShipment(specimens: Specimen*): ServiceValidation[List[Specimen]] =
    specimens
      .map { specimen =>
        val present = shipmentSpecimenRepository.allForSpecimen(specimen.id).filter { ss =>
          ss.state == ShipmentItemState.Present
        }

        if (present.isEmpty) specimen.successNel[String]
        else specimen.inventoryId.failureNel[Specimen]
      }.toList.sequenceU.leftMap(
        err =>
          EntityCriteriaError(
            s"specimens are already in an active shipment: " + err.list.toList.mkString(", ")
          ).nel
      )

  /**
   * Checks that a specimen is not present in a shipment.
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def specimensNotInShipment(
      shipmentId: ShipmentId,
      specimens:  Specimen*
    ): ServiceValidation[List[Specimen]] =
    specimens
      .map { specimen =>
        shipmentSpecimenRepository
          .getBySpecimen(shipmentId, specimen).fold(err => specimen.successNel[String],
                                                    _   => specimen.inventoryId.failureNel[Specimen])
      }.toList.sequenceU.leftMap(
        err =>
          EntityCriteriaError(
            s"specimen inventory IDs already in this shipment: " + err.list.toList.mkString(", ")
          ).nel
      )

  /**
   *
   * @param shipSpecimenMap map of inventory ID to Shipment Specimen.
   *
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def getPackedShipmentSpecimens(
      shipSpecimenMap: List[(String, ShipmentSpecimen)]
    ): ServiceValidation[List[ShipmentSpecimen]] =
    shipSpecimenMap
      .map {
        case (inventoryId, shipSpecimen) =>
          shipSpecimen.isStatePresent
            .map { _ =>
              shipSpecimen
            }.leftMap(err => NonEmptyList(inventoryId))
      }.toList.sequenceU.leftMap(
        err => EntityCriteriaError("shipment specimens not present: " + err.list.toList.mkString(",")).nel
      )

  /**
   *
   * @param shipSpecimenMap map of inventory ID to Shipment Specimen.
   *
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def getNonPackedShipmentSpecimens(
      shipSpecimenMap: List[(String, ShipmentSpecimen)]
    ): ServiceValidation[List[ShipmentSpecimen]] =
    shipSpecimenMap
      .map {
        case (inventoryId, shipSpecimen) =>
          shipSpecimen.isStateNotPresent
            .map { _ =>
              shipSpecimen
            }.leftMap(err => NonEmptyList(inventoryId))
      }.toList.sequenceU.leftMap(
        err => EntityCriteriaError("shipment specimens are present: " + err.list.toList.mkString(",")).nel
      )

  private def shipmentSpecimensPresent(
      shipmentId:           ShipmentId,
      specimenInventoryIds: String*
    ): ServiceValidation[List[ShipmentSpecimen]] =
    for {
      specimens           <- getSpecimens(specimenInventoryIds: _*)
      shipSpecimens       <- getShipmentSpecimens(shipmentId, specimens)
      packedShipSpecimens <- getPackedShipmentSpecimens(shipSpecimens)
    } yield packedShipSpecimens

  private def shipmentSpecimensNotPresent(
      shipmentId:           ShipmentId,
      specimenInventoryIds: String*
    ): ServiceValidation[List[ShipmentSpecimen]] =
    for {
      specimens              <- getSpecimens(specimenInventoryIds: _*)
      shipSpecimens          <- getShipmentSpecimens(shipmentId, specimens)
      nonPackedShipSpecimens <- getNonPackedShipmentSpecimens(shipSpecimens)
    } yield nonPackedShipSpecimens

  private def init(): Unit = {
    shipmentRepository.init
    shipmentSpecimenRepository.init
  }

  init

}
