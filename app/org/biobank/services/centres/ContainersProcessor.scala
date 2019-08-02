package org.biobank.services.centres

import akka.actor._
import akka.persistence.{RecoveryCompleted, SaveSnapshotSuccess, SaveSnapshotFailure, SnapshotOffer}
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import javax.inject.{Inject}
import org.biobank.domain._
import org.biobank.domain.centres.CentreId
import org.biobank.domain.containers._
import org.biobank.infrastructure.commands.ContainerCommands._
import org.biobank.infrastructure.events.ContainerEvents._
import org.biobank.services.{Processor, ServiceValidation, SnapshotWriter}
import play.api.libs.json._
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._

object ContainersProcessor {

  def props: Props = Props[ContainersProcessor]

  final case class SnapshotState(containers: Set[Container])

  implicit val snapshotStateFormat: Format[SnapshotState] = Json.format[SnapshotState]

}

class ContainersProcessor @Inject() (val containerRepository: ContainerRepository,
                                     val snapshotWriter:   SnapshotWriter)
    extends Processor {
  import ContainersProcessor._
  //import org.biobank.CommonValidations._
  import ContainerEvent.EventType

  override def persistenceId: String = "container-processor-id"

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  private var replyTo: Option[ActorRef] = None

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  val receiveRecover: Receive = {
    case event: ContainerEvent => event.eventType match {
      case et: EventType.RootAdded              => applyRootAddedEvent(event)
      // case et: EventType.NameUpdated        => applyNameUpdatedEvent(event)
      // case et: EventType.DescriptionUpdated => applyDescriptionUpdatedEvent(event)

      case et => log.error(s"event not handled: $event")
    }

    case SnapshotOffer(_, snapshotFilename: String) =>
      applySnapshot(snapshotFilename)

    case RecoveryCompleted =>
      log.debug("ContainersProcessor: recovery completed")

    case cmd => log.error(s"ContainersProcessor: message not handled: $cmd")
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any", "org.wartremover.warts.Throw"))
  val receiveCommand: Receive = {
    case cmd: AddRootContainerCmd =>
      process(addTopContainerCmdToEvent(cmd))(applyRootAddedEvent)

    case "snap" =>
     mySaveSnapshot
     replyTo = Some(sender())

    case SaveSnapshotSuccess(metadata) =>
      log.debug(s"snapshot saved successfully: ${metadata}")
      replyTo.foreach(_ ! akka.actor.Status.Success(s"snapshot saved: $metadata"))
      replyTo = None

    case SaveSnapshotFailure(metadata, reason) =>
      log.debug(s"snapshot save error: ${metadata}")
      replyTo.foreach(_ ! akka.actor.Status.Failure(reason))
      replyTo = None

    case "persistence_restart" =>
      throw new Exception("Intentionally throwing exception to test persistence by restarting the actor")

    case cmd => log.error(s"ContainersProcessor: message not handled: $cmd")
  }
  private def mySaveSnapshot(): Unit = {
    val snapshotState = SnapshotState(containerRepository.getValues.toSet)
    val filename = snapshotWriter.save(persistenceId, Json.toJson(snapshotState).toString)
    log.debug(s"saved snapshot to: $filename")
    saveSnapshot(filename)
  }

  private def applySnapshot(filename: String): Unit = {
    log.debug(s"snapshot recovery file: $filename")
    val fileContents = snapshotWriter.load(filename);
    Json.parse(fileContents).validate[SnapshotState].fold(
      errors => log.error(s"could not apply snapshot: $filename: $errors"),
      snapshot =>  {
        log.debug(s"snapshot contains ${snapshot.containers.size} containers")
        snapshot.containers.foreach(containerRepository.put)
      }
    )
  }

  private def addTopContainerCmdToEvent(cmd: AddRootContainerCmd): ServiceValidation[ContainerEvent] = {
    for {
      containerId   <- validNewIdentity(containerRepository.nextIdentity, containerRepository)
      sharedProperties <- ContainerSharedProperties.create(CentreId(cmd.centreId),
                                                          LocationId(cmd.locationId),
                                                          cmd.temperature)
      newContainer  <- StorageContainer.create(id               = containerId,
                                              version          = 0L,
                                              inventoryId      = cmd.inventoryId,
                                              label            = cmd.label,
                                              enabled          = false,
                                              containerTypeId  = ContainerTypeId(cmd.containerTypeId),
                                              sharedProperties = Some(sharedProperties),
                                              parentId         = None,
                                              position         = None,
                                              constraints      = None)
    } yield ContainerEvent(newContainer.id.id).update(
      _.sessionUserId             := cmd.sessionUserId,
      _.time                      := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
      _.rootAdded.inventoryId     := cmd.inventoryId,
      _.rootAdded.label           := cmd.label,
      _.rootAdded.centreId        := cmd.centreId,
      _.rootAdded.locationId      := cmd.locationId,
      _.rootAdded.temperature     := cmd.temperature.toString,
      _.rootAdded.containerTypeId := cmd.containerTypeId
    )
  }

  private def applyRootAddedEvent(event: ContainerEvent): Unit = {
    if (!event.eventType.isRootAdded) {
      log.error(s"invalid event type: $event")
    } else {
      val addedEvent = event.getRootAdded
      val validation = for {
          sharedProperties <- ContainerSharedProperties.create(
            CentreId(addedEvent.getCentreId),
            LocationId(addedEvent.getLocationId),
            PreservationTemperature.withName(addedEvent.getTemperature))
          container <- StorageContainer.create(
            id               = ContainerId(event.id),
            version          = 0L,
            inventoryId      = addedEvent.getInventoryId,
            label            = addedEvent.getLabel,
            enabled          = false,
            containerTypeId  = ContainerTypeId(addedEvent.getContainerTypeId),
            sharedProperties = Some(sharedProperties),
            parentId         = None,
            position         = None,
            constraints      = None).map { c =>
            c.copy(slug      = containerRepository.uniqueSlugFromStr(c.inventoryId),
                   timeAdded = OffsetDateTime.parse(event.getTime))
          }
        } yield container

      if (validation.isFailure) {
        log.error(s"could not add container from event: $event")
      }

      validation.foreach(containerRepository.put)
    }
  }

}
