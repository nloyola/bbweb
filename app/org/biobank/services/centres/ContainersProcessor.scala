package org.biobank.services.centres

import akka.actor._
import akka.persistence.{RecoveryCompleted, SaveSnapshotFailure, SaveSnapshotSuccess, SnapshotOffer}
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
import org.biobank.CommonValidations.EntityCriteriaError

object ContainersProcessor {

  def props: Props = Props[ContainersProcessor]

  final case class SnapshotState(containers: Set[Container])

  implicit val snapshotStateFormat: Format[SnapshotState] = Json.format[SnapshotState]

}

final case class AddChildRequest(
    containerType:  ContainerType,
    schema:         ContainerSchema,
    parent:         Container,
    newContainerId: ContainerId)

class ContainersProcessor @Inject()(
    val containerRepository:       ContainerRepository,
    val containerTypeRepository:   ContainerTypeRepository,
    val containerSchemaRepository: ContainerSchemaRepository,
    val snapshotWriter:            SnapshotWriter)
    extends Processor {
  import ContainersProcessor._
  import org.biobank.CommonValidations._
  import ContainerEvent.EventType

  override def persistenceId: String = "container-processor-id"

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  private var replyTo: Option[ActorRef] = None

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  val receiveRecover: Receive = {
    case event: ContainerEvent =>
      event.eventType match {
        case et: EventType.RootAdded     => applyRootAddedEvent(event)
        case et: EventType.StorageAdded  => applyStorageAddedEvent(event)
        case et: EventType.SpecimenAdded => applySpecimenAddedEvent(event)
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
      process(addRootContainerCmdToEvent(cmd))(applyRootAddedEvent)

    case cmd: AddStorageContainerCmd =>
      process(addStorageContainerCmdToEvent(cmd))(applyStorageAddedEvent)

    case cmd: AddSpecimenContainerCmd =>
      process(addSpecimenContainerCmdToEvent(cmd))(applySpecimenAddedEvent)

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
    val filename      = snapshotWriter.save(persistenceId, Json.toJson(snapshotState).toString)
    log.debug(s"saved snapshot to: $filename")
    saveSnapshot(filename)
  }

  private def applySnapshot(filename: String): Unit = {
    log.debug(s"snapshot recovery file: $filename")
    val fileContents = snapshotWriter.load(filename);
    Json
      .parse(fileContents).validate[SnapshotState].fold(
        errors => log.error(s"could not apply snapshot: $filename: $errors"),
        snapshot => {
          log.debug(s"snapshot contains ${snapshot.containers.size} containers")
          snapshot.containers.foreach(containerRepository.put)
        }
      )
  }

  private def addRootContainerCmdToEvent(cmd: AddRootContainerCmd): ServiceValidation[ContainerEvent] = {
    val containerTypeId = ContainerTypeId(cmd.containerTypeId)
    for {
      containerId   <- validNewIdentity(containerRepository.nextIdentity, containerRepository)
      containerType <- containerTypeRepository.getByKey(containerTypeId)
      validCtype    <- containerTypeRepository.getStorageContainerType(containerType.id)
      inventoryId   <- inventoryIdAvailable(cmd.inventoryId)
      newContainer <- RootContainer
                       .create(id              = containerId,
                               version         = 0L,
                               inventoryId     = cmd.inventoryId,
                               label           = cmd.label,
                               containerTypeId = containerType.id,
                               centreId        = CentreId(cmd.centreId),
                               locationId      = LocationId(cmd.locationId),
                               temperature     = cmd.temperature,
                               constraints     = None)
    } yield ContainerEvent(newContainer.id.id)
      .update(_.sessionUserId             := cmd.sessionUserId,
              _.time                      := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.rootAdded.inventoryId     := cmd.inventoryId,
              _.rootAdded.label           := cmd.label,
              _.rootAdded.centreId        := cmd.centreId,
              _.rootAdded.locationId      := cmd.locationId,
              _.rootAdded.temperature     := cmd.temperature.toString,
              _.rootAdded.containerTypeId := cmd.containerTypeId)
  }

  private def addStorageContainerCmdToEvent(cmd: AddStorageContainerCmd): ServiceValidation[ContainerEvent] =
    for {
      values     <- addChildValidate(cmd)
      validCtype <- containerTypeRepository.getStorageContainerType(values.containerType.id)
      position   <- ContainerSchemaPosition.create(values.containerType.schemaId, cmd.label)
      newContainer <- StorageContainer
                       .create(id              = values.newContainerId,
                               version         = 0L,
                               inventoryId     = cmd.inventoryId,
                               containerTypeId = values.containerType.id,
                               parentId        = values.parent.id,
                               position        = position,
                               constraints     = None)
    } yield ContainerEvent(newContainer.id.id)
      .update(_.sessionUserId                := cmd.sessionUserId,
              _.time                         := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.storageAdded.inventoryId     := cmd.inventoryId,
              _.storageAdded.label           := cmd.label,
              _.storageAdded.containerTypeId := cmd.containerTypeId,
              _.storageAdded.parentId        := cmd.parentId,
              _.storageAdded.schemaId        := cmd.schemaId)

  private def addSpecimenContainerCmdToEvent(
      cmd: AddSpecimenContainerCmd
    ): ServiceValidation[ContainerEvent] =
    for {
      values     <- addChildValidate(cmd)
      validCtype <- containerTypeRepository.getSpecimenContainerType(values.containerType.id)
      position   <- ContainerSchemaPosition.create(values.containerType.schemaId, cmd.label)
      newContainer <- SpecimenContainer
                       .create(id              = values.newContainerId,
                               version         = 0L,
                               inventoryId     = cmd.inventoryId,
                               containerTypeId = values.containerType.id,
                               parentId        = values.parent.id,
                               position        = position)
    } yield ContainerEvent(newContainer.id.id)
      .update(_.sessionUserId                 := cmd.sessionUserId,
              _.time                          := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.specimenAdded.inventoryId     := cmd.inventoryId,
              _.specimenAdded.label           := cmd.label,
              _.specimenAdded.containerTypeId := cmd.containerTypeId,
              _.specimenAdded.parentId        := cmd.parentId,
              _.specimenAdded.schemaId        := cmd.schemaId)

  private def addChildValidate(cmd: AddSubContainerCommand): ServiceValidation[AddChildRequest] =
    for {
      containerType <- containerTypeRepository.getByKey(ContainerTypeId(cmd.containerTypeId))
      schema        <- containerSchemaRepository.getByKey(containerType.schemaId)
      inventoryId   <- inventoryIdAvailable(cmd.inventoryId)
      parent <- containerRepository
                 .getByKey(ContainerId(cmd.parentId)).leftMap(
                   err => IdNotFound(s"parent id ${cmd.parentId}").nel
                 )
      posEmpty    <- containerRepository.positionEmpty(parent.id, cmd.label)
      containerId <- validNewIdentity(containerRepository.nextIdentity, containerRepository)
    } yield AddChildRequest(containerType, schema, parent, containerId)

  private def applyRootAddedEvent(event: ContainerEvent): Unit =
    if (!event.eventType.isRootAdded) {
      log.error(s"invalid event type: $event")
    } else {
      val addedEvent = event.getRootAdded
      val validation = for {
        container <- RootContainer
                      .create(id              = ContainerId(event.id),
                              version         = 0L,
                              label           = addedEvent.getLabel,
                              inventoryId     = addedEvent.getInventoryId,
                              containerTypeId = ContainerTypeId(addedEvent.getContainerTypeId),
                              centreId        = CentreId(addedEvent.getCentreId),
                              locationId      = LocationId(addedEvent.getLocationId),
                              temperature     = PreservationTemperature.withName(addedEvent.getTemperature),
                              constraints     = None).map { c =>
                        c.copy(slug      = containerRepository.uniqueSlugFromStr(c.label),
                               timeAdded = OffsetDateTime.parse(event.getTime))
                      }
      } yield container

      if (validation.isFailure) {
        log.error(s"could not add root container from event: $event")
      }

      validation.foreach(containerRepository.put)
    }

  private def applyStorageAddedEvent(event: ContainerEvent): Unit =
    if (!event.eventType.isStorageAdded) {
      log.error(s"invalid event type: $event")
    } else {
      val addedEvent = event.getStorageAdded
      val validation = for {
        position <- ContainerSchemaPosition
                     .create(ContainerSchemaId(addedEvent.getSchemaId), addedEvent.getLabel)
        container <- StorageContainer
                      .create(id              = ContainerId(event.id),
                              version         = 0L,
                              inventoryId     = addedEvent.getInventoryId,
                              containerTypeId = ContainerTypeId(addedEvent.getContainerTypeId),
                              parentId        = ContainerId(addedEvent.getParentId),
                              position        = position,
                              constraints     = None).map { c =>
                        c.copy(slug      = containerRepository.uniqueSlugFromStr(c.position.label),
                               timeAdded = OffsetDateTime.parse(event.getTime))
                      }
      } yield container

      if (validation.isFailure) {
        log.error(s"could not add storage container from event: $event")
      }

      validation.foreach(containerRepository.put)
    }

  private def applySpecimenAddedEvent(event: ContainerEvent): Unit =
    if (!event.eventType.isSpecimenAdded) {
      log.error(s"invalid event type: $event")
    } else {
      val addedEvent = event.getSpecimenAdded
      val validation = for {
        position <- ContainerSchemaPosition
                     .create(ContainerSchemaId(addedEvent.getSchemaId), addedEvent.getLabel)
        container <- SpecimenContainer
                      .create(id              = ContainerId(event.id),
                              version         = 0L,
                              inventoryId     = addedEvent.getInventoryId,
                              containerTypeId = ContainerTypeId(addedEvent.getContainerTypeId),
                              parentId        = ContainerId(addedEvent.getParentId),
                              position        = position).map { c =>
                        c.copy(slug      = containerRepository.uniqueSlugFromStr(c.position.label),
                               timeAdded = OffsetDateTime.parse(event.getTime))
                      }
      } yield container

      if (validation.isFailure) {
        log.error(s"could not add specimen container from event: $event")
      }

      validation.foreach(containerRepository.put)
    }

  private def inventoryIdAvailable(inventoryId: String): ServiceValidation[Unit] = {
    val exists = containerRepository
      .exists { e =>
        e.inventoryId == inventoryId
      }
    if (exists)
      EntityCriteriaError(s"container with inventory ID already exists: $inventoryId").failureNel[Unit]
    else ().successNel[String]
  }
}
