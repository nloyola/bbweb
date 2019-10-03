package org.biobank.services.centres

import akka.actor._
import akka.persistence.{RecoveryCompleted, SaveSnapshotFailure, SaveSnapshotSuccess, SnapshotOffer}
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import javax.inject.{Inject}
import org.biobank.domain.centres.{CentreId, CentreRepository}
import org.biobank.domain.containers._
import org.biobank.infrastructure.commands.ContainerTypeCommands._
import org.biobank.infrastructure.events.ContainerTypeEvents._
import org.biobank.services.{Processor, ServiceValidation, SnapshotWriter}
import play.api.libs.json._
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._

object ContainerTypesProcessor {

  def props: Props = Props[ContainerTypesProcessor]

  final case class SnapshotState(containerTypes: Set[ContainerType])

  implicit val snapshotStateFormat: Format[SnapshotState] = Json.format[SnapshotState]

}

class ContainerTypesProcessor @Inject()(
    val containerTypeRepository: ContainerTypeRepository,
    val containerRepository:     ContainerRepository,
    val schemaRepository:        ContainerSchemaRepository,
    val centreRepository:        CentreRepository,
    val snapshotWriter:          SnapshotWriter)
    extends Processor {
  import ContainerTypesProcessor._
  import org.biobank.CommonValidations._
  import ContainerTypeEvent.EventType

  override def persistenceId: String = "container-types-processor-id"

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  private var replyTo: Option[ActorRef] = None

  val ErrMsgNameExists: String = "container type with name already exists"

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  val receiveRecover: Receive = {
    case event: ContainerTypeEvent =>
      event.eventType match {
        case et: EventType.StorageAdded       => applyStorageAddedEvent(event)
        case et: EventType.SpecimenAdded      => applySpecimenAddedEvent(event)
        case et: EventType.NameUpdated        => applyNameUpdatedEvent(event)
        case et: EventType.DescriptionUpdated => applyDescriptionUpdatedEvent(event)
        case et: EventType.CentreUpdated      => applyCentreUpdatedEvent(event)
        case et: EventType.SchemaUpdated      => applySchemaUpdatedEvent(event)
        case et: EventType.SharedUpdated      => applySharedUpdatedEvent(event)
        case et: EventType.EnabledUpdated     => applyEnabledUpdatedEvent(event)
        case et: EventType.Removed            => applyRemovedEvent(event)

        case et => log.error(s"event not handled: $event")
      }

    case SnapshotOffer(_, snapshotFilename: String) =>
      applySnapshot(snapshotFilename)

    case RecoveryCompleted =>
      log.debug("ContainerTypesProcessor: recovery completed")

    case cmd => log.error(s"ContainerTypesProcessor: message not handled: $cmd")
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any", "org.wartremover.warts.Throw"))
  val receiveCommand: Receive = {
    case cmd: AddStorageContainerTypeCmd =>
      process(addStorageCmdToEvent(cmd))(applyStorageAddedEvent)

    case cmd: AddSpecimenContainerTypeCmd =>
      process(addSpecimenCmdToEvent(cmd))(applySpecimenAddedEvent)

    case cmd: UpdateContainerTypeNameCmd =>
      processUpdateCmd(cmd, updateNameCmdToEvent, applyNameUpdatedEvent)

    case cmd: UpdateContainerTypeDescriptionCmd =>
      processUpdateCmd(cmd, updateDescriptionCmdToEvent, applyDescriptionUpdatedEvent)

    case cmd: UpdateContainerTypeCentreCmd =>
      processUpdateCmd(cmd, updateCentreCmdToEvent, applyCentreUpdatedEvent)

    case cmd: UpdateContainerTypeSchemaCmd =>
      processUpdateCmd(cmd, updateSchemaCmdToEvent, applySchemaUpdatedEvent)

    case cmd: UpdateContainerTypeSharedCmd =>
      processUpdateCmd(cmd, updateSharedCmdToEvent, applySharedUpdatedEvent)

    case cmd: UpdateContainerTypeEnabledCmd =>
      processUpdateCmd(cmd, updateEnabledCmdToEvent, applyEnabledUpdatedEvent)

    case cmd: RemoveContainerTypeCmd =>
      process(removeCmdToEvent(cmd))(applyRemovedEvent)

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

    case cmd => log.error(s"ContainerTypesProcessor: message not handled: $cmd")
  }

  private def mySaveSnapshot(): Unit = {
    val snapshotState = SnapshotState(containerTypeRepository.getValues.toSet)
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
          log.debug(s"snapshot contains ${snapshot.containerTypes.size} containerTypes")
          snapshot.containerTypes.foreach(containerTypeRepository.put)
        }
      )
  }

  private def addStorageCmdToEvent(cmd: AddStorageContainerTypeCmd): ServiceValidation[ContainerTypeEvent] = {
    addCmdToEvent(cmd, createStorageFromCommand, eventFromStorageAddCommand)
  }

  private def addSpecimenCmdToEvent(
      cmd: AddSpecimenContainerTypeCmd
    ): ServiceValidation[ContainerTypeEvent] = {
    addCmdToEvent(cmd, createSpecimenFromCommand, eventFromSpecimenAddCommand)
  }

  private def updateNameCmdToEvent(
      cmd:   UpdateContainerTypeNameCmd,
      ctype: ContainerType
    ): ServiceValidation[ContainerTypeEvent] =
    for {
      nameAvailable <- nameAvailable(cmd.name, ctype.id, ctype.centreId)
      containerType <- ctype.withName(cmd.name)
    } yield ContainerTypeEvent(containerType.id.id)
      .update(_.sessionUserId       := cmd.sessionUserId,
              _.time                := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.nameUpdated.version := cmd.expectedVersion,
              _.nameUpdated.name    := cmd.name)

  private def updateDescriptionCmdToEvent(
      cmd:           UpdateContainerTypeDescriptionCmd,
      containerType: ContainerType
    ): ServiceValidation[ContainerTypeEvent] =
    containerType.withDescription(cmd.description).map { _ =>
      ContainerTypeEvent(containerType.id.id)
        .update(_.sessionUserId                          := cmd.sessionUserId,
                _.time                                   := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                _.descriptionUpdated.version             := cmd.expectedVersion,
                _.descriptionUpdated.optionalDescription := cmd.description)
    }

  private def updateSharedCmdToEvent(
      cmd:   UpdateContainerTypeSharedCmd,
      ctype: ContainerType
    ): ServiceValidation[ContainerTypeEvent] =
    ctype.withShared(cmd.shared).map { _ =>
      ContainerTypeEvent(ctype.id.id)
        .update(_.sessionUserId         := cmd.sessionUserId,
                _.time                  := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                _.sharedUpdated.version := cmd.expectedVersion,
                _.sharedUpdated.shared  := cmd.shared)
    }

  private def updateEnabledCmdToEvent(
      cmd:   UpdateContainerTypeEnabledCmd,
      ctype: ContainerType
    ): ServiceValidation[ContainerTypeEvent] =
    ctype.withEnabled(cmd.enabled).map { _ =>
      ContainerTypeEvent(ctype.id.id)
        .update(_.sessionUserId          := cmd.sessionUserId,
                _.time                   := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                _.enabledUpdated.version := cmd.expectedVersion,
                _.enabledUpdated.enabled := cmd.enabled)
    }

  private def updateCentreCmdToEvent(
      cmd:   UpdateContainerTypeCentreCmd,
      ctype: ContainerType
    ): ServiceValidation[ContainerTypeEvent] = {
    val centreId = CentreId(cmd.centreId)
    for {
      centre       <- centreRepository.getByKey(centreId)
      updatedCtype <- ctype.withCentre(centreId)
    } yield ContainerTypeEvent(ctype.id.id)
      .update(_.sessionUserId          := cmd.sessionUserId,
              _.time                   := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.centreUpdated.version  := cmd.expectedVersion,
              _.centreUpdated.centreId := cmd.centreId)
  }

  private def updateSchemaCmdToEvent(
      cmd:   UpdateContainerTypeSchemaCmd,
      ctype: ContainerType
    ): ServiceValidation[ContainerTypeEvent] = {
    val schemaId = ContainerSchemaId(cmd.schemaId)
    for {
      schema       <- schemaRepository.getByKey(schemaId)
      updatedCtype <- ctype.withSchema(schemaId)
    } yield ContainerTypeEvent(ctype.id.id)
      .update(_.sessionUserId          := cmd.sessionUserId,
              _.time                   := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.schemaUpdated.version  := cmd.expectedVersion,
              _.schemaUpdated.schemaId := cmd.schemaId)
  }

  private def removeCmdToEvent(cmd: RemoveContainerTypeCmd): ServiceValidation[ContainerTypeEvent] = {
    for {
      ctype <- containerTypeRepository.getByKey(ContainerTypeId(cmd.id))
      notInUse <- if (containerRepository.containerTypeInUse(ctype.id)) {
                   EntityInUse(s"container type in use: ${ctype.id}").failureNel[Unit]
                 } else {
                   ().successNel[String]
                 }
    } yield ContainerTypeEvent(ctype.id.id)
      .update(_.sessionUserId   := cmd.sessionUserId,
              _.time            := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.removed.version := ctype.version)
  }

  type ApplyEvent = (ContainerType, ContainerTypeEvent, OffsetDateTime) => ServiceValidation[ContainerType]

  private def onValidEventAndVersion(
      event:        ContainerTypeEvent,
      eventType:    Boolean,
      eventVersion: Long
    )(applyEvent:   ApplyEvent
    ): Unit =
    if (!eventType) {
      log.error(s"invalid event type: $event")
    } else {
      containerTypeRepository
        .getByKey(ContainerTypeId(event.id)).fold(
          err => log.error(s"container type from event does not exist: $err"),
          containerType => {
            if (containerType.version != eventVersion) {
              log.error(
                s"event version check failed: container type version: ${containerType.version}, event: $event"
              )
            } else {
              val eventTime = OffsetDateTime.parse(event.getTime)
              val update    = applyEvent(containerType, event, eventTime)

              if (update.isFailure) {
                log.error(s"container type update from event failed: $update")
              }
            }
          }
        )
    }

  private def applyStorageAddedEvent(event: ContainerTypeEvent): Unit = {
    if (!event.eventType.isStorageAdded) {
      log.error(s"invalid event type: $event")
    } else {
      val addedEvent = event.getStorageAdded
      val validation = StorageContainerType
        .create(id          = ContainerTypeId(event.id),
                version     = 0L,
                name        = addedEvent.getName,
                description = addedEvent.description,
                centreId    = CentreId(addedEvent.getCentreId),
                schemaId    = ContainerSchemaId(addedEvent.getSchemaId),
                shared      = addedEvent.getShared,
                enabled     = false).map { c =>
          c.copy(slug      = schemaRepository.uniqueSlugFromStr(c.name),
                 timeAdded = OffsetDateTime.parse(event.getTime))
        }

      if (validation.isFailure) {
        log.error(s"could not add containerType from event: $event")
      }

      validation.foreach(containerTypeRepository.put)
    }
  }

  private def applySpecimenAddedEvent(event: ContainerTypeEvent): Unit = {
    if (!event.eventType.isSpecimenAdded) {
      log.error(s"invalid event type: $event")
    } else {
      val addedEvent = event.getSpecimenAdded
      val validation = SpecimenContainerType
        .create(id          = ContainerTypeId(event.id),
                version     = 0L,
                name        = addedEvent.getName,
                description = addedEvent.description,
                centreId    = CentreId(addedEvent.getCentreId),
                schemaId    = ContainerSchemaId(addedEvent.getSchemaId),
                shared      = addedEvent.getShared,
                enabled     = false).map { c =>
          c.copy(slug      = schemaRepository.uniqueSlugFromStr(c.name),
                 timeAdded = OffsetDateTime.parse(event.getTime))
        }

      if (validation.isFailure) {
        log.error(s"could not add containerType from event: $event")
      }

      validation.foreach(containerTypeRepository.put)
    }
  }

  private def applyNameUpdatedEvent(event: ContainerTypeEvent): Unit = {
    onValidEventAndVersion(event, event.eventType.isNameUpdated, event.getNameUpdated.getVersion) {
      (containerType, _, eventTime) =>
        val name = event.getNameUpdated.getName
        val slug = containerTypeRepository.uniqueSlugFromStr(name)
        val v: ServiceValidation[ContainerType] = containerType match {
          case ct: StorageContainerType =>
            ct.withName(name).map(_.copy(slug = slug, timeModified = Some(eventTime)))
          case ct: SpecimenContainerType =>
            ct.withName(name).map(_.copy(slug = slug, timeModified = Some(eventTime)))
        }
        v.foreach(containerTypeRepository.put)
        v
    }
  }

  private def applyDescriptionUpdatedEvent(event: ContainerTypeEvent): Unit = {
    onValidEventAndVersion(event,
                           event.eventType.isDescriptionUpdated,
                           event.getDescriptionUpdated.getVersion) { (containerType, _, eventTime) =>
      val description = event.getDescriptionUpdated.description
      val v: ServiceValidation[ContainerType] = containerType match {
        case ct: StorageContainerType =>
          ct.withDescription(description).map(_.copy(timeModified = Some(eventTime)))
        case ct: SpecimenContainerType =>
          ct.withDescription(description).map(_.copy(timeModified = Some(eventTime)))
      }
      v.foreach(containerTypeRepository.put)
      v
    }
  }

  private def applyCentreUpdatedEvent(event: ContainerTypeEvent): Unit = {
    onValidEventAndVersion(event, event.eventType.isCentreUpdated, event.getCentreUpdated.getVersion) {
      (containerType, _, eventTime) =>
        val centreId = CentreId(event.getCentreUpdated.getCentreId)
        val v: ServiceValidation[ContainerType] = containerType match {
          case ct: StorageContainerType =>
            ct.withCentre(centreId).map(_.copy(timeModified = Some(eventTime)))
          case ct: SpecimenContainerType =>
            ct.withCentre(centreId).map(_.copy(timeModified = Some(eventTime)))
        }
        v.foreach(containerTypeRepository.put)
        v
    }
  }

  private def applySchemaUpdatedEvent(event: ContainerTypeEvent): Unit = {
    onValidEventAndVersion(event, event.eventType.isSchemaUpdated, event.getSchemaUpdated.getVersion) {
      (containerType, _, eventTime) =>
        val schemaId = ContainerSchemaId(event.getSchemaUpdated.getSchemaId)
        val v: ServiceValidation[ContainerType] = containerType match {
          case ct: StorageContainerType =>
            ct.withSchema(schemaId).map(_.copy(timeModified = Some(eventTime)))
          case ct: SpecimenContainerType =>
            ct.withSchema(schemaId).map(_.copy(timeModified = Some(eventTime)))
        }
        v.foreach(containerTypeRepository.put)
        v
    }
  }

  private def applySharedUpdatedEvent(event: ContainerTypeEvent): Unit = {
    onValidEventAndVersion(event, event.eventType.isSharedUpdated, event.getSharedUpdated.getVersion) {
      (containerType, _, eventTime) =>
        val shared = event.getSharedUpdated.getShared
        val v: ServiceValidation[ContainerType] = containerType match {
          case ct: StorageContainerType =>
            ct.withShared(shared).map(_.copy(timeModified = Some(eventTime)))
          case ct: SpecimenContainerType =>
            ct.withShared(shared).map(_.copy(timeModified = Some(eventTime)))
        }
        v.foreach(containerTypeRepository.put)
        v
    }
  }

  private def applyEnabledUpdatedEvent(event: ContainerTypeEvent): Unit = {
    onValidEventAndVersion(event, event.eventType.isEnabledUpdated, event.getEnabledUpdated.getVersion) {
      (containerType, _, eventTime) =>
        val enabled = event.getEnabledUpdated.getEnabled
        val v: ServiceValidation[ContainerType] = containerType match {
          case ct: StorageContainerType =>
            ct.withEnabled(enabled).map(_.copy(timeModified = Some(eventTime)))
          case ct: SpecimenContainerType =>
            ct.withEnabled(enabled).map(_.copy(timeModified = Some(eventTime)))
        }
        v.foreach(containerTypeRepository.put)
        v
    }
  }

  private def applyRemovedEvent(event: ContainerTypeEvent): Unit =
    onValidEventAndVersion(event, event.eventType.isRemoved, event.getRemoved.getVersion) {
      (ctype, _, eventTime) =>
        containerTypeRepository.remove(ctype)
        ctype.successNel[String]
    }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  private def nameAvailable(name: String, centreId: CentreId): ServiceValidation[Unit] =
    nameAvailableMatcher(name, containerTypeRepository, ErrMsgNameExists) { ctype =>
      (ctype.shared || ctype.centreId == centreId) && ctype.name == name
    }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  private def nameAvailable(
      name:      String,
      excludeId: ContainerTypeId,
      centreId:  CentreId
    ): ServiceValidation[Unit] =
    nameAvailableMatcher(name, containerTypeRepository, ErrMsgNameExists) { ctype =>
      (ctype.shared || ctype.centreId == centreId) && (ctype.name == name) && (ctype.id != excludeId)
    }

  private def processUpdateCmd[T <: ContainerTypeModifyCommand](
      cmd:            T,
      commandToEvent: (T, ContainerType) => ServiceValidation[ContainerTypeEvent],
      applyEvent:     ContainerTypeEvent => Unit
    ): Unit = {
    val event = for {
      containerType <- containerTypeRepository.getByKey(ContainerTypeId(cmd.id))
      validVersion  <- containerType.requireVersion(cmd.expectedVersion)
      event         <- commandToEvent(cmd, containerType)
    } yield event

    process(event)(applyEvent)
  }

  private def createStorageFromCommand(
      cmd:   AddStorageContainerTypeCmd,
      newId: ContainerTypeId
    ): ServiceValidation[StorageContainerType] = {
    StorageContainerType.create(id          = newId,
                                version     = 0L,
                                name        = cmd.name,
                                description = cmd.description,
                                centreId    = CentreId(cmd.centreId),
                                schemaId    = ContainerSchemaId(cmd.schemaId),
                                shared      = cmd.shared,
                                enabled     = false)
  }

  private def eventFromStorageAddCommand(
      cmd:   AddStorageContainerTypeCmd,
      newId: ContainerTypeId
    ): ContainerTypeEvent = {
    ContainerTypeEvent(newId.id)
      .update(_.sessionUserId                    := cmd.sessionUserId,
              _.time                             := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.storageAdded.name                := cmd.name,
              _.storageAdded.optionalDescription := cmd.description,
              _.storageAdded.centreId            := cmd.centreId,
              _.storageAdded.schemaId            := cmd.schemaId,
              _.storageAdded.shared              := cmd.shared)
  }

  private def createSpecimenFromCommand(
      cmd:   AddSpecimenContainerTypeCmd,
      newId: ContainerTypeId
    ): ServiceValidation[SpecimenContainerType] = {
    SpecimenContainerType.create(id          = newId,
                                 version     = 0L,
                                 name        = cmd.name,
                                 description = cmd.description,
                                 centreId    = CentreId(cmd.centreId),
                                 schemaId    = ContainerSchemaId(cmd.schemaId),
                                 shared      = cmd.shared,
                                 enabled     = false)
  }

  private def eventFromSpecimenAddCommand(
      cmd:   AddSpecimenContainerTypeCmd,
      newId: ContainerTypeId
    ): ContainerTypeEvent = {
    ContainerTypeEvent(newId.id)
      .update(_.sessionUserId                     := cmd.sessionUserId,
              _.time                              := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.specimenAdded.name                := cmd.name,
              _.specimenAdded.optionalDescription := cmd.description,
              _.specimenAdded.centreId            := cmd.centreId,
              _.specimenAdded.schemaId            := cmd.schemaId,
              _.specimenAdded.shared              := cmd.shared)
  }

  private def addCmdToEvent[C <: ContainerTypeAddCommand, T <: ContainerType](
      cmd:          C,
      cmdToCtype:   (C, ContainerTypeId) => ServiceValidation[T],
      ctypeToEvent: (C, ContainerTypeId) => ContainerTypeEvent
    ): ServiceValidation[ContainerTypeEvent] = {
    val centreId = CentreId(cmd.centreId)
    for {
      newId         <- validNewIdentity(containerTypeRepository.nextIdentity, containerTypeRepository)
      centre        <- centreRepository.getByKey(centreId)
      nameAvailable <- nameAvailable(cmd.name, centreId)
      newCtype      <- cmdToCtype(cmd, newId)
    } yield ctypeToEvent(cmd, newId)
  }

  private def init(): Unit =
    containerTypeRepository.init

  init
}
