package org.biobank.services.centres

import akka.actor._
import akka.persistence.{RecoveryCompleted, SaveSnapshotFailure, SaveSnapshotSuccess, SnapshotOffer}
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import javax.inject.{Inject}
import org.biobank.domain._
import org.biobank.domain.centres.{CentreId, CentreRepository}
import org.biobank.domain.containers._
import org.biobank.infrastructure.commands.ContainerCommands._
import org.biobank.infrastructure.events.ContainerEvents._
import org.biobank.services.{Processor, ServiceError, ServiceValidation, SnapshotWriter}
import play.api.libs.json._
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._
import org.biobank.CommonValidations.EntityCriteriaError
import org.biobank.domain.participants.SpecimenRepository

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
    val centreRepository:          CentreRepository,
    val containerRepository:       ContainerRepository,
    val containerTypeRepository:   ContainerTypeRepository,
    val containerSchemaRepository: ContainerSchemaRepository,
    val specimenRepository:        SpecimenRepository,
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
        case et: EventType.RootAdded             => applyRootAddedEvent(event)
        case et: EventType.StorageAdded          => applyStorageAddedEvent(event)
        case et: EventType.SpecimenAdded         => applySpecimenAddedEvent(event)
        case et: EventType.LabelUpdated          => applyLabelUpdatedEvent(event)
        case et: EventType.InventoryIdUpdated    => applyInventoryIdUpdatedEvent(event)
        case et: EventType.EnabledUpdated        => applyEnabledUpdatedEvent(event)
        case et: EventType.ContainerTypeUpdated  => applyContainerTypeUpdatedEvent(event)
        case et: EventType.CentreLocationUpdated => applyCentreLocationUpdatedEvent(event)
        case et: EventType.TemperatureUpdated    => applyTemperatureUpdatedEvent(event)
        case et: EventType.ConstraintsUpdated    => applyConstraintsUpdatedEvent(event)
        case et: EventType.ConstraintsRemoved    => applyConstraintsRemovedEvent(event)
        case et: EventType.PositionUpdated       => applyPositionUpdatedEvent(event)
        case et: EventType.Removed               => applyRemovedEvent(event)

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

    case cmd: UpdateContainerLabelCmd =>
      processUpdateCmd(cmd, updateLabelCmdToEvent, applyLabelUpdatedEvent)

    case cmd: UpdateContainerInventoryIdCmd =>
      processUpdateCmd(cmd, updateInventoryIdCmdToEvent, applyInventoryIdUpdatedEvent)

    case cmd: UpdateContainerEnabledCmd =>
      processUpdateCmd(cmd, updateEnabledCmdToEvent, applyEnabledUpdatedEvent)

    case cmd: UpdateContainerContainerTypeCmd =>
      processUpdateCmd(cmd, updateContainerTypeCmdToEvent, applyContainerTypeUpdatedEvent)

    case cmd: UpdateContainerCentreLocationCmd =>
      processUpdateCmd(cmd, updateCentreLocationCmdToEvent, applyCentreLocationUpdatedEvent)

    case cmd: UpdateContainerTemperatureCmd =>
      processUpdateCmd(cmd, updateTemperatureCmdToEvent, applyTemperatureUpdatedEvent)

    case cmd: UpdateContainerConstraintsCmd =>
      processUpdateCmd(cmd, updateConstraintsCmdToEvent, applyConstraintsUpdatedEvent)

    case cmd: RemoveContainerConstraintsCmd =>
      processUpdateCmd(cmd, removeConstraintsCmdToEvent, applyConstraintsRemovedEvent)

    case cmd: UpdateContainerPositionCmd =>
      processUpdateCmd(cmd, updatePositionCmdToEvent, applyPositionUpdatedEvent)

    case cmd: RemoveContainerCmd =>
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
      newContainer <- StorageContainer
                       .create(id              = values.newContainerId,
                               version         = 0L,
                               inventoryId     = cmd.inventoryId,
                               containerTypeId = values.containerType.id,
                               parentId        = values.parent.id,
                               schemaLabel     = ContainerSchemaLabel(values.schema.id, cmd.label),
                               constraints     = None)
    } yield ContainerEvent(newContainer.id.id)
      .update(_.sessionUserId                := cmd.sessionUserId,
              _.time                         := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.storageAdded.inventoryId     := cmd.inventoryId,
              _.storageAdded.containerTypeId := cmd.containerTypeId,
              _.storageAdded.parentId        := cmd.parentId,
              _.storageAdded.schemaId        := values.schema.id.id,
              _.storageAdded.label           := cmd.label)

  private def addSpecimenContainerCmdToEvent(
      cmd: AddSpecimenContainerCmd
    ): ServiceValidation[ContainerEvent] =
    for {
      values     <- addChildValidate(cmd)
      validCtype <- containerTypeRepository.getSpecimenContainerType(values.containerType.id)
      newContainer <- SpecimenContainer
                       .create(id              = values.newContainerId,
                               version         = 0L,
                               inventoryId     = cmd.inventoryId,
                               containerTypeId = values.containerType.id,
                               parentId        = values.parent.id,
                               schemaLabel     = ContainerSchemaLabel(values.schema.id, cmd.label))
    } yield ContainerEvent(newContainer.id.id)
      .update(_.sessionUserId                 := cmd.sessionUserId,
              _.time                          := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.specimenAdded.inventoryId     := cmd.inventoryId,
              _.specimenAdded.containerTypeId := cmd.containerTypeId,
              _.specimenAdded.parentId        := cmd.parentId,
              _.specimenAdded.schemaId        := values.schema.id.id,
              _.specimenAdded.label           := cmd.label)

  private def addChildValidate(cmd: AddSubContainerCommand): ServiceValidation[AddChildRequest] =
    for {
      containerType <- containerTypeRepository.getByKey(ContainerTypeId(cmd.containerTypeId))
      inventoryId   <- inventoryIdAvailable(cmd.inventoryId)
      parent <- containerRepository
                 .getByKey(ContainerId(cmd.parentId)).leftMap(
                   err => IdNotFound(s"parent id ${cmd.parentId}").nel
                 )
      parentCType <- containerTypeRepository.getByKey(parent.containerTypeId)
      schema      <- containerSchemaRepository.getByKey(parentCType.schemaId)
      labelValid  <- containerSchemaRepository.getLabel(schema.id, cmd.label)
      posEmpty    <- containerRepository.positionEmpty(parent.id, cmd.label)
      containerId <- validNewIdentity(containerRepository.nextIdentity, containerRepository)
    } yield AddChildRequest(containerType, schema, parent, containerId)

  private def updateLabelCmdToEvent(
      cmd:       UpdateContainerLabelCmd,
      container: Container
    ): ServiceValidation[ContainerEvent] = {

    def validateRootContainerCmd(container: RootContainer) =
      container.withLabel(cmd.label)

    def validateChildContainerCmd(container: ChildContainer) =
      for {
        labelValid       <- containerSchemaRepository.getLabel(container.schemaLabel.schemaId, cmd.label)
        posEmpty         <- containerRepository.positionEmpty(container.parentId, cmd.label)
        updatedContainer <- container.withLabel(cmd.label)
      } yield ContainerEvent(container.id.id)

    val validation = container match {
      case c: RootContainer  => validateRootContainerCmd(c)
      case c: ChildContainer => validateChildContainerCmd(c)
    }

    validation.map { _ =>
      ContainerEvent(container.id.id)
        .update(_.sessionUserId        := cmd.sessionUserId,
                _.time                 := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                _.labelUpdated.version := cmd.expectedVersion,
                _.labelUpdated.label   := cmd.label)
    }
  }

  private def updateInventoryIdCmdToEvent(
      cmd:       UpdateContainerInventoryIdCmd,
      container: Container
    ): ServiceValidation[ContainerEvent] = {
    val slug = containerRepository.uniqueSlugFromStr(cmd.inventoryId)
    for {
      idAvailable      <- inventoryIdAvailable(cmd.inventoryId, container.id)
      updatedContainer <- container.withInventoryId(cmd.inventoryId, slug)
    } yield ContainerEvent(container.id.id)
      .update(_.sessionUserId                  := cmd.sessionUserId,
              _.time                           := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.inventoryIdUpdated.version     := cmd.expectedVersion,
              _.inventoryIdUpdated.inventoryId := cmd.inventoryId)
  }

  private def updateEnabledCmdToEvent(
      cmd:       UpdateContainerEnabledCmd,
      container: Container
    ): ServiceValidation[ContainerEvent] = {
    val validation = container match {
      case c: RootContainer    => c.withEnabled(cmd.enabled, OffsetDateTime.now)
      case c: StorageContainer => c.withEnabled(cmd.enabled, OffsetDateTime.now)
      case _ =>
        ServiceError(
          InvalidStatus(s"not a root or storage container: $id").failureNel[StorageContainer].toString
        ).failureNel[Container]
    }
    validation.map { _ =>
      ContainerEvent(container.id.id)
        .update(_.sessionUserId          := cmd.sessionUserId,
                _.time                   := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                _.enabledUpdated.version := cmd.expectedVersion,
                _.enabledUpdated.enabled := cmd.enabled)
    }
  }

  private def updateContainerTypeCmdToEvent(
      cmd:       UpdateContainerContainerTypeCmd,
      container: Container
    ): ServiceValidation[ContainerEvent] = {
    for {
      containerType <- containerTypeRepository.getByKey(ContainerTypeId(cmd.containerTypeId))
      validType     <- validateContainerAndContainerType(container, containerType)
      updated       <- container.withContainerType(containerType.id, OffsetDateTime.now)
    } yield ContainerEvent(container.id.id)
      .update(_.sessionUserId                        := cmd.sessionUserId,
              _.time                                 := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.containerTypeUpdated.version         := cmd.expectedVersion,
              _.containerTypeUpdated.containerTypeId := cmd.containerTypeId)

  }

  private def validateContainerAndContainerType(
      container:     Container,
      containerType: ContainerType
    ): ServiceValidation[Unit] = {
    (container, containerType) match {
      case (c: RootContainer, ct:     StorageContainerType)  => ().successNel[String]
      case (c: StorageContainer, ct:  StorageContainerType)  => ().successNel[String]
      case (c: SpecimenContainer, ct: SpecimenContainerType) => ().successNel[String]
      case _ => EntityCriteriaError(s"container and containerTypes not compatible").failureNel[Unit]
    }
  }

  private def updateCentreLocationCmdToEvent(
      cmd:       UpdateContainerCentreLocationCmd,
      container: Container
    ): ServiceValidation[ContainerEvent] = {
    container match {
      case c: RootContainer =>
        for {
          centre   <- centreRepository.getByKey(CentreId(cmd.centreId))
          location <- centre.locationWithId(LocationId(cmd.locationId))
          updated  <- c.withCentreLocation(centre.id, location.id, OffsetDateTime.now)
        } yield ContainerEvent(container.id.id)
          .update(_.sessionUserId                    := cmd.sessionUserId,
                  _.time                             := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                  _.centreLocationUpdated.version    := cmd.expectedVersion,
                  _.centreLocationUpdated.centreId   := cmd.centreId,
                  _.centreLocationUpdated.locationId := cmd.locationId)
      case _ =>
        EntityCriteriaError("cannot change the centre location on a non root container")
          .failureNel[ContainerEvent]
    }

  }

  private def updateTemperatureCmdToEvent(
      cmd:       UpdateContainerTemperatureCmd,
      container: Container
    ): ServiceValidation[ContainerEvent] = {
    container match {
      case c: RootContainer =>
        c.withTemperature(cmd.temperature, OffsetDateTime.now).map { _ =>
          ContainerEvent(container.id.id)
            .update(_.sessionUserId                  := cmd.sessionUserId,
                    _.time                           := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                    _.temperatureUpdated.version     := cmd.expectedVersion,
                    _.temperatureUpdated.temperature := cmd.temperature.toString)
        }
      case _ =>
        EntityCriteriaError("cannot change the temperature on a non root container")
          .failureNel[ContainerEvent]
    }
  }

  private def updateConstraintsCmdToEvent(
      cmd:       UpdateContainerConstraintsCmd,
      container: Container
    ): ServiceValidation[ContainerEvent] = {
    for {
      constraints <- ContainerConstraints.create(cmd.name,
                                                 cmd.description,
                                                 cmd.anatomicalSources,
                                                 cmd.preservationTypes,
                                                 cmd.specimenTypes)
      valid <- container match {
                case c: RootContainer    => c.withConstraints(Some(constraints), OffsetDateTime.now)
                case c: StorageContainer => c.withConstraints(Some(constraints), OffsetDateTime.now)
                case _ =>
                  EntityCriteriaError("cannot add constraints to a specimen container").failureNel[Container]
              }
    } yield ContainerEvent(container.id.id)
      .update(_.sessionUserId                          := cmd.sessionUserId,
              _.time                                   := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.constraintsUpdated.version             := cmd.expectedVersion,
              _.constraintsUpdated.name                := cmd.name,
              _.constraintsUpdated.optionalDescription := cmd.description,
              _.constraintsUpdated.anatomicalSources   := cmd.anatomicalSources.toSeq.map(_.toString),
              _.constraintsUpdated.preservationTypes   := cmd.preservationTypes.toSeq.map(_.toString),
              _.constraintsUpdated.specimenTypes       := cmd.specimenTypes.toSeq.map(_.toString))

  }

  private def removeConstraintsCmdToEvent(
      cmd:       RemoveContainerConstraintsCmd,
      container: Container
    ): ServiceValidation[ContainerEvent] = {
    val v = container match {
      case c: RootContainer    => c.withConstraints(None, OffsetDateTime.now)
      case c: StorageContainer => c.withConstraints(None, OffsetDateTime.now)
      case _ =>
        EntityCriteriaError("cannot remove constraints on a specimen container").failureNel[Container]
    }

    v.map { _ =>
      ContainerEvent(container.id.id)
        .update(_.sessionUserId              := cmd.sessionUserId,
                _.time                       := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                _.constraintsRemoved.version := cmd.expectedVersion)
    }

  }

  private def updatePositionCmdToEvent(
      cmd:       UpdateContainerPositionCmd,
      container: Container
    ): ServiceValidation[ContainerEvent] = {
    val v = container match {
      case c: ChildContainer =>
        if ((c.parentId.id == cmd.parentId) && (c.schemaLabel.label == cmd.label)) {
          EntityCriteriaError("container already occupies requested position").failureNel[Container]
        } else {
          for {
            parent     <- containerRepository.getByKey(ContainerId(cmd.parentId))
            schema     <- containerSchemaRepository.getByKey(c.schemaLabel.schemaId)
            labelValid <- containerSchemaRepository.getLabel(schema.id, cmd.label)
            posEmpty   <- containerRepository.positionEmpty(parent.id, cmd.label)
          } yield parent
        }

      case _: RootContainer =>
        EntityCriteriaError("cannot update position on a root container").failureNel[Container]
    }
    v.map { _ =>
      ContainerEvent(container.id.id)
        .update(_.sessionUserId            := cmd.sessionUserId,
                _.time                     := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                _.positionUpdated.version  := cmd.expectedVersion,
                _.positionUpdated.parentId := cmd.parentId,
                _.positionUpdated.label    := cmd.label)
    }
  }

  private def removeCmdToEvent(cmd: RemoveContainerCmd): ServiceValidation[ContainerEvent] = {
    for {
      container <- containerRepository.getByKey(ContainerId(cmd.id))
      notUsedByContainers <- if (containerRepository.containerInUse(container.id)) {
                              EntityInUse(s"container in use: ${container.id}").failureNel[Unit]
                            } else {
                              ().successNel[String]
                            }
      notUsedBySpecimens <- if (specimenRepository.containerInUse(container.id)) {
                             EntityInUse(s"container in use by specimens: ${container.id}").failureNel[Unit]
                           } else {
                             ().successNel[String]
                           }
    } yield ContainerEvent(container.id.id)
      .update(_.sessionUserId   := cmd.sessionUserId,
              _.time            := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.removed.version := container.version)
  }

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
                        c.copy(slug      = containerRepository.uniqueSlugFromStr(addedEvent.getInventoryId),
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
      val schemaId   = ContainerSchemaId(addedEvent.getSchemaId)
      val validation = for {
        labelValid <- containerSchemaRepository.isLabelValid(schemaId, addedEvent.getLabel)
        schemaLabel = ContainerSchemaLabel(schemaId, addedEvent.getLabel)
        container <- StorageContainer
                      .create(id              = ContainerId(event.id),
                              version         = 0L,
                              inventoryId     = addedEvent.getInventoryId,
                              containerTypeId = ContainerTypeId(addedEvent.getContainerTypeId),
                              parentId        = ContainerId(addedEvent.getParentId),
                              schemaLabel     = schemaLabel,
                              constraints     = None).map { c =>
                        c.copy(slug      = containerRepository.uniqueSlugFromStr(addedEvent.getInventoryId),
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
      val schemaId   = ContainerSchemaId(addedEvent.getSchemaId)
      val validation = for {
        labelValid <- containerSchemaRepository.isLabelValid(schemaId, addedEvent.getLabel)
        schemaLabel = ContainerSchemaLabel(schemaId, addedEvent.getLabel)
        container <- SpecimenContainer
                      .create(id              = ContainerId(event.id),
                              version         = 0L,
                              inventoryId     = addedEvent.getInventoryId,
                              containerTypeId = ContainerTypeId(addedEvent.getContainerTypeId),
                              parentId        = ContainerId(addedEvent.getParentId),
                              schemaLabel     = schemaLabel).map { c =>
                        c.copy(slug      = containerRepository.uniqueSlugFromStr(addedEvent.getInventoryId),
                               timeAdded = OffsetDateTime.parse(event.getTime))
                      }
      } yield container

      if (validation.isFailure) {
        log.error(s"could not add specimen container from event: $event")
      }

      validation.foreach(containerRepository.put)
    }

  private def applyLabelUpdatedEvent(event: ContainerEvent): Unit = {
    onValidEventAndVersion(event, event.eventType.isLabelUpdated, event.getLabelUpdated.getVersion) {
      (container, _, eventTime) =>
        val updated = container.withLabel(event.getLabelUpdated.getLabel, eventTime)
        updated.foreach(containerRepository.put)
        updated
    }
  }

  private def applyInventoryIdUpdatedEvent(event: ContainerEvent): Unit = {
    onValidEventAndVersion(event,
                           event.eventType.isInventoryIdUpdated,
                           event.getInventoryIdUpdated.getVersion) { (container, _, eventTime) =>
      val inventoryId = event.getInventoryIdUpdated.getInventoryId
      val slug        = containerRepository.uniqueSlugFromStr(inventoryId)
      val updated     = container.withInventoryId(inventoryId, slug, eventTime)
      updated.foreach(containerRepository.put)
      updated
    }
  }

  private def applyEnabledUpdatedEvent(event: ContainerEvent): Unit = {
    onValidEventAndVersion(event, event.eventType.isEnabledUpdated, event.getEnabledUpdated.getVersion) {
      (container, _, eventTime) =>
        val enabled = event.getEnabledUpdated.getEnabled
        val updated = container match {
          case c: RootContainer    => c.withEnabled(enabled, eventTime)
          case c: StorageContainer => c.withEnabled(enabled, eventTime)
          case _ =>
            ServiceError(
              InvalidStatus(s"not a root or storage container: $id").failureNel[StorageContainer].toString
            ).failureNel[Container]
        }
        updated.foreach(containerRepository.put)
        updated
    }
  }

  private def applyContainerTypeUpdatedEvent(event: ContainerEvent): Unit = {
    onValidEventAndVersion(event,
                           event.eventType.isContainerTypeUpdated,
                           event.getContainerTypeUpdated.getVersion) { (container, _, eventTime) =>
      val containerTypeId = ContainerTypeId(event.getContainerTypeUpdated.getContainerTypeId)
      val updated = for {
        containerType <- containerTypeRepository.getByKey(containerTypeId)
        validType     <- validateContainerAndContainerType(container, containerType)
        updated       <- container.withContainerType(containerTypeId, eventTime)
      } yield updated
      updated.foreach(containerRepository.put)
      updated
    }
  }

  private def applyCentreLocationUpdatedEvent(event: ContainerEvent): Unit = {
    onValidEventAndVersion(event,
                           event.eventType.isCentreLocationUpdated,
                           event.getCentreLocationUpdated.getVersion) { (container, _, eventTime) =>
      val updated = container match {
        case c: RootContainer =>
          val centreId   = CentreId(event.getCentreLocationUpdated.getCentreId)
          val locationId = LocationId(event.getCentreLocationUpdated.getLocationId)
          for {
            centre   <- centreRepository.getByKey(centreId)
            location <- centre.locationWithId(locationId)
            updated  <- c.withCentreLocation(centreId, locationId, eventTime)
          } yield updated
        case _ =>
          EntityCriteriaError("cannot change the centre location on a non root container")
            .failureNel[Container]
      }
      updated.foreach(containerRepository.put)
      updated
    }
  }

  private def applyTemperatureUpdatedEvent(event: ContainerEvent): Unit = {
    onValidEventAndVersion(event,
                           event.eventType.isTemperatureUpdated,
                           event.getTemperatureUpdated.getVersion) { (container, _, eventTime) =>
      val updated = container match {
        case c: RootContainer =>
          val temperature = PreservationTemperature.withName(event.getTemperatureUpdated.getTemperature)
          c.withTemperature(temperature, eventTime)
        case _ =>
          EntityCriteriaError("cannot change the temperature on a non root container")
            .failureNel[Container]
      }
      updated.foreach(containerRepository.put)
      updated
    }
  }

  private def applyConstraintsUpdatedEvent(event: ContainerEvent): Unit = {
    onValidEventAndVersion(event,
                           event.eventType.isConstraintsUpdated,
                           event.getConstraintsUpdated.getVersion) { (container, _, eventTime) =>
      val constraintsEvent  = event.getConstraintsUpdated
      val anatomicalSources = constraintsEvent.anatomicalSources.map(AnatomicalSourceType.withName).toSet
      val preservationTypes = constraintsEvent.preservationTypes.map(PreservationType.withName).toSet
      val specimenTypes     = constraintsEvent.specimenTypes.map(SpecimenType.withName).toSet
      val updated = for {
        constraints <- ContainerConstraints.create(constraintsEvent.getName,
                                                   constraintsEvent.description,
                                                   anatomicalSources,
                                                   preservationTypes,
                                                   specimenTypes)
        updated <- container match {
                    case c: RootContainer    => c.withConstraints(Some(constraints), eventTime)
                    case c: StorageContainer => c.withConstraints(Some(constraints), eventTime)
                    case _ =>
                      EntityCriteriaError("cannot add constraints to a specimen container")
                        .failureNel[Container]
                  }
      } yield updated
      updated.foreach(containerRepository.put)
      updated
    }
  }

  private def applyConstraintsRemovedEvent(event: ContainerEvent): Unit = {
    onValidEventAndVersion(event,
                           event.eventType.isConstraintsRemoved,
                           event.getConstraintsRemoved.getVersion) { (container, _, eventTime) =>
      val updated = container match {
        case c: RootContainer    => c.withConstraints(None, eventTime)
        case c: StorageContainer => c.withConstraints(None, eventTime)
        case _ =>
          EntityCriteriaError("cannot remove constraints on a specimen container").failureNel[Container]
      }
      updated.foreach(containerRepository.put)
      updated
    }
  }

  private def applyPositionUpdatedEvent(event: ContainerEvent): Unit = {
    onValidEventAndVersion(event, event.eventType.isPositionUpdated, event.getPositionUpdated.getVersion) {
      val positionEvent = event.getPositionUpdated
      (container, _, eventTime) =>
        val updated = container match {
          case c: ChildContainer =>
            for {
              parent     <- containerRepository.getByKey(ContainerId(positionEvent.getParentId))
              schema     <- containerSchemaRepository.getByKey(c.schemaLabel.schemaId)
              labelValid <- containerSchemaRepository.getLabel(schema.id, positionEvent.getLabel)
              posEmpty   <- containerRepository.positionEmpty(parent.id, positionEvent.getLabel)
              updated    <- c.withPosition(parent.id, positionEvent.getLabel, eventTime)
            } yield updated

          case _: RootContainer =>
            EntityCriteriaError("cannot update parent on a root containre").failureNel[Container]
        }
        updated.foreach(containerRepository.put)
        updated
    }
  }

  private def applyRemovedEvent(event: ContainerEvent): Unit = {
    onValidEventAndVersion(event, event.eventType.isRemoved, event.getRemoved.getVersion) {
      (container, _, eventTime) =>
        val updated = for {
          notUsedByContainers <- if (containerRepository.containerInUse(container.id)) {
                                  EntityInUse(s"container in use: ${container.id}").failureNel[Unit]
                                } else {
                                  ().successNel[String]
                                }
          notUsedBySpecimens <- if (specimenRepository.containerInUse(container.id)) {
                                 EntityInUse(s"container in use by specimens: ${container.id}")
                                   .failureNel[Unit]
                               } else {
                                 ().successNel[String]
                               }
        } yield container
        updated.foreach(containerRepository.remove)
        updated
    }
  }

  private def processUpdateCmd[T <: ContainerModifyCommand](
      cmd:            T,
      commandToEvent: (T, Container) => ServiceValidation[ContainerEvent],
      applyEvent:     ContainerEvent => Unit
    ): Unit = {
    val event = for {
      container    <- containerRepository.getByKey(ContainerId(cmd.id))
      validVersion <- container.requireVersion(cmd.expectedVersion)
      event        <- commandToEvent(cmd, container)
    } yield event

    process(event)(applyEvent)
  }

  type ApplyEvent = (Container, ContainerEvent, OffsetDateTime) => ServiceValidation[Container]

  private def onValidEventAndVersion(
      event:        ContainerEvent,
      eventType:    Boolean,
      eventVersion: Long
    )(applyEvent:   ApplyEvent
    ): Unit =
    if (!eventType) {
      log.error(s"invalid event type: $event")
    } else {
      containerRepository
        .getByKey(ContainerId(event.id))
        .fold(err => log.error(s"container from event does not exist: $err"),
              container => {
                if (container.version != eventVersion) {
                  log.error(
                    s"event version check failed: container version: ${container.version}, event: $event"
                  )
                } else {
                  val eventTime = OffsetDateTime.parse(event.getTime)
                  val update    = applyEvent(container, event, eventTime)

                  if (update.isFailure) {
                    log.error(s"container update from event failed: $update")
                  }
                }
              })
    }

  private def inventoryIdAvailable(inventoryId: String): ServiceValidation[Unit] = {
    val exists = containerRepository.exists(c => (c.inventoryId == inventoryId))
    if (exists)
      EntityCriteriaError(s"container with inventory ID already exists: $inventoryId").failureNel[Unit]
    else ().successNel[String]
  }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  private def inventoryIdAvailable(inventoryId: String, excludeId: ContainerId): ServiceValidation[Unit] = {
    val exists = containerRepository.exists(c => (c.inventoryId == inventoryId) && (c.id != excludeId))
    if (exists)
      EntityCriteriaError(s"container with inventory ID already exists: $inventoryId").failureNel[Unit]
    else ().successNel[String]
  }

}
