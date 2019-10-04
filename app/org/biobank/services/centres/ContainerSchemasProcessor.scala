package org.biobank.services.centres

import akka.actor._
import akka.persistence.{RecoveryCompleted, SaveSnapshotFailure, SaveSnapshotSuccess, SnapshotOffer}
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import javax.inject.{Inject}
import org.biobank.domain.centres.{CentreId, CentreRepository}
import org.biobank.domain.containers._
import org.biobank.infrastructure.commands.ContainerSchemaCommands._
import org.biobank.infrastructure.events.ContainerSchemaEvents._
import org.biobank.services.{Processor, ServiceValidation, SnapshotWriter}
import play.api.libs.json._
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._

object ContainerSchemasProcessor {

  def props: Props = Props[ContainerSchemasProcessor]

  final case class SnapshotState(containerSchemas: Set[ContainerSchema])

  implicit val snapshotStateFormat: Format[SnapshotState] = Json.format[SnapshotState]

}

class ContainerSchemasProcessor @Inject()(
    val schemaRepository:        ContainerSchemaRepository,
    val centreRepository:        CentreRepository,
    val containerTypeRepository: ContainerTypeRepository,
    val snapshotWriter:          SnapshotWriter)
    extends Processor {
  import ContainerSchemasProcessor._
  import org.biobank.CommonValidations._
  import ContainerSchemaEvent.EventType

  override def persistenceId: String = "container-schema-processor-id"

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  private var replyTo: Option[ActorRef] = None

  val ErrMsgNameExists: String = "container schema with name already exists"

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  val receiveRecover: Receive = {
    case event: ContainerSchemaEvent =>
      event.eventType match {
        case et: EventType.Added              => applyAddedEvent(event)
        case et: EventType.NameUpdated        => applyNameUpdatedEvent(event)
        case et: EventType.DescriptionUpdated => applyDescriptionUpdatedEvent(event)
        case et: EventType.SharedUpdated      => applySharedUpdatedEvent(event)
        case et: EventType.CentreUpdated      => applyCentreUpdatedEvent(event)
        case et: EventType.LabelsUpdated      => applyLabelsUpdatedEvent(event)
        case et: EventType.Removed            => applyRemovedEvent(event)

        case et => log.error(s"event not handled: $event")
      }

    case SnapshotOffer(_, snapshotFilename: String) =>
      applySnapshot(snapshotFilename)

    case RecoveryCompleted =>
      log.debug("ContainerSchemasProcessor: recovery completed")

    case cmd => log.error(s"ContainerSchemasProcessor: message not handled: $cmd")
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any", "org.wartremover.warts.Throw"))
  val receiveCommand: Receive = {
    case cmd: AddContainerSchemaCmd =>
      process(addSchemaCmdToEvent(cmd))(applyAddedEvent)

    case cmd: UpdateContainerSchemaNameCmd =>
      processUpdateCmd(cmd, updateSchemaNameCmdToEvent, applyNameUpdatedEvent)

    case cmd: UpdateContainerSchemaDescriptionCmd =>
      processUpdateCmd(cmd, updateSchemaDescriptionCmdToEvent, applyDescriptionUpdatedEvent)

    case cmd: UpdateContainerSchemaSharedCmd =>
      processUpdateCmd(cmd, updateSchemaSharedCmdToEvent, applySharedUpdatedEvent)

    case cmd: UpdateContainerSchemaCentreCmd =>
      processUpdateCmd(cmd, updateSchemaCentreCmdToEvent, applyCentreUpdatedEvent)

    case cmd: UpdateContainerSchemaLabelsCmd =>
      processUpdateCmd(cmd, updateSchemaLabelsCmdToEvent, applyLabelsUpdatedEvent)

    case cmd: RemoveContainerSchemaCmd =>
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

    case cmd => log.error(s"ContainerSchemasProcessor: message not handled: $cmd")
  }

  private def mySaveSnapshot(): Unit = {
    val snapshotState = SnapshotState(schemaRepository.getValues.toSet)
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
          log.debug(s"snapshot contains ${snapshot.containerSchemas.size} containerSchemas")
          snapshot.containerSchemas.foreach(schemaRepository.put)
        }
      )
  }

  private def addSchemaCmdToEvent(cmd: AddContainerSchemaCmd): ServiceValidation[ContainerSchemaEvent] = {
    val centreId = CentreId(cmd.centreId)
    for {
      schemaId      <- validNewIdentity(schemaRepository.nextIdentity, schemaRepository)
      nameAvailable <- nameAvailable(cmd.name, centreId)
      newSchema <- ContainerSchema.create(id = schemaId,
                                          version     = 0L,
                                          name        = cmd.name,
                                          description = cmd.description,
                                          shared      = cmd.shared,
                                          centreId    = centreId)
    } yield ContainerSchemaEvent(newSchema.id.id)
      .update(_.sessionUserId := cmd.sessionUserId,
              _.time := OffsetDateTime.now
                .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.added.name                := cmd.name,
              _.added.optionalDescription := cmd.description,
              _.added.shared              := cmd.shared,
              _.added.centreId            := cmd.centreId)
  }

  private def updateSchemaNameCmdToEvent(
      cmd:    UpdateContainerSchemaNameCmd,
      schema: ContainerSchema
    ): ServiceValidation[ContainerSchemaEvent] =
    for {
      nameAvailable   <- nameAvailable(cmd.name, schema.id, schema.centreId)
      containerSchema <- schema.withName(cmd.name)
    } yield ContainerSchemaEvent(containerSchema.id.id)
      .update(_.sessionUserId       := cmd.sessionUserId,
              _.time                := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.nameUpdated.version := cmd.expectedVersion,
              _.nameUpdated.name    := cmd.name)

  private def updateSchemaDescriptionCmdToEvent(
      cmd:             UpdateContainerSchemaDescriptionCmd,
      containerSchema: ContainerSchema
    ): ServiceValidation[ContainerSchemaEvent] =
    containerSchema.withDescription(cmd.description).map { _ =>
      ContainerSchemaEvent(containerSchema.id.id)
        .update(_.sessionUserId                          := cmd.sessionUserId,
                _.time                                   := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                _.descriptionUpdated.version             := cmd.expectedVersion,
                _.descriptionUpdated.optionalDescription := cmd.description)
    }

  private def updateSchemaSharedCmdToEvent(
      cmd:    UpdateContainerSchemaSharedCmd,
      schema: ContainerSchema
    ): ServiceValidation[ContainerSchemaEvent] =
    schema.withShared(cmd.shared).map { _ =>
      ContainerSchemaEvent(schema.id.id)
        .update(_.sessionUserId         := cmd.sessionUserId,
                _.time                  := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                _.sharedUpdated.version := cmd.expectedVersion,
                _.sharedUpdated.shared  := cmd.shared)
    }

  private def updateSchemaCentreCmdToEvent(
      cmd:    UpdateContainerSchemaCentreCmd,
      schema: ContainerSchema
    ): ServiceValidation[ContainerSchemaEvent] = {
    val centreId = CentreId(cmd.centreId)
    for {
      centre        <- centreRepository.getByKey(centreId)
      updatedSchema <- schema.withCentre(centreId)
    } yield ContainerSchemaEvent(schema.id.id)
      .update(_.sessionUserId          := cmd.sessionUserId,
              _.time                   := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.centreUpdated.version  := cmd.expectedVersion,
              _.centreUpdated.centreId := cmd.centreId)
  }

  private def updateSchemaLabelsCmdToEvent(
      cmd:    UpdateContainerSchemaLabelsCmd,
      schema: ContainerSchema
    ): ServiceValidation[ContainerSchemaEvent] =
    schema.withLabels(cmd.labels.toSet).map { updatedSchema =>
      ContainerSchemaEvent(schema.id.id)
        .update(_.sessionUserId         := cmd.sessionUserId,
                _.time                  := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                _.labelsUpdated.version := cmd.expectedVersion,
                _.labelsUpdated.labels  := cmd.labels)
    }

  private def removeCmdToEvent(cmd: RemoveContainerSchemaCmd): ServiceValidation[ContainerSchemaEvent] =
    for {
      schema <- schemaRepository.getByKey(ContainerSchemaId(cmd.id))
      notInUse <- if (containerTypeRepository.schemaInUse(schema.id)) {
                   EntityInUse(s"schema in use: ${schema.id}").failureNel[Unit]
                 } else {
                   ().successNel[String]
                 }
    } yield ContainerSchemaEvent(schema.id.id)
      .update(_.sessionUserId   := cmd.sessionUserId,
              _.time            := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.removed.version := schema.version)

  private def onValidEventAndVersion(
      event:        ContainerSchemaEvent,
      eventType:    Boolean,
      eventVersion: Long
    )(applyEvent: (ContainerSchema, ContainerSchemaEvent,
          OffsetDateTime) => ServiceValidation[ContainerSchema]
    ): Unit =
    if (!eventType) {
      log.error(s"invalid event type: $event")
    } else {
      schemaRepository
        .getByKey(ContainerSchemaId(event.id)).fold(
          err => log.error(s"containerSchema from event does not exist: $err"),
          containerSchema => {
            if (containerSchema.version != eventVersion) {
              log.error(
                s"event version check failed: containerSchema version: ${containerSchema.version}, event: $event"
              )
            } else {
              val eventTime = OffsetDateTime.parse(event.getTime)
              val update    = applyEvent(containerSchema, event, eventTime)

              if (update.isFailure) {
                log.error(s"containerSchema update from event failed: $update")
              }
            }
          }
        )
    }

  private def applyAddedEvent(event: ContainerSchemaEvent): Unit =
    if (!event.eventType.isAdded) {
      log.error(s"invalid event type: $event")
    } else {
      val addedEvent = event.getAdded
      val validation = ContainerSchema
        .create(id          = ContainerSchemaId(event.id),
                version     = 0L,
                name        = addedEvent.getName,
                description = addedEvent.description,
                shared      = addedEvent.getShared,
                centreId    = CentreId(addedEvent.getCentreId)).map { c =>
          c.copy(slug      = schemaRepository.uniqueSlugFromStr(c.name),
                 timeAdded = OffsetDateTime.parse(event.getTime))
        }

      if (validation.isFailure) {
        log.error(s"could not add containerSchema from event: $event")
      }

      validation.foreach(schemaRepository.put)
    }

  private def applyNameUpdatedEvent(event: ContainerSchemaEvent): Unit =
    onValidEventAndVersion(event, event.eventType.isNameUpdated, event.getNameUpdated.getVersion) {
      (containerSchema, _, eventTime) =>
        val v = containerSchema.withName(event.getNameUpdated.getName).map { c =>
          c.copy(slug = schemaRepository.uniqueSlugFromStr(c.name), timeModified = Some(eventTime))
        }
        v.foreach(schemaRepository.put)
        v
    }

  private def applyDescriptionUpdatedEvent(event: ContainerSchemaEvent): Unit =
    onValidEventAndVersion(event,
                           event.eventType.isDescriptionUpdated,
                           event.getDescriptionUpdated.getVersion) { (containerSchema, _, eventTime) =>
      val v = containerSchema
        .withDescription(event.getDescriptionUpdated.description)
        .map { s =>
          s.copy(timeModified = Some(eventTime))
        }
      v.foreach(schemaRepository.put)
      v
    }

  private def applySharedUpdatedEvent(event: ContainerSchemaEvent): Unit =
    onValidEventAndVersion(event, event.eventType.isSharedUpdated, event.getSharedUpdated.getVersion) {
      (containerSchema, _, eventTime) =>
        val v = containerSchema
          .withShared(event.getSharedUpdated.getShared)
          .map { s =>
            s.copy(timeModified = Some(eventTime))
          }
        v.foreach(schemaRepository.put)
        v
    }

  private def applyCentreUpdatedEvent(event: ContainerSchemaEvent): Unit =
    onValidEventAndVersion(event, event.eventType.isCentreUpdated, event.getCentreUpdated.getVersion) {
      (schema, _, eventTime) =>
        val v = schema
          .withCentre(CentreId(event.getCentreUpdated.getCentreId))
          .map { s =>
            s.copy(timeModified = Some(eventTime))
          }
        v.foreach(schemaRepository.put)
        v
    }

  private def applyLabelsUpdatedEvent(event: ContainerSchemaEvent): Unit =
    onValidEventAndVersion(event, event.eventType.isLabelsUpdated, event.getLabelsUpdated.getVersion) {
      (schema, _, eventTime) =>
        val v = schema
          .withLabels(event.getLabelsUpdated.labels.toSet)
          .map { s =>
            s.copy(timeModified = Some(eventTime))
          }
        v.foreach(schemaRepository.put)
        v
    }

  private def applyRemovedEvent(event: ContainerSchemaEvent): Unit =
    onValidEventAndVersion(event, event.eventType.isRemoved, event.getRemoved.getVersion) {
      (schema, _, eventTime) =>
        schemaRepository.remove(schema)
        schema.successNel[String]
    }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  private def nameAvailable(name: String, centreId: CentreId): ServiceValidation[Unit] =
    nameAvailableMatcher(name, schemaRepository, ErrMsgNameExists) { schema =>
      (schema.shared || schema.centreId == centreId) && schema.name == name
    }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  private def nameAvailable(
      name:      String,
      excludeId: ContainerSchemaId,
      centreId:  CentreId
    ): ServiceValidation[Unit] =
    nameAvailableMatcher(name, schemaRepository, ErrMsgNameExists) { schema =>
      (schema.shared || schema.centreId == centreId) && (schema.name == name) && (schema.id != excludeId)
    }

  private def processUpdateCmd[T <: ContainerSchemaModifyCommand](
      cmd:            T,
      commandToEvent: (T, ContainerSchema) => ServiceValidation[ContainerSchemaEvent],
      applyEvent:     ContainerSchemaEvent => Unit
    ): Unit = {
    val event = for {
      containerSchema <- schemaRepository.getByKey(ContainerSchemaId(cmd.id))
      validVersion    <- containerSchema.requireVersion(cmd.expectedVersion)
      event           <- commandToEvent(cmd, containerSchema)
    } yield event

    process(event)(applyEvent)
  }

  private def init(): Unit =
    schemaRepository.init

  init
}
