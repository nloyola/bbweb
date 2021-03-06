package org.biobank.services.studies

import akka.actor._
import akka.persistence.{RecoveryCompleted, SaveSnapshotFailure, SaveSnapshotSuccess, SnapshotOffer}
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import org.biobank.domain._
import org.biobank.domain.annotations._
import org.biobank.domain.participants.CollectionEventRepository
import org.biobank.domain.studies._
import org.biobank.infrastructure.commands.CollectionEventTypeCommands._
import org.biobank.infrastructure.events.EventUtils
import org.biobank.services.{Processor, ServiceValidation, SnapshotWriter}
import play.api.libs.json._
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._

object CollectionEventTypeProcessor {

  def props: Props = Props[CollectionEventTypeProcessor]

  final case class SnapshotState(collectionEventTypes: Set[CollectionEventType])

  implicit val snapshotStateFormat: Format[SnapshotState] = Json.format[SnapshotState]

}

/**
 * The CollectionEventTypeProcessor is responsible for maintaining state changes for all
 * [[domain.studies.CollectionEventType CollectionEventType]] aggregates. This particular processor uses
 * [[https://doc.akka.io/docs/akka/2.5/persistence.html Akka's PersistentActor]]. It receives
 * Commands, and if valid, persists the generated events, afterwhich it will update the state of the
 * [[domain.studies.CollectionEventType CollectionEventType]] being processed.
 *
 * It is a child actor of [[services.studies.StudiesProcessor StudiesProcessor]].
 */
class CollectionEventTypeProcessor @javax.inject.Inject()(
    val collectionEventTypeRepository: CollectionEventTypeRepository,
    val collectionEventRepository:     CollectionEventRepository,
    val snapshotWriter:                SnapshotWriter)
    extends Processor {

  import CollectionEventTypeProcessor._
  import org.biobank.CommonValidations._
  import org.biobank.infrastructure.events.CollectionEventTypeEvents._
  import org.biobank.infrastructure.events.CollectionEventTypeEvents.CollectionEventTypeEvent.EventType

  override def persistenceId: String = "collection-event-type-processor-id"

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  private var replyTo: Option[ActorRef] = None

  /**
   * These are the events that are recovered during journal recovery. They cannot fail and must be
   * processed to recreate the current state of the aggregate.
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  override def receiveRecover: Receive = {
    case event: CollectionEventTypeEvent =>
      event.eventType match {
        case et: EventType.Added                     => applyAddedEvent(event)
        case et: EventType.Removed                   => applyRemovedEvent(event)
        case et: EventType.NameUpdated               => applyNameUpdatedEvent(event)
        case et: EventType.DescriptionUpdated        => applyDescriptionUpdatedEvent(event)
        case et: EventType.RecurringUpdated          => applyRecurringUpdatedEvent(event)
        case et: EventType.AnnotationTypeAdded       => applyAnnotationTypeAddedEvent(event)
        case et: EventType.AnnotationTypeUpdated     => applyAnnotationTypeUpdatedEvent(event)
        case et: EventType.AnnotationTypeRemoved     => applyAnnotationTypeRemovedEvent(event)
        case et: EventType.SpecimenDefinitionAdded   => applySpecimenDefinitionAddedEvent(event)
        case et: EventType.SpecimenDefinitionUpdated => applySpecimenDefinitionUpdatedEvent(event)
        case et: EventType.SpecimenDefinitionRemoved => applySpecimenDefinitionRemovedEvent(event)

        case event => log.error(s"event not handled: $event")
      }

    case SnapshotOffer(_, snapshotFilename: String) =>
      applySnapshot(snapshotFilename)

    case RecoveryCompleted =>
      log.debug("CollectionEventTypesProcessor: recovery completed")

    case msg =>
      log.error(s"message not handled: $msg")

  }

  /**
   * These are the commands that are requested. A command can fail, and will send the failure as a response
   * back to the user. Each valid command generates one or more events and is journaled.
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any", "org.wartremover.warts.Throw"))
  override def receiveCommand: Receive = {
    case cmd: AddCollectionEventTypeCmd =>
      process(addCmdToEvent(cmd))(applyAddedEvent)

    case cmd: RemoveCollectionEventTypeCmd =>
      processUpdateCmd(cmd, removeCmdToEvent, applyRemovedEvent)

    case cmd: UpdateCollectionEventTypeNameCmd =>
      processUpdateCmd(cmd, updateNameCmdToEvent, applyNameUpdatedEvent)

    case cmd: UpdateCollectionEventTypeDescriptionCmd =>
      processUpdateCmd(cmd, updateDescriptionCmdToEvent, applyDescriptionUpdatedEvent)

    case cmd: UpdateCollectionEventTypeRecurringCmd =>
      processUpdateCmd(cmd, updateRecurringCmdToEvent, applyRecurringUpdatedEvent)

    case cmd: CollectionEventTypeAddAnnotationTypeCmd =>
      processUpdateCmd(cmd, addAnnotationTypeCmdToEvent, applyAnnotationTypeAddedEvent)

    case cmd: CollectionEventTypeUpdateAnnotationTypeCmd =>
      processUpdateCmd(cmd, updateAnnotationTypeCmdToEvent, applyAnnotationTypeUpdatedEvent)

    case cmd: RemoveCollectionEventTypeAnnotationTypeCmd =>
      processUpdateCmd(cmd, removeAnnotationTypeCmdToEvent, applyAnnotationTypeRemovedEvent)

    case cmd: AddCollectedSpecimenDefinitionCmd =>
      processUpdateCmd(cmd, addSepcimenSpecCmdToEvent, applySpecimenDefinitionAddedEvent)

    case cmd: UpdateCollectedSpecimenDefinitionCmd =>
      processUpdateCmd(cmd, updateSepcimenSpecCmdToEvent, applySpecimenDefinitionUpdatedEvent)

    case cmd: RemoveCollectedSpecimenDefinitionCmd =>
      processUpdateCmd(cmd, removeSpecimenDefinitionCmdToEvent, applySpecimenDefinitionRemovedEvent)

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

    case cmd => log.error(s"CollectionEventTypeProcessor: message not handled: $cmd")
  }

  private def mySaveSnapshot(): Unit = {
    val snapshotState = SnapshotState(collectionEventTypeRepository.getValues.toSet)
    val filename      = snapshotWriter.save(persistenceId, Json.toJson(snapshotState).toString)
    log.debug(s"saved snapshot to: $filename")
    saveSnapshot(filename)
  }

  private def applySnapshot(filename: String): Unit = {
    val fileContents = snapshotWriter.load(filename);
    Json
      .parse(fileContents).validate[SnapshotState].fold(
        errors => log.error(s"could not apply snapshot: $filename: $errors"),
        snapshot => {
          log.debug(s"snapshot contains ${snapshot.collectionEventTypes.size} collection event types")
          snapshot.collectionEventTypes.foreach(collectionEventTypeRepository.put)
        }
      )
  }

  private def addCmdToEvent(cmd: AddCollectionEventTypeCmd): ServiceValidation[CollectionEventTypeEvent] = {
    val studyId = StudyId(cmd.studyId)
    for {
      cetId     <- validNewIdentity(collectionEventTypeRepository.nextIdentity, collectionEventTypeRepository)
      nameValid <- nameAvailable(cmd.name, studyId)
      newItem <- CollectionEventType
                  .create(studyId, cetId, 0L, cmd.name, cmd.description, cmd.recurring, Set.empty, Set.empty)
    } yield CollectionEventTypeEvent(cetId.id)
      .update(_.studyId       := studyId.id,
              _.sessionUserId := cmd.sessionUserId,
              _.time := OffsetDateTime.now
                .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.added.name                := newItem.name,
              _.added.optionalDescription := newItem.description,
              _.added.recurring           := newItem.recurring)
  }

  private def removeCmdToEvent(
      cmd:        RemoveCollectionEventTypeCmd,
      ceventType: CollectionEventType
    ): ServiceValidation[CollectionEventTypeEvent] =
    if (collectionEventRepository.collectionEventTypeInUse(ceventType.id)) {
      EntityInUse(s"collection event type in use: ${ceventType.id}").failureNel[CollectionEventTypeEvent]
    } else {
      CollectionEventTypeEvent(ceventType.id.id)
        .update(_.studyId         := ceventType.studyId.id,
                _.sessionUserId   := cmd.sessionUserId,
                _.time            := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                _.removed.version := cmd.expectedVersion).successNel[String]
    }

  private def updateNameCmdToEvent(
      cmd: UpdateCollectionEventTypeNameCmd,
      cet: CollectionEventType
    ): ServiceValidation[CollectionEventTypeEvent] =
    for {
      nameAvailable <- nameAvailable(cmd.name, cet.studyId, CollectionEventTypeId(cmd.id))
      newItem       <- cet.withName(cmd.name)
    } yield CollectionEventTypeEvent(newItem.id.id)
      .update(_.studyId             := newItem.studyId.id,
              _.sessionUserId       := cmd.sessionUserId,
              _.time                := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.nameUpdated.version := cmd.expectedVersion,
              _.nameUpdated.name    := newItem.name)

  private def updateDescriptionCmdToEvent(
      cmd: UpdateCollectionEventTypeDescriptionCmd,
      cet: CollectionEventType
    ): ServiceValidation[CollectionEventTypeEvent] =
    cet.withDescription(cmd.description).map { _ =>
      CollectionEventTypeEvent(cet.id.id)
        .update(_.studyId                                := cet.studyId.id,
                _.sessionUserId                          := cmd.sessionUserId,
                _.time                                   := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                _.descriptionUpdated.version             := cmd.expectedVersion,
                _.descriptionUpdated.optionalDescription := cmd.description)
    }

  private def updateRecurringCmdToEvent(
      cmd: UpdateCollectionEventTypeRecurringCmd,
      cet: CollectionEventType
    ): ServiceValidation[CollectionEventTypeEvent] =
    cet.withRecurring(cmd.recurring).map { _ =>
      CollectionEventTypeEvent(cet.id.id)
        .update(_.studyId                    := cet.studyId.id,
                _.sessionUserId              := cmd.sessionUserId,
                _.time                       := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                _.recurringUpdated.version   := cmd.expectedVersion,
                _.recurringUpdated.recurring := cmd.recurring)
    }

  private def addAnnotationTypeCmdToEvent(
      cmd: CollectionEventTypeAddAnnotationTypeCmd,
      cet: CollectionEventType
    ): ServiceValidation[CollectionEventTypeEvent] =
    for {
      annotationType <- {
        // need to call AnnotationType.create so that a new Id is generated
        AnnotationType
          .create(cmd.name, cmd.description, cmd.valueType, cmd.maxValueCount, cmd.options, cmd.required)
      }
      updatedCet <- cet.withAnnotationType(annotationType)
    } yield CollectionEventTypeEvent(cet.id.id)
      .update(_.studyId                            := cet.studyId.id,
              _.sessionUserId                      := cmd.sessionUserId,
              _.time                               := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
              _.annotationTypeAdded.version        := cmd.expectedVersion,
              _.annotationTypeAdded.annotationType := EventUtils.annotationTypeToEvent(annotationType))

  private def updateAnnotationTypeCmdToEvent(
      cmd: CollectionEventTypeUpdateAnnotationTypeCmd,
      cet: CollectionEventType
    ): ServiceValidation[CollectionEventTypeEvent] =
    for {
      valid <- AnnotationType.create(cmd.name,
                                     cmd.description,
                                     cmd.valueType,
                                     cmd.maxValueCount,
                                     cmd.options,
                                     cmd.required)
      annotationType <- valid.copy(id = AnnotationTypeId(cmd.annotationTypeId)).successNel[String]
      updatedCet     <- cet.withAnnotationType(annotationType)
    } yield {
      CollectionEventTypeEvent(cet.id.id)
        .update(_.studyId                              := cet.studyId.id,
                _.sessionUserId                        := cmd.sessionUserId,
                _.time                                 := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                _.annotationTypeUpdated.version        := cmd.expectedVersion,
                _.annotationTypeUpdated.annotationType := EventUtils.annotationTypeToEvent(annotationType))
    }

  private def removeAnnotationTypeCmdToEvent(
      cmd: RemoveCollectionEventTypeAnnotationTypeCmd,
      cet: CollectionEventType
    ): ServiceValidation[CollectionEventTypeEvent] =
    cet.removeAnnotationType(AnnotationTypeId(cmd.annotationTypeId)) map { c =>
      CollectionEventTypeEvent(cet.id.id)
        .update(_.studyId       := cet.studyId.id,
                _.sessionUserId := cmd.sessionUserId,
                _.time := OffsetDateTime.now
                  .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                _.annotationTypeRemoved.version := cmd.expectedVersion,
                _.annotationTypeRemoved.id      := cmd.annotationTypeId)
    }

  private def addSepcimenSpecCmdToEvent(
      cmd: AddCollectedSpecimenDefinitionCmd,
      cet: CollectionEventType
    ): ServiceValidation[CollectionEventTypeEvent] =
    for {
      specimenDefinition <- CollectedSpecimenDefinition.create(cmd.name,
                                                               cmd.description,
                                                               cmd.units,
                                                               cmd.anatomicalSourceType,
                                                               cmd.preservationType,
                                                               cmd.preservationTemperature,
                                                               cmd.specimenType,
                                                               cmd.maxCount,
                                                               cmd.amount)
      updatedCet <- cet.withSpecimenDefinition(specimenDefinition)
    } yield CollectionEventTypeEvent(cet.id.id).update(_.studyId       := cet.studyId.id,
                                                       _.sessionUserId := cmd.sessionUserId,
                                                       _.time := OffsetDateTime.now
                                                         .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                                       _.specimenDefinitionAdded.version := cmd.expectedVersion,
                                                       _.specimenDefinitionAdded.specimenDefinition := EventUtils
                                                         .specimenDefinitionToEvent(specimenDefinition))

  private def updateSepcimenSpecCmdToEvent(
      cmd: UpdateCollectedSpecimenDefinitionCmd,
      cet: CollectionEventType
    ): ServiceValidation[CollectionEventTypeEvent] =
    for {
      specimenDefinition <- {
        CollectedSpecimenDefinition(SpecimenDefinitionId(cmd.specimenDefinitionId),
                                    Slug(cmd.name),
                                    cmd.name,
                                    cmd.description,
                                    cmd.units,
                                    cmd.anatomicalSourceType,
                                    cmd.preservationType,
                                    cmd.preservationTemperature,
                                    cmd.specimenType,
                                    cmd.maxCount,
                                    cmd.amount).successNel[String]
      }
      updatedCet <- cet.withSpecimenDefinition(specimenDefinition)
    } yield CollectionEventTypeEvent(cet.id.id).update(_.studyId       := cet.studyId.id,
                                                       _.sessionUserId := cmd.sessionUserId,
                                                       _.time := OffsetDateTime.now
                                                         .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                                       _.specimenDefinitionUpdated.version := cmd.expectedVersion,
                                                       _.specimenDefinitionUpdated.specimenDefinition := EventUtils
                                                         .specimenDefinitionToEvent(specimenDefinition))

  private def removeSpecimenDefinitionCmdToEvent(
      cmd: RemoveCollectedSpecimenDefinitionCmd,
      cet: CollectionEventType
    ): ServiceValidation[CollectionEventTypeEvent] =
    cet.removeSpecimenDefinition(SpecimenDefinitionId(cmd.specimenDefinitionId)) map { c =>
      CollectionEventTypeEvent(cet.id.id).update(_.studyId       := cet.studyId.id,
                                                 _.sessionUserId := cmd.sessionUserId,
                                                 _.time := OffsetDateTime.now
                                                   .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                                                 _.specimenDefinitionRemoved.version := cmd.expectedVersion,
                                                 _.specimenDefinitionRemoved.id      := cmd.specimenDefinitionId)
    }

  private def processUpdateCmd[T <: CollectionEventTypeModifyCommand](
      cmd:        T,
      cmdToEvent: (T, CollectionEventType) => ServiceValidation[CollectionEventTypeEvent],
      applyEvent: CollectionEventTypeEvent => Unit
    ): Unit = {
    val event = for {
      cet          <- collectionEventTypeRepository.withId(StudyId(cmd.studyId), CollectionEventTypeId(cmd.id))
      validVersion <- cet.requireVersion(cmd.expectedVersion)
      event        <- cmdToEvent(cmd, cet)
    } yield event

    process(event)(applyEvent)
  }

  private def onValidEventAndVersion(
      event:        CollectionEventTypeEvent,
      eventType:    Boolean,
      eventVersion: Long
    )(applyEvent:   (CollectionEventType, OffsetDateTime) => ServiceValidation[Unit]
    ): Unit =
    if (!eventType) {
      log.error(s"invalid event type: $event")
    } else {
      collectionEventTypeRepository
        .getByKey(CollectionEventTypeId(event.id)).fold(
          err => log.error(s"collection event type from event does not exist: $err"),
          cet => {
            if (cet.version != eventVersion) {
              log.error(s"event version check failed: cet version: ${cet.version}, event: $event")
            } else {
              val eventTime = OffsetDateTime.parse(event.getTime)
              val update    = applyEvent(cet, eventTime)

              if (update.isFailure) {
                log.error(s"collection event type update from event failed: $update")
              }
            }
          }
        )
    }

  private def applyAddedEvent(event: CollectionEventTypeEvent): Unit =
    if (!event.eventType.isAdded) {
      log.error(s"invalid event type: $event")
    } else {
      val addedEvent = event.getAdded

      val v = CollectionEventType.create(studyId = StudyId(event.getStudyId),
                                         id                  = CollectionEventTypeId(event.id),
                                         version             = 0L,
                                         name                = addedEvent.getName,
                                         description         = addedEvent.description,
                                         recurring           = addedEvent.getRecurring,
                                         specimenDefinitions = Set.empty,
                                         annotationTypes     = Set.empty)

      if (v.isFailure) {
        log.error(s"could not add collection event type from event: $v")
      }

      v.foreach { ct =>
        val timeAdded = OffsetDateTime.parse(event.getTime)
        val slug      = collectionEventTypeRepository.uniqueSlugFromStr(ct.name)
        collectionEventTypeRepository.put(ct.copy(slug = slug, timeAdded = timeAdded))
      }
    }

  private def applyRemovedEvent(event: CollectionEventTypeEvent): Unit =
    onValidEventAndVersion(event, event.eventType.isRemoved, event.getRemoved.getVersion) { (cet, _) =>
      val v = collectionEventTypeRepository.getByKey(cet.id)
      v.foreach(collectionEventTypeRepository.remove)
      v.map(_ => ())
    }

  private def storeIfValid(
      validation: ServiceValidation[CollectionEventType],
      eventTime:  OffsetDateTime
    ): ServiceValidation[Unit] = {
    validation.foreach { c =>
      collectionEventTypeRepository.put(c.copy(timeModified = Some(eventTime)))
    }
    validation.map(_ => ())
  }

  private def applyNameUpdatedEvent(event: CollectionEventTypeEvent): Unit =
    onValidEventAndVersion(event, event.eventType.isNameUpdated, event.getNameUpdated.getVersion) {
      (cet, eventTime) =>
        val v = cet.withName(event.getNameUpdated.getName).map { updated =>
          updated.copy(slug = collectionEventTypeRepository.uniqueSlugFromStr(updated.name))
        }
        storeIfValid(v, eventTime)
    }

  private def applyDescriptionUpdatedEvent(event: CollectionEventTypeEvent): Unit =
    onValidEventAndVersion(event,
                           event.eventType.isDescriptionUpdated,
                           event.getDescriptionUpdated.getVersion) { (cet, eventTime) =>
      storeIfValid(cet.withDescription(event.getDescriptionUpdated.description), eventTime)
    }

  private def applyRecurringUpdatedEvent(event: CollectionEventTypeEvent): Unit =
    onValidEventAndVersion(event, event.eventType.isRecurringUpdated, event.getRecurringUpdated.getVersion) {
      (cet, eventTime) =>
        storeIfValid(cet.withRecurring(event.getRecurringUpdated.getRecurring), eventTime)
    }

  private def applyAnnotationTypeAddedEvent(event: CollectionEventTypeEvent): Unit =
    onValidEventAndVersion(event,
                           event.eventType.isAnnotationTypeAdded,
                           event.getAnnotationTypeAdded.getVersion) { (cet, eventTime) =>
      val eventAnnotationType = event.getAnnotationTypeAdded.getAnnotationType
      storeIfValid(cet.withAnnotationType(EventUtils.annotationTypeFromEvent(eventAnnotationType)), eventTime)
    }

  private def applyAnnotationTypeUpdatedEvent(event: CollectionEventTypeEvent): Unit =
    onValidEventAndVersion(event,
                           event.eventType.isAnnotationTypeUpdated,
                           event.getAnnotationTypeUpdated.getVersion) { (cet, eventTime) =>
      val eventAnnotationType = event.getAnnotationTypeUpdated.getAnnotationType
      val annotationType = AnnotationType(AnnotationTypeId(eventAnnotationType.getId),
                                          Slug(eventAnnotationType.getName),
                                          eventAnnotationType.getName,
                                          eventAnnotationType.description,
                                          AnnotationValueType.withName(eventAnnotationType.getValueType),
                                          eventAnnotationType.maxValueCount,
                                          eventAnnotationType.options,
                                          eventAnnotationType.getRequired)
      storeIfValid(cet.withAnnotationType(annotationType), eventTime)
    }

  private def applyAnnotationTypeRemovedEvent(event: CollectionEventTypeEvent): Unit =
    onValidEventAndVersion(event,
                           event.eventType.isAnnotationTypeRemoved,
                           event.getAnnotationTypeRemoved.getVersion) { (cet, eventTime) =>
      storeIfValid(cet.removeAnnotationType(AnnotationTypeId(event.getAnnotationTypeRemoved.getId)),
                   eventTime)
    }

  private def applySpecimenDefinitionAddedEvent(event: CollectionEventTypeEvent): Unit =
    onValidEventAndVersion(event,
                           event.eventType.isSpecimenDefinitionAdded,
                           event.getSpecimenDefinitionAdded.getVersion) { (cet, eventTime) =>
      storeIfValid(
        cet.withSpecimenDefinition(
          EventUtils.specimenDefinitionFromEvent(event.getSpecimenDefinitionAdded.getSpecimenDefinition)
        ),
        eventTime
      )
    }

  private def applySpecimenDefinitionUpdatedEvent(event: CollectionEventTypeEvent): Unit =
    onValidEventAndVersion(event,
                           event.eventType.isSpecimenDefinitionUpdated,
                           event.getSpecimenDefinitionUpdated.getVersion) { (cet, eventTime) =>
      storeIfValid(
        cet.withSpecimenDefinition(
          EventUtils.specimenDefinitionFromEvent(event.getSpecimenDefinitionUpdated.getSpecimenDefinition)
        ),
        eventTime
      )
    }

  private def applySpecimenDefinitionRemovedEvent(event: CollectionEventTypeEvent): Unit =
    onValidEventAndVersion(event,
                           event.eventType.isSpecimenDefinitionRemoved,
                           event.getSpecimenDefinitionRemoved.getVersion) { (cet, eventTime) =>
      val id = SpecimenDefinitionId(event.getSpecimenDefinitionRemoved.getId)
      storeIfValid(cet.removeSpecimenDefinition(id), eventTime)
    }

  val ErrMsgNameExists: String = "collection event type with name already exists"

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  private def nameAvailable(name: String, studyId: StudyId): ServiceValidation[Unit] =
    nameAvailableMatcher(name, collectionEventTypeRepository, ErrMsgNameExists) { item =>
      (item.name == name) && (item.studyId == studyId)
    }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  private def nameAvailable(
      name:      String,
      studyId:   StudyId,
      excludeId: CollectionEventTypeId
    ): ServiceValidation[Unit] =
    nameAvailableMatcher(name, collectionEventTypeRepository, ErrMsgNameExists) { item =>
      (item.name == name) && (item.studyId == studyId) && (item.id != excludeId)
    }

  private def init(): Unit =
    collectionEventTypeRepository.init

  init
}
