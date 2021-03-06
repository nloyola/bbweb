package org.biobank.services.participants

import akka.actor._

import akka.persistence.{RecoveryCompleted, SaveSnapshotFailure, SaveSnapshotSuccess, SnapshotOffer}
//import com.github.ghik.silencer.silent
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import javax.inject.{Inject, Singleton}
import org.biobank.domain.LocationId
import org.biobank.domain.participants._
import org.biobank.domain.processing.ProcessingEventInputSpecimenRepository
import org.biobank.domain.studies.{CollectionEventType, CollectionEventTypeRepository, SpecimenDefinitionId}
import org.biobank.infrastructure.commands.SpecimenCommands._
import org.biobank.infrastructure.events.SpecimenEvents._
import org.biobank.services._
import play.api.libs.json._
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._

object SpecimensProcessor {
  import org.biobank.CommonValidations._

  def props: Props = Props[SpecimensProcessor]

  final case class SnapshotState(specimens: Set[Specimen], ceventSpecimens: Set[CeventSpecimen])

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val snapshotStateFormat: Format[SnapshotState] = Json.format[SnapshotState]

  def specimenSpecNotFound(id: String): IdNotFound = IdNotFound(s"collection specimen spec id: $id")

}

/**
 * Responsible for handing [[infrastructure.commands.SpecimenCommands SpecimenCommands]] to add, update and
 * remove [[domain.participant.Specimen Specimens]].
 */
@Singleton
class SpecimensProcessor @Inject()(
    val specimenRepository:                     SpecimenRepository,
    val collectionEventRepository:              CollectionEventRepository,
    val collectionEventTypeRepository:          CollectionEventTypeRepository,
    val ceventSpecimenRepository:               CeventSpecimenRepository,
    val processingEventInputSpecimenRepository: ProcessingEventInputSpecimenRepository,
    val snapshotWriter:                         SnapshotWriter)
// FIXME add container repository when implemented
//val containerRepository:       ContainerRepository)
    extends Processor {

  import SpecimensProcessor._
  import SpecimenEvent.EventType
  import org.biobank.infrastructure.events.EventUtils._
  import org.biobank.CommonValidations._

  override def persistenceId: String = "specimens-processor-id"

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  private var replyTo: Option[ActorRef] = None

  /**
   * These are the events that are recovered during journal recovery. They cannot fail and must be
   * processed to recreate the current state of the aggregate.
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  val receiveRecover: Receive = {
    case event: SpecimenEvent =>
      event.eventType match {
        case et: EventType.Added              => applyAddedEvent(event)
        case et: EventType.Moved              => applyMovedEvent(event)
        case et: EventType.PosisitionAssigned => applyPositionAssignedEvent(event)
        case et: EventType.AmountRemoved      => applyAmountRemovedEvent(event)
        case et: EventType.UsableUpdated      => applyUsableUpdatedEvent(event)
        case et: EventType.Removed            => applyRemovedEvent(event)

        case event => log.error(s"event not handled: $event")
      }

    case SnapshotOffer(_, snapshotFilename: String) =>
      applySnapshot(snapshotFilename)

    case RecoveryCompleted =>
      log.debug("SpecimensProcessor: recovery completed")

    case msg =>
      log.error(s"message not handled: $msg")
  }

  /**
   * These are the commands that are requested. A command can fail, and will send the failure as a response
   * back to the user. Each valid command generates one or more events and is journaled.
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any", "org.wartremover.warts.Throw"))
  val receiveCommand: Receive = {
    case command: SpecimenCommand =>
      log.debug(s"SpecimensProcessor: command: $command")

      command match {
        case cmd: AddSpecimensCmd =>
          process(addCmdToEvent(cmd))(applyAddedEvent)

        case cmd: MoveSpecimensCmd =>
          process(moveCmdToEvent(cmd))(applyMovedEvent)

        case cmd: SpecimenAssignPositionCmd =>
          processUpdateCmd(cmd, assignPositionCmdToEvent, applyPositionAssignedEvent)

        case cmd: SpecimenRemoveAmountCmd =>
          processUpdateCmd(cmd, removeAmountCmdToEvent, applyAmountRemovedEvent)

        case cmd: SpecimenUpdateUsableCmd =>
          processUpdateCmd(cmd, updateUsableCmdToEvent, applyUsableUpdatedEvent)

        case cmd: RemoveSpecimenCmd =>
          processUpdateCmd(cmd, removeCmdToEvent, applyRemovedEvent)
      }

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
      throw new Exception(
        "SpecimensProcessor: Intentionally throwing exception to test persistence by restarting the actor"
      )

    case msg =>
      log.error(s"specimensProcessor: message not handled: $msg")
  }

  private def mySaveSnapshot(): Unit = {
    val snapshotState =
      SnapshotState(specimenRepository.getValues.toSet, ceventSpecimenRepository.getValues.toSet)
    val filename = snapshotWriter.save(persistenceId, Json.toJson(snapshotState).toString)
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
          log.debug(s"snapshot contains ${snapshot.specimens.size} collection events")
          snapshot.specimens.foreach(specimenRepository.put)
          snapshot.ceventSpecimens.foreach(ceventSpecimenRepository.put)
        }
      )
  }

  private def addCmdToEvent(cmd: AddSpecimensCmd): ServiceValidation[SpecimenEvent] =
    for {
      collectionEvent <- collectionEventRepository.getByKey(CollectionEventId(cmd.collectionEventId))
      ceventType      <- collectionEventTypeRepository.getByKey(collectionEvent.collectionEventTypeId)
      specIdsValid    <- validateSpecimenInfo(cmd.specimenData, ceventType)
      invIdsValid     <- validateInventoryId(cmd.specimenData)
    } yield SpecimenEvent(cmd.sessionUserId).update(
      _.time                    := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
      _.added.collectionEventId := collectionEvent.id.id,
      _.added.specimenData := cmd.specimenData.map { specimenInfo =>
        specimenInfoToEvent(specimenRepository.nextIdentity, specimenInfo)
      }
    )

  //@silent
  private def moveCmdToEvent(cmd: MoveSpecimensCmd): ServiceValidation[SpecimenEvent] =
    ???

  //@silent
  private def assignPositionCmdToEvent(
      cmd:      SpecimenAssignPositionCmd,
      cevent:   CollectionEvent,
      specimen: Specimen
    ): ServiceValidation[SpecimenEvent] =
    ???

  //@silent
  private def removeAmountCmdToEvent(
      cmd:      SpecimenRemoveAmountCmd,
      cevent:   CollectionEvent,
      specimen: Specimen
    ): ServiceValidation[SpecimenEvent] =
    ???

  //@silent
  private def updateUsableCmdToEvent(
      cmd:      SpecimenUpdateUsableCmd,
      cevent:   CollectionEvent,
      specimen: Specimen
    ): ServiceValidation[SpecimenEvent] =
    ???

  private def removeCmdToEvent(
      cmd:      RemoveSpecimenCmd,
      cevent:   CollectionEvent,
      specimen: Specimen
    ): ServiceValidation[SpecimenEvent] =
    specimenHasNoChildren(specimen).map(
      _ =>
        SpecimenEvent(cmd.sessionUserId).update(
          _.time                      := OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
          _.removed.version           := specimen.version,
          _.removed.specimenId        := specimen.id.id,
          _.removed.collectionEventId := cevent.id.id
        )
    )

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def applyAddedEvent(event: SpecimenEvent): Unit = {
    val v = for {
      validEventType <- validEventType(event.eventType.isAdded)
      specimens <- {
        event.getAdded.specimenData.toList.traverseU { info =>
          UsableSpecimen.create(id                   = SpecimenId(info.getId),
                                version              = 0L,
                                inventoryId          = info.getInventoryId,
                                specimenDefinitionId = SpecimenDefinitionId(info.getSpecimenDefinitionId),
                                originLocationId     = LocationId(info.getLocationId),
                                locationId           = LocationId(info.getLocationId),
                                containerId          = None,
                                schemaLabel          = None,
                                timeAdded            = OffsetDateTime.parse(event.getTime),
                                timeCreated          = OffsetDateTime.parse(info.getTimeCreated),
                                amount               = BigDecimal(info.getAmount))
        }
      }
    } yield specimens

    if (v.isFailure) {
      log.error(s"*** ERROR ***: $v, event: $event: ")
    }

    v.foreach { specimens =>
      val ceventId = CollectionEventId(event.getAdded.getCollectionEventId)
      specimens.foreach { specimen =>
        specimenRepository.put(
          specimen.copy(slug = specimenRepository.uniqueSlugFromStr(specimen.inventoryId))
        )
        ceventSpecimenRepository.put(CeventSpecimen(ceventId, specimen.id))
      }
    }
  }

  //@silent
  private def applyMovedEvent(event: SpecimenEvent): Unit =
    ???

  //@silent
  private def applyPositionAssignedEvent(event: SpecimenEvent): Unit =
    ???

  //@silent
  private def applyAmountRemovedEvent(event: SpecimenEvent): Unit =
    ???

  //@silent
  private def applyUsableUpdatedEvent(event: SpecimenEvent): Unit =
    ???

  private def applyRemovedEvent(event: SpecimenEvent): Unit = {
    val v = for {
      validEventType <- validEventType(event.eventType.isRemoved)
      specimen       <- specimenRepository.getByKey(SpecimenId(event.getRemoved.getSpecimenId))
      validVersion   <- specimen.requireVersion(event.getRemoved.getVersion)
      collectionEvent <- collectionEventRepository.getByKey(
                          CollectionEventId(event.getRemoved.getCollectionEventId)
                        )
    } yield {
      ceventSpecimenRepository.remove(CeventSpecimen(collectionEvent.id, specimen.id))
      specimenRepository.remove(specimen)
    }

    if (v.isFailure) {
      log.error(s"*** ERROR ***: $v, event: $event: ")
    }
  }

  private def processUpdateCmd[T <: SpecimenModifyCommand](
      cmd:        T,
      validation: (T, CollectionEvent, Specimen) => ServiceValidation[SpecimenEvent],
      applyEvent: SpecimenEvent => Unit
    ): Unit = {

    val specimenId        = SpecimenId(cmd.id)
    val collectionEventId = CollectionEventId(cmd.collectionEventId)

    val event = for {
      pair         <- ceventSpecimenRepository.withSpecimenId(specimenId)
      specimen     <- specimenRepository.getByKey(specimenId)
      cevent       <- collectionEventRepository.getByKey(collectionEventId)
      validVersion <- specimen.requireVersion(cmd.expectedVersion)
      event        <- validation(cmd, cevent, specimen)
    } yield event
    process(event)(applyEvent)
  }

  private def validateSpecimenInfo(
      specimenData: List[SpecimenInfo],
      ceventType:   CollectionEventType
    ): ServiceValidation[Unit] = {

    val cmdSpcDefIds = specimenData.map(s => SpecimenDefinitionId(s.specimenDefinitionId)).toSet
    val ceventDefId  = ceventType.specimenDefinitions.map(s => s.id).toSet
    val notBelonging = cmdSpcDefIds.diff(ceventDefId)

    if (notBelonging.isEmpty) ().successNel[String]
    else
      EntityCriteriaError(
        "specimen descriptions do not belong to collection event type: "
          + notBelonging.mkString(", ")
      ).failureNel[Unit]
  }

  /**
   * Returns success if none of the inventory IDs are found in the repository.
   *
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def validateInventoryId(specimenData: List[SpecimenInfo]): ServiceValidation[Unit] =
    specimenData
      .map { info =>
        specimenRepository.getByInventoryId(info.inventoryId) fold (
          err => ().successNel[String],
          spc => s"specimen ID already in use: ${info.inventoryId}".failureNel[Unit]
        )
      }.sequenceU.map { x =>
        ()
      }

  private def validEventType(eventType: Boolean): ServiceValidation[Unit] =
    if (eventType) ().successNel[String]
    else s"invalid event type".failureNel[Unit]

  private def specimenHasNoChildren(specimen: Specimen): ServiceValidation[Unit] = {
    val children = processingEventInputSpecimenRepository.withSpecimenId(specimen.id)
    if (children.isEmpty) ().successNel[String]
    else ServiceError(s"specimen has child specimens: ${specimen.id}").failureNel[Unit]
  }

  private def init(): Unit = {
    ceventSpecimenRepository.init
    specimenRepository.init
  }

  init
}
