package org.biobank.infrastructure.command

import org.biobank.domain.Annotation
import org.biobank.infrastructure.JsonUtils._

import Commands._

import play.api.libs.json._
import org.joda.time.DateTime

object CollectionEventCommands {

  trait CollectionEventCommand extends Command

  trait CollectionEventModifyCommand
      extends CollectionEventCommand
      with HasIdentity
      with HasExpectedVersion

  case class AddCollectionEventCmd(userId:                Option[String],
                                   participantId:         String,
                                   collectionEventTypeId: String,
                                   timeCompleted:         DateTime,
                                   visitNumber:           Int,
                                   annotations:           List[Annotation])
      extends CollectionEventCommand

  case class UpdateCollectionEventVisitNumberCmd(userId:          Option[String],
                                                 id:              String,
                                                 expectedVersion: Long,
                                                 visitNumber:     Int)
      extends CollectionEventModifyCommand

  case class UpdateCollectionEventTimeCompletedCmd(userId:          Option[String],
                                                   id:              String,
                                                   expectedVersion: Long,
                                                   timeCompleted:   DateTime)
      extends CollectionEventModifyCommand

  case class UpdateCollectionEventAnnotationCmd(userId:           Option[String],
                                                id:               String,
                                                expectedVersion:  Long,
                                                annotationTypeId: String,
                                                stringValue:      Option[String],
                                                numberValue:      Option[String],
                                                selectedValues:   Set[String])
      extends CollectionEventModifyCommand

  case class RemoveCollectionEventAnnotationCmd(userId:           Option[String],
                                                id:               String,
                                                expectedVersion:  Long,
                                                annotationTypeId: String)
      extends CollectionEventModifyCommand

  case class RemoveCollectionEventCmd(userId:          Option[String],
                                      id:              String,
                                      participantId:   String,
                                      expectedVersion: Long)
      extends CollectionEventModifyCommand

  implicit val addCollectionEventCmdReads                 = Json.reads[AddCollectionEventCmd]
  implicit val updateCollectionEventVisitNumberCmdReads   = Json.reads[UpdateCollectionEventVisitNumberCmd]
  implicit val updateCollectionEventTimeCompletedCmdReads = Json.reads[UpdateCollectionEventTimeCompletedCmd]
  implicit val updateCollectionEventAnnotationCmdReads    = Json.reads[UpdateCollectionEventAnnotationCmd]
  implicit val removeCollectionEventCmdReads              = Json.reads[RemoveCollectionEventCmd]

}
