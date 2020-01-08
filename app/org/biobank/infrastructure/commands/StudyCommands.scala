package org.biobank.infrastructure.commands

import org.biobank.domain.annotations.AnnotationValueType._
import play.api.libs.json.Reads._
import play.api.libs.json._

object StudyCommands {
  import org.biobank.infrastructure.commands.Commands._

  // study commands
  trait StudyCommand extends Command with HasOptionalSessionUserId

  trait StudyModifyCommand extends StudyCommand with HasIdentity with HasExpectedVersion

  trait StudyStateChangeCommand extends StudyModifyCommand

  trait StudyCommandWithStudyId extends StudyCommand with HasStudyIdentity

  trait StudyModifyCommandWithStudyId
      extends StudyCommand with HasStudyIdentity with HasIdentity with HasExpectedVersion

  final case class AddStudyCmd(sessionUserId: Option[String], name: String, description: Option[String])
      extends StudyCommand

  final case class UpdateStudyNameCmd(
      sessionUserId:   Option[String],
      id:              String,
      expectedVersion: Long,
      name:            String)
      extends StudyModifyCommand

  final case class UpdateStudyDescriptionCmd(
      sessionUserId:   Option[String],
      id:              String,
      expectedVersion: Long,
      description:     Option[String])
      extends StudyModifyCommand

  final case class StudyAddParticipantAnnotationTypeCmd(
      sessionUserId:   Option[String],
      id:              String,
      expectedVersion: Long,
      name:            String,
      description:     Option[String],
      valueType:       AnnotationValueType,
      maxValueCount:   Option[Int],
      options:         Seq[String],
      required:        Boolean)
      extends StudyModifyCommand

  final case class StudyUpdateParticipantAnnotationTypeCmd(
      sessionUserId:    Option[String],
      id:               String,
      annotationTypeId: String,
      expectedVersion:  Long,
      name:             String,
      description:      Option[String],
      valueType:        AnnotationValueType,
      maxValueCount:    Option[Int],
      options:          Seq[String],
      required:         Boolean)
      extends StudyModifyCommand

  final case class UpdateStudyRemoveAnnotationTypeCmd(
      sessionUserId:    Option[String],
      id:               String,
      expectedVersion:  Long,
      annotationTypeId: String)
      extends StudyModifyCommand

  final case class EnableStudyCmd(sessionUserId: Option[String], id: String, expectedVersion: Long)
      extends StudyStateChangeCommand

  final case class DisableStudyCmd(sessionUserId: Option[String], id: String, expectedVersion: Long)
      extends StudyStateChangeCommand

  final case class RetireStudyCmd(sessionUserId: Option[String], id: String, expectedVersion: Long)
      extends StudyStateChangeCommand

  final case class UnretireStudyCmd(sessionUserId: Option[String], id: String, expectedVersion: Long)
      extends StudyStateChangeCommand

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val addStudyCmdReads: Reads[AddStudyCmd] =
    Json.reads[AddStudyCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val updateStudyNameCmdReads: Reads[UpdateStudyNameCmd] =
    Json.reads[UpdateStudyNameCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val updateStudyDescriptionCmdReads: Reads[UpdateStudyDescriptionCmd] =
    Json.reads[UpdateStudyDescriptionCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val studyAddParticipantAnnotationTypeCmdReads: Reads[StudyAddParticipantAnnotationTypeCmd] =
    Json.reads[StudyAddParticipantAnnotationTypeCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val studyUpdateParticipantAnnotationTypeCmdReads: Reads[StudyUpdateParticipantAnnotationTypeCmd] =
    Json.reads[StudyUpdateParticipantAnnotationTypeCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val updateStudyRemoveAnnotationTypeCmdReads: Reads[UpdateStudyRemoveAnnotationTypeCmd] =
    Json.reads[UpdateStudyRemoveAnnotationTypeCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val enableStudyCmdReads: Reads[EnableStudyCmd] =
    Json.reads[EnableStudyCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val disableStudyCmdReads: Reads[DisableStudyCmd] =
    Json.reads[DisableStudyCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val retireStudyCmdReads: Reads[RetireStudyCmd] =
    Json.reads[RetireStudyCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val unretireStudyCmdReads: Reads[UnretireStudyCmd] =
    Json.reads[UnretireStudyCmd]

}
