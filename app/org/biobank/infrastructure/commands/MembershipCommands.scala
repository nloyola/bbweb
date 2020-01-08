package org.biobank.infrastructure.commands

import Commands._

import play.api.libs.json._
import play.api.libs.json.Reads._

object MembershipCommands {

  trait MembershipCommand extends Command with HasSessionUserId

  trait MembershipModifyCommand extends MembershipCommand with HasExpectedVersion {
    val membershipId: String
  }

  final case class AddMembershipCmd(
      sessionUserId: String,
      name:          String,
      description:   Option[String],
      userIds:       List[String],
      allStudies:    Boolean,
      studyIds:      List[String],
      allCentres:    Boolean,
      centreIds:     List[String])
      extends MembershipCommand

  final case class MembershipUpdateNameCmd(
      sessionUserId:   String,
      expectedVersion: Long,
      membershipId:    String,
      name:            String)
      extends MembershipModifyCommand

  final case class MembershipUpdateDescriptionCmd(
      sessionUserId:   String,
      expectedVersion: Long,
      membershipId:    String,
      description:     Option[String])
      extends MembershipModifyCommand

  final case class MembershipAddUserCmd(
      sessionUserId:   String,
      expectedVersion: Long,
      membershipId:    String,
      userId:          String)
      extends MembershipModifyCommand

  final case class MembershipUpdateStudyDataCmd(
      sessionUserId:   String,
      expectedVersion: Long,
      membershipId:    String,
      allStudies:      Boolean,
      studyIds:        List[String])
      extends MembershipModifyCommand

  final case class MembershipAllStudiesCmd(sessionUserId: String, expectedVersion: Long, membershipId: String)
      extends MembershipModifyCommand

  final case class MembershipAddStudyCmd(
      sessionUserId:   String,
      expectedVersion: Long,
      membershipId:    String,
      studyId:         String)
      extends MembershipModifyCommand

  final case class MembershipUpdateCentreDataCmd(
      sessionUserId:   String,
      expectedVersion: Long,
      membershipId:    String,
      allCentres:      Boolean,
      centreIds:       List[String])
      extends MembershipModifyCommand

  final case class MembershipAllCentresCmd(sessionUserId: String, expectedVersion: Long, membershipId: String)
      extends MembershipModifyCommand

  final case class MembershipAddCentreCmd(
      sessionUserId:   String,
      expectedVersion: Long,
      membershipId:    String,
      centreId:        String)
      extends MembershipModifyCommand

  final case class MembershipRemoveUserCmd(
      sessionUserId:   String,
      expectedVersion: Long,
      membershipId:    String,
      userId:          String)
      extends MembershipModifyCommand

  final case class MembershipRemoveStudyCmd(
      sessionUserId:   String,
      expectedVersion: Long,
      membershipId:    String,
      studyId:         String)
      extends MembershipModifyCommand

  final case class MembershipRemoveCentreCmd(
      sessionUserId:   String,
      expectedVersion: Long,
      membershipId:    String,
      centreId:        String)
      extends MembershipModifyCommand

  final case class RemoveMembershipCmd(sessionUserId: String, expectedVersion: Long, membershipId: String)
      extends MembershipModifyCommand

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val addMembershipCmdReads: Reads[AddMembershipCmd] =
    Json.reads[AddMembershipCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val membershipUpdateNameCmdReads: Reads[MembershipUpdateNameCmd] =
    Json.reads[MembershipUpdateNameCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val membershipUpdateDescriptionCmdReads: Reads[MembershipUpdateDescriptionCmd] =
    Json.reads[MembershipUpdateDescriptionCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val membershipAddUserCmdReads: Reads[MembershipAddUserCmd] =
    Json.reads[MembershipAddUserCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val membershipUpdateStudyDataCmdReads: Reads[MembershipUpdateStudyDataCmd] =
    Json.reads[MembershipUpdateStudyDataCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val membershipAllStudiesCmdReads: Reads[MembershipAllStudiesCmd] =
    Json.reads[MembershipAllStudiesCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val membershipAddStudyCmdReads: Reads[MembershipAddStudyCmd] =
    Json.reads[MembershipAddStudyCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val membershipUpdateCentreDataCmdReads: Reads[MembershipUpdateCentreDataCmd] =
    Json.reads[MembershipUpdateCentreDataCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val membershipAllCentresCmdReads: Reads[MembershipAllCentresCmd] =
    Json.reads[MembershipAllCentresCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val membershipAddCentreCmdReads: Reads[MembershipAddCentreCmd] =
    Json.reads[MembershipAddCentreCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val membershipRemoveUserCmdReads: Reads[MembershipRemoveUserCmd] =
    Json.reads[MembershipRemoveUserCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val membershipRemoveStudyCmdReads: Reads[MembershipRemoveStudyCmd] =
    Json.reads[MembershipRemoveStudyCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val membershipRemoveCentreCmdReads: Reads[MembershipRemoveCentreCmd] =
    Json.reads[MembershipRemoveCentreCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val removeMembershipCmdReads: Reads[RemoveMembershipCmd] =
    Json.reads[RemoveMembershipCmd]

}
