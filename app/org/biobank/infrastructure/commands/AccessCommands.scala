package org.biobank.infrastructure.commands

import Commands._

import play.api.libs.json._
import play.api.libs.json.Reads._

object AccessCommands {

  trait AccessCommand extends Command with HasSessionUserId

  trait AccessModifyCommand extends AccessCommand with HasExpectedVersion

  trait RoleModifyCommand extends AccessModifyCommand {
    val roleId:          String
    val expectedVersion: Long
  }

  trait RoleCommand extends AccessCommand

  final case class AddRoleCmd(
      sessionUserId: String,
      name:          String,
      description:   Option[String],
      userIds:       List[String],
      parentIds:     List[String],
      childrenIds:   List[String])
      extends RoleCommand

  final case class RoleUpdateNameCmd(
      sessionUserId:   String,
      expectedVersion: Long,
      roleId:          String,
      name:            String)
      extends RoleModifyCommand

  final case class RoleUpdateDescriptionCmd(
      sessionUserId:   String,
      expectedVersion: Long,
      roleId:          String,
      description:     Option[String])
      extends RoleModifyCommand

  final case class RoleAddUserCmd(
      sessionUserId:   String,
      expectedVersion: Long,
      roleId:          String,
      userId:          String)
      extends RoleModifyCommand

  final case class RoleRemoveUserCmd(
      sessionUserId:   String,
      expectedVersion: Long,
      roleId:          String,
      userId:          String)
      extends RoleModifyCommand

  final case class RoleAddParentCmd(
      sessionUserId:   String,
      expectedVersion: Long,
      roleId:          String,
      parentRoleId:    String)
      extends RoleModifyCommand

  final case class RoleRemoveParentCmd(
      sessionUserId:   String,
      expectedVersion: Long,
      roleId:          String,
      parentRoleId:    String)
      extends RoleModifyCommand

  final case class RoleAddChildCmd(
      sessionUserId:   String,
      expectedVersion: Long,
      roleId:          String,
      childRoleId:     String)
      extends RoleModifyCommand

  final case class RoleRemoveChildCmd(
      sessionUserId:   String,
      expectedVersion: Long,
      roleId:          String,
      childRoleId:     String)
      extends RoleModifyCommand

  final case class RemoveRoleCmd(sessionUserId: String, expectedVersion: Long, roleId: String)
      extends RoleModifyCommand

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val addRoleCmdReads: Reads[AddRoleCmd] = Json.reads[AddRoleCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val roleUpdateNameCmdReads: Reads[RoleUpdateNameCmd] = Json.reads[RoleUpdateNameCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val roleUpdateDescriptionCmdReads: Reads[RoleUpdateDescriptionCmd] =
    Json.reads[RoleUpdateDescriptionCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val roleAddUserCmdReads: Reads[RoleAddUserCmd] = Json.reads[RoleAddUserCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val roleAddParentCmdReads: Reads[RoleAddParentCmd] = Json.reads[RoleAddParentCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val roleAddChildCmdReads: Reads[RoleAddChildCmd] = Json.reads[RoleAddChildCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val roleRemoveUserCmdReads: Reads[RoleRemoveUserCmd] = Json.reads[RoleRemoveUserCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val roleRemoveParentCmdReads: Reads[RoleRemoveParentCmd] = Json.reads[RoleRemoveParentCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val roleRemoveChildCmdReads: Reads[RoleRemoveChildCmd] = Json.reads[RoleRemoveChildCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val removeRoleCmdReads: Reads[RemoveRoleCmd] = Json.reads[RemoveRoleCmd]

}
