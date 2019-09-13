package org.biobank.infrastructure.commands

import org.biobank.infrastructure.commands.Commands._
import play.api.libs.json._
import play.api.libs.json.Reads._

object ContainerTypeCommands {

  trait ContainerTypeCommand extends Command with HasSessionUserId

  trait ContainerTypeAddCommand extends ContainerTypeCommand {
    val sessionUserId: String
    val name:          String
    val description:   Option[String]
    val centreId:      String
    val schemaId:      String
    val shared:        Boolean
  }

  trait ContainerTypeModifyCommand extends ContainerTypeCommand with HasIdentity with HasExpectedVersion

  final case class AddStorageContainerTypeCmd(
      sessionUserId: String,
      name:          String,
      description:   Option[String],
      centreId:      String,
      schemaId:      String,
      shared:        Boolean)
      extends ContainerTypeAddCommand

  final case class AddSpecimenContainerTypeCmd(
      sessionUserId: String,
      name:          String,
      description:   Option[String],
      centreId:      String,
      schemaId:      String,
      shared:        Boolean)
      extends ContainerTypeAddCommand

  final case class UpdateContainerTypeNameCmd(
      sessionUserId:   String,
      id:              String, // shipment ID
      expectedVersion: Long,
      name:            String)
      extends ContainerTypeModifyCommand

  final case class UpdateContainerTypeDescriptionCmd(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      description:     Option[String])
      extends ContainerTypeModifyCommand

  final case class UpdateContainerTypeCentreCmd(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      centreId:        String)
      extends ContainerTypeModifyCommand

  final case class UpdateContainerTypeSchemaCmd(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      schemaId:        String)
      extends ContainerTypeModifyCommand

  final case class UpdateContainerTypeSharedCmd(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      shared:          Boolean)
      extends ContainerTypeModifyCommand

  final case class UpdateContainerTypeEnabledCmd(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      enabled:         Boolean)
      extends ContainerTypeModifyCommand

  final case class RemoveContainerTypeCmd(sessionUserId: String, id: String, expectedVersion: Long)
      extends ContainerTypeCommand

  implicit val addStorageContainerTypeCmdReads: Reads[AddStorageContainerTypeCmd] =
    Json.reads[AddStorageContainerTypeCmd]
  implicit val addSpecimenContainerTypeCmdReads: Reads[AddSpecimenContainerTypeCmd] =
    Json.reads[AddSpecimenContainerTypeCmd]
  implicit val updateContainerTypeNameCmdReads: Reads[UpdateContainerTypeNameCmd] =
    Json.reads[UpdateContainerTypeNameCmd]
  implicit val updateContainerTypeDescriptionCmdReads: Reads[UpdateContainerTypeDescriptionCmd] =
    Json.reads[UpdateContainerTypeDescriptionCmd]
  implicit val updateContainerTypeCentreCmdReads: Reads[UpdateContainerTypeCentreCmd] =
    Json.reads[UpdateContainerTypeCentreCmd]
  implicit val updateContainerTypeSchemaCmdReads: Reads[UpdateContainerTypeSchemaCmd] =
    Json.reads[UpdateContainerTypeSchemaCmd]
  implicit val updateContainerTypeSharedCmdReads: Reads[UpdateContainerTypeSharedCmd] =
    Json.reads[UpdateContainerTypeSharedCmd]
  implicit val updateContainerTypeEnabledCmdReads: Reads[UpdateContainerTypeEnabledCmd] =
    Json.reads[UpdateContainerTypeEnabledCmd]
  implicit val removeContainerTypeCmdReads: Reads[RemoveContainerTypeCmd] =
    Json.reads[RemoveContainerTypeCmd]

}
