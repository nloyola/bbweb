package org.biobank.infrastructure.commands

import org.biobank.infrastructure.commands.Commands._
import play.api.libs.json._
import play.api.libs.json.Reads._

object ContainerSchemaCommands {

  trait ContainerSchemaCommand extends Command with HasSessionUserId

  trait ContainerSchemaModifyCommand extends ContainerSchemaCommand with HasIdentity with HasExpectedVersion

  final case class AddContainerSchemaCmd(
      sessionUserId: String,
      name:          String,
      description:   Option[String],
      shared:        Boolean,
      centreId:      String,
      labels:        List[String])
      extends ContainerSchemaCommand

  final case class UpdateContainerSchemaNameCmd(
      sessionUserId:   String,
      id:              String, // shipment ID
      expectedVersion: Long,
      name:            String)
      extends ContainerSchemaModifyCommand

  final case class UpdateContainerSchemaDescriptionCmd(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      description:     Option[String])
      extends ContainerSchemaModifyCommand

  final case class UpdateContainerSchemaSharedCmd(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      shared:          Boolean)
      extends ContainerSchemaModifyCommand

  final case class UpdateContainerSchemaCentreCmd(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      centreId:        String)
      extends ContainerSchemaModifyCommand

  final case class UpdateContainerSchemaLabelsCmd(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      labels:          List[String])
      extends ContainerSchemaModifyCommand

  final case class RemoveContainerSchemaCmd(sessionUserId: String, id: String, expectedVersion: Long)
      extends ContainerSchemaCommand

  implicit val addContainerSchemaCmdReads: Reads[AddContainerSchemaCmd] = Json.reads[AddContainerSchemaCmd]
  implicit val updateContainerSchemaNameCmdReads: Reads[UpdateContainerSchemaNameCmd] =
    Json.reads[UpdateContainerSchemaNameCmd]
  implicit val updateContainerSchemaDescriptionCmdReads: Reads[UpdateContainerSchemaDescriptionCmd] =
    Json.reads[UpdateContainerSchemaDescriptionCmd]
  implicit val updateContainerSchemaSharedCmdReads: Reads[UpdateContainerSchemaSharedCmd] =
    Json.reads[UpdateContainerSchemaSharedCmd]
  implicit val updateContainerSchemaCentreCmdReads: Reads[UpdateContainerSchemaCentreCmd] =
    Json.reads[UpdateContainerSchemaCentreCmd]
  implicit val updateContainerSchemaLabelsCmdReads: Reads[UpdateContainerSchemaLabelsCmd] =
    Json.reads[UpdateContainerSchemaLabelsCmd]
  implicit val removeContainerSchemaCmdReads: Reads[RemoveContainerSchemaCmd] =
    Json.reads[RemoveContainerSchemaCmd]

}