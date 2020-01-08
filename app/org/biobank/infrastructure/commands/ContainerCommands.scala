package org.biobank.infrastructure.commands

import org.biobank.domain.AnatomicalSourceType._
import org.biobank.domain.PreservationTemperature._
import org.biobank.domain.PreservationType._
import org.biobank.domain.SpecimenType._
import org.biobank.infrastructure.commands.Commands._
import play.api.libs.json._
import play.api.libs.json.Reads._

object ContainerCommands {

  trait HasContainerTypeIdentity {

    /** A command that includes the container type ID that it is related to. */
    val containerTypeId: String

  }

  trait HasParentIdentity {

    /** A command that includes the parent container ID that it is related to. */
    val parentId: String

  }

  trait ContainerCommand extends Command with HasSessionUserId

  trait AddContainerCommand extends ContainerCommand with HasContainerTypeIdentity

  trait AddSubContainerCommand
      extends AddContainerCommand with HasContainerTypeIdentity with HasParentIdentity {
    val label:       String
    val inventoryId: String
  }

  trait ContainerModifyCommand extends ContainerCommand with HasIdentity with HasExpectedVersion

  final case class AddRootContainerCmd(
      sessionUserId:   String,
      inventoryId:     String,
      label:           String,
      centreId:        String,
      locationId:      String,
      temperature:     PreservationTemperature,
      containerTypeId: String)
      extends AddContainerCommand

  final case class AddStorageContainerCmd(
      sessionUserId:   String,
      label:           String,
      inventoryId:     String,
      containerTypeId: String,
      parentId:        String)
      extends AddSubContainerCommand

  final case class AddSpecimenContainerCmd(
      sessionUserId:   String,
      label:           String,
      inventoryId:     String,
      containerTypeId: String,
      parentId:        String)
      extends AddSubContainerCommand

  final case class UpdateContainerLabelCmd(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      label:           String)
      extends ContainerModifyCommand

  final case class UpdateContainerInventoryIdCmd(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      inventoryId:     String)
      extends ContainerModifyCommand

  final case class UpdateContainerEnabledCmd(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      enabled:         Boolean)
      extends ContainerModifyCommand

  final case class UpdateContainerContainerTypeCmd(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      containerTypeId: String)
      extends ContainerModifyCommand

  final case class UpdateContainerCentreLocationCmd(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      centreId:        String,
      locationId:      String)
      extends ContainerModifyCommand

  final case class UpdateContainerTemperatureCmd(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      temperature:     PreservationTemperature)
      extends ContainerModifyCommand

  final case class UpdateContainerConstraintsCmd(
      sessionUserId:     String,
      id:                String,
      expectedVersion:   Long,
      name:              String,
      description:       Option[String],
      anatomicalSources: Set[AnatomicalSourceType],
      preservationTypes: Set[PreservationType],
      specimenTypes:     Set[SpecimenType])
      extends ContainerModifyCommand

  final case class RemoveContainerConstraintsCmd(sessionUserId: String, id: String, expectedVersion: Long)
      extends ContainerModifyCommand

  final case class UpdateContainerPositionCmd(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      parentId:        String,
      label:           String)
      extends ContainerModifyCommand

  final case class RemoveContainerCmd(sessionUserId: String, id: String, expectedVersion: Long)
      extends ContainerModifyCommand

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val addTopContainerCmdReads: Reads[AddRootContainerCmd] =
    Json.reads[AddRootContainerCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val addStorageContainerCmdReads: Reads[AddStorageContainerCmd] =
    Json.reads[AddStorageContainerCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val addSpecimenContainerCmdReads: Reads[AddSpecimenContainerCmd] =
    Json.reads[AddSpecimenContainerCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val updateContainerLabelCmdReads: Reads[UpdateContainerLabelCmd] =
    Json.reads[UpdateContainerLabelCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val updateContainerInventoryIdCmdReads: Reads[UpdateContainerInventoryIdCmd] =
    Json.reads[UpdateContainerInventoryIdCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val updateContainerEnabledCmdReads: Reads[UpdateContainerEnabledCmd] =
    Json.reads[UpdateContainerEnabledCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val updateContainerContainerTypeCmdReads: Reads[UpdateContainerContainerTypeCmd] =
    Json.reads[UpdateContainerContainerTypeCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val updateContainerCentreLocationCmdReads: Reads[UpdateContainerCentreLocationCmd] =
    Json.reads[UpdateContainerCentreLocationCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val updateContainerTemperatureCmdReads: Reads[UpdateContainerTemperatureCmd] =
    Json.reads[UpdateContainerTemperatureCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val updateContainerConstraintsCmdReads: Reads[UpdateContainerConstraintsCmd] =
    Json.reads[UpdateContainerConstraintsCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val removeContainerConstraintsCmdReads: Reads[RemoveContainerConstraintsCmd] =
    Json.reads[RemoveContainerConstraintsCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val updateContainerPositionCmdReads: Reads[UpdateContainerPositionCmd] =
    Json.reads[UpdateContainerPositionCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val removeContainerCmdReads: Reads[RemoveContainerCmd] =
    Json.reads[RemoveContainerCmd]

}
