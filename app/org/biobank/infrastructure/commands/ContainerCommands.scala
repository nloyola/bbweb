package org.biobank.infrastructure.commands

import org.biobank.domain.PreservationTemperature._
import org.biobank.infrastructure.commands.Commands._
import play.api.libs.json._
import play.api.libs.json.Reads._

object ContainerCommands {

  trait HasParentIdentity {

    /** A command that includes the parent conatainer ID that it is related to. */
    val parentId: String

  }

  trait ContainerCommand extends Command with HasSessionUserId

  trait AddContainerCommand extends ContainerCommand

  trait AddRootContainerCommand extends AddContainerCommand with HasCentreIdentity

  trait AddSubContainerCommand extends AddContainerCommand with HasParentIdentity

  trait ContainerModifyCommand extends ContainerCommand with HasIdentity with HasExpectedVersion

  /**
   * The treeId field is filled  in by the service.
   */
  final case class AddRootContainerCmd(sessionUserId:   String,
                                       inventoryId:     String,
                                       label:           String,
                                       centreId:        String,
                                       locationId:      String,
                                       temperature:     PreservationTemperature,
                                       containerTypeId: String)
      extends AddContainerCommand

  final case class AddStorageContainerCmd(sessionUserId:     String,
                                          inventoryId:       String,
                                          label:             String,
                                          containerTypeId:   String,
                                          parentId:          String)
      extends AddSubContainerCommand

  final case class AddSpecimenContainerCmd(sessionUserId:     String,
                                           inventoryId:       String,
                                           label:             String,
                                           containerTypeId:   String,
                                           parentId:          String)
      extends AddSubContainerCommand

  final case class UpdateContainerCmd(sessionUserId:   String,
                                      id:              String,
                                      expectedVersion: Long,
                                      attribute:       String,
                                      value:           String)
      extends ContainerModifyCommand

  final case class RemoveContainerCmd(sessionUserId:   String,
                                      id:              String,
                                      expectedVersion: Long)
      extends ContainerModifyCommand

  implicit val addTopContainerCmdReads: Reads[AddRootContainerCmd] =
    Json.reads[AddRootContainerCmd]

  implicit val addStorageContainerCmdReads: Reads[AddStorageContainerCmd] =
    Json.reads[AddStorageContainerCmd]

  implicit val addSpecimenContainerCmdReads: Reads[AddSpecimenContainerCmd] =
    Json.reads[AddSpecimenContainerCmd]

  implicit val updateContainerCmdReads: Reads[UpdateContainerCmd] =
    Json.reads[UpdateContainerCmd]

  implicit val removeContainerCmdReads: Reads[RemoveContainerCmd] =
    Json.reads[RemoveContainerCmd]

}
