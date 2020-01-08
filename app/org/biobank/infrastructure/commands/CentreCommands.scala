package org.biobank.infrastructure.commands

import org.biobank.infrastructure.commands.Commands._

import play.api.libs.json._
import play.api.libs.json.Reads._

object CentreCommands {

  trait CentreCommand extends Command with HasSessionUserId

  trait CentreModifyCommand extends CentreCommand with HasIdentity with HasExpectedVersion

  trait CentreStateChangeCommand extends CentreModifyCommand

  trait CentreCommandWithCentreId extends CentreCommand with HasCentreIdentity

  final case class AddCentreCmd(sessionUserId: String, name: String, description: Option[String])
      extends CentreCommand

  final case class UpdateCentreNameCmd(sessionUserId: String, id: String, expectedVersion: Long, name: String)
      extends CentreModifyCommand

  final case class UpdateCentreDescriptionCmd(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      description:     Option[String])
      extends CentreModifyCommand

  final case class AddCentreLocationCmd(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      name:            String,
      street:          String,
      city:            String,
      province:        String,
      postalCode:      String,
      poBoxNumber:     Option[String],
      countryIsoCode:  String)
      extends CentreModifyCommand

  final case class UpdateCentreLocationCmd(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      locationId:      String,
      name:            String,
      street:          String,
      city:            String,
      province:        String,
      postalCode:      String,
      poBoxNumber:     Option[String],
      countryIsoCode:  String)
      extends CentreModifyCommand

  final case class RemoveCentreLocationCmd(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      locationId:      String)
      extends CentreModifyCommand

  trait CentreStudyCmd extends CentreCommandWithCentreId

  final case class AddStudyToCentreCmd(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      studyId:         String)
      extends CentreModifyCommand

  final case class RemoveStudyFromCentreCmd(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      studyId:         String)
      extends CentreModifyCommand

  final case class EnableCentreCmd(sessionUserId: String, id: String, expectedVersion: Long)
      extends CentreStateChangeCommand

  final case class DisableCentreCmd(sessionUserId: String, id: String, expectedVersion: Long)
      extends CentreStateChangeCommand

  final case class SearchCentreLocationsCmd(sessionUserId: String, filter: String, limit: Int) extends Command

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val addCentreCmdReads: Reads[AddCentreCmd] =
    Json.reads[AddCentreCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val updateCentreNameCmdReads: Reads[UpdateCentreNameCmd] =
    Json.reads[UpdateCentreNameCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val updateCentreDescriptionCmdReads: Reads[UpdateCentreDescriptionCmd] =
    Json.reads[UpdateCentreDescriptionCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val enableCentreCmdReads: Reads[EnableCentreCmd] =
    Json.reads[EnableCentreCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val disableCentreCmdReads: Reads[DisableCentreCmd] =
    Json.reads[DisableCentreCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val addCentreLocationCmdReads: Reads[AddCentreLocationCmd] =
    Json.reads[AddCentreLocationCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val updateCentreLocationCmdReads: Reads[UpdateCentreLocationCmd] =
    Json.reads[UpdateCentreLocationCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val removeCentreLocationCmdReads: Reads[RemoveCentreLocationCmd] =
    Json.reads[RemoveCentreLocationCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val addStudyToCentreCmdReads: Reads[AddStudyToCentreCmd] =
    Json.reads[AddStudyToCentreCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val removeStudyFromCentreCmdReads: Reads[RemoveStudyFromCentreCmd] =
    Json.reads[RemoveStudyFromCentreCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val searchCentreLocationsCmdReads: Reads[SearchCentreLocationsCmd] =
    Json.reads[SearchCentreLocationsCmd]

}
