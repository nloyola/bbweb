package org.biobank.infrastructure.commands

import java.time.OffsetDateTime
import play.api.libs.json._

object ShipmentCommands {
  import org.biobank.infrastructure.commands.Commands._

  trait ShipmentCommand extends Command with HasSessionUserId

  trait ShipmentModifyCommand extends ShipmentCommand with HasIdentity with HasExpectedVersion

  final case class AddShipmentCmd(
      sessionUserId:         String,
      courierName:           String,
      trackingNumber:        String,
      originLocationId:      String,
      destinationLocationId: String)
      extends ShipmentCommand

  final case class UpdateShipmentCourierNameCmd(
      sessionUserId:   String,
      id:              String, // shipment ID
      expectedVersion: Long,
      courierName:     String)
      extends ShipmentModifyCommand

  final case class UpdateShipmentTrackingNumberCmd(
      sessionUserId:   String,
      id:              String, // shipment ID
      expectedVersion: Long,
      trackingNumber:  String)
      extends ShipmentModifyCommand

  final case class UpdateShipmentOriginCmd(
      sessionUserId:   String,
      id:              String, // shipment ID
      expectedVersion: Long,
      locationId:      String)
      extends ShipmentModifyCommand

  final case class UpdateShipmentDestinationCmd(
      sessionUserId:   String,
      id:              String, // shipment ID
      expectedVersion: Long,
      locationId:      String)
      extends ShipmentModifyCommand

  final case class CreatedShipmentCmd(
      sessionUserId:   String,
      id:              String, // shipment ID
      expectedVersion: Long)
      extends ShipmentModifyCommand

  final case class PackShipmentCmd(
      sessionUserId:   String,
      id:              String, // shipment ID
      expectedVersion: Long,
      datetime:        OffsetDateTime)
      extends ShipmentModifyCommand

  final case class SendShipmentCmd(
      sessionUserId:   String,
      id:              String, // shipment ID
      expectedVersion: Long,
      datetime:        OffsetDateTime)
      extends ShipmentModifyCommand

  final case class ReceiveShipmentCmd(
      sessionUserId:   String,
      id:              String, // shipment ID
      expectedVersion: Long,
      datetime:        OffsetDateTime)
      extends ShipmentModifyCommand

  final case class UnpackShipmentCmd(
      sessionUserId:   String,
      id:              String, // shipment ID
      expectedVersion: Long,
      datetime:        OffsetDateTime)
      extends ShipmentModifyCommand

  final case class CompleteShipmentCmd(
      sessionUserId:   String,
      id:              String, // shipment ID
      expectedVersion: Long,
      datetime:        OffsetDateTime)
      extends ShipmentModifyCommand

  final case class LostShipmentCmd(
      sessionUserId:   String,
      id:              String, // shipment ID
      expectedVersion: Long)
      extends ShipmentModifyCommand

  final case class ShipmentSkipStateToSentCmd(
      sessionUserId:   String,
      id:              String, // shipment ID
      expectedVersion: Long,
      timePacked:      OffsetDateTime,
      timeSent:        OffsetDateTime)
      extends ShipmentModifyCommand

  final case class ShipmentSkipStateToUnpackedCmd(
      sessionUserId:   String,
      id:              String, // shipment ID
      expectedVersion: Long,
      timeReceived:    OffsetDateTime,
      timeUnpacked:    OffsetDateTime)
      extends ShipmentModifyCommand

  final case class ShipmentRemoveCmd(
      sessionUserId:   String,
      id:              String, // shipment ID
      expectedVersion: Long)
      extends ShipmentModifyCommand

  final case class ShipmentsSnapshotCmd(sessionUserId: String) extends ShipmentCommand

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val addShipmentCmdReads: Reads[AddShipmentCmd] =
    Json.reads[AddShipmentCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val updateShipmentCourierNameCmdReads: Reads[UpdateShipmentCourierNameCmd] =
    Json.reads[UpdateShipmentCourierNameCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val updateShipmentTrackingNumberCmdReads: Reads[UpdateShipmentTrackingNumberCmd] =
    Json.reads[UpdateShipmentTrackingNumberCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val updateShipmentOriginLocationCmdReads: Reads[UpdateShipmentOriginCmd] =
    Json.reads[UpdateShipmentOriginCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val updateShipmentDestinationLocationCmdReads: Reads[UpdateShipmentDestinationCmd] =
    Json.reads[UpdateShipmentDestinationCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val createdShipmentCmdReads: Reads[CreatedShipmentCmd] =
    Json.reads[CreatedShipmentCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val packShipmentCmdReads: Reads[PackShipmentCmd] =
    Json.reads[PackShipmentCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val sendShipmentCmdReads: Reads[SendShipmentCmd] =
    Json.reads[SendShipmentCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val receiveShipmentCmdReads: Reads[ReceiveShipmentCmd] =
    Json.reads[ReceiveShipmentCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val unpackShipmentCmdReads: Reads[UnpackShipmentCmd] =
    Json.reads[UnpackShipmentCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val completeShipmentCmdReads: Reads[CompleteShipmentCmd] =
    Json.reads[CompleteShipmentCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val lostShipmentCmdReads: Reads[LostShipmentCmd] =
    Json.reads[LostShipmentCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val shipmentSkipStateToSentCmdReads: Reads[ShipmentSkipStateToSentCmd] =
    Json.reads[ShipmentSkipStateToSentCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val shipmentSkipStateToUnpackedCmdReads: Reads[ShipmentSkipStateToUnpackedCmd] =
    Json.reads[ShipmentSkipStateToUnpackedCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val shipmentRemoveCmdReads: Reads[ShipmentRemoveCmd] =
    Json.reads[ShipmentRemoveCmd]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val shipmentsSnapshotReads: Reads[ShipmentsSnapshotCmd] =
    Json.reads[ShipmentsSnapshotCmd]

}
