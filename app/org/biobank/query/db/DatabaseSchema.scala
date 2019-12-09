package org.biobank.query.db

import java.sql.Timestamp
import java.time.{OffsetDateTime, ZoneId}
import org.biobank.domain.LocationId
import org.biobank.domain.centres._
import org.biobank.domain.containers._
import org.biobank.domain.participants._
import slick.jdbc.JdbcProfile
import slick.lifted.ProvenShape
import play.api.db.slick.DatabaseConfigProvider
import play.api.db.slick.HasDatabaseConfigProvider

trait DatabaseSchema extends HasDatabaseConfigProvider[JdbcProfile] {
  protected val dbConfigProvider: DatabaseConfigProvider

  import dbConfig.profile.api._

  implicit val CentreIdMapper =
    MappedColumnType.base[CentreId, String](_.id, CentreId.apply)

  implicit val LocationIdMapper =
    MappedColumnType.base[LocationId, String](_.id, LocationId.apply)

  implicit val ShipmentIdMapper =
    MappedColumnType.base[ShipmentId, String](_.id, ShipmentId.apply)

  implicit val ShipmentSpecimenIdMapper =
    MappedColumnType.base[ShipmentSpecimenId, String](_.id, ShipmentSpecimenId.apply)

  implicit val ShipmentItemStateMapper =
    MappedColumnType.base[ShipmentItemState.Value, String](_.toString, ShipmentItemState.withName)

  implicit val SpecimenIdMapper =
    MappedColumnType.base[SpecimenId, String](_.id, SpecimenId.apply)

  implicit val ContainerIdMapper =
    MappedColumnType.base[ContainerId, String](_.id, ContainerId.apply)

  implicit val ShipmentContainerIdMapper =
    MappedColumnType.base[ShipmentContainerId, String](_.id, ShipmentContainerId.apply)

  implicit val JavaOffsetDateTimeMapper = MappedColumnType.base[OffsetDateTime, Timestamp](
    offsetDateTime => Timestamp.from(offsetDateTime.toInstant()),
    timestamp      => OffsetDateTime.ofInstant(timestamp.toInstant(), ZoneId.of("UTC"))
  )

  class SequenceNumbers(tag: Tag) extends Table[SequenceNumber](tag, "SEQUENCE_NUMBERS") {
    def persistenceId  = column[String]("PERSITENCE_ID", O.PrimaryKey)
    def sequenceNumber = column[Long]("SEQUENCE_NUMBER")

    def * : ProvenShape[SequenceNumber] =
      (persistenceId, sequenceNumber) <> (SequenceNumber.tupled, SequenceNumber.unapply)
  }

  val sequenceNumbers = TableQuery[SequenceNumbers]

  class Shipments(tag: Tag) extends Table[Shipment](tag, "SHIPMENTS") {
    def id                    = column[ShipmentId]("ID", O.PrimaryKey)
    def version               = column[Long]("VERSION")
    def timeAdded             = column[OffsetDateTime]("TIME_ADDED")
    def timeModified          = column[Option[OffsetDateTime]]("TIME_MODIFIED")
    def state                 = column[String]("STATE")
    def courierName           = column[String]("COURIER_NAME")
    def trackingNumber        = column[String]("TRACKING_NUMBER")
    def originCentreId        = column[CentreId]("ORIGIN_CENTRE_ID")
    def originLocationId      = column[LocationId]("ORIGIN_LOCATION_ID")
    def destinationCentreId   = column[CentreId]("DESTINATION_CENTRE_ID")
    def destinationLocationId = column[LocationId]("DESTINATION_LOCATION_ID")
    def timePacked            = column[Option[OffsetDateTime]]("TIME_PACKED")
    def timeSent              = column[Option[OffsetDateTime]]("TIME_SENT")
    def timeReceived          = column[Option[OffsetDateTime]]("TIME_RECEIVED")
    def timeUnpacked          = column[Option[OffsetDateTime]]("TIME_UNPACKED")
    def timeCompleted         = column[Option[OffsetDateTime]]("TIME_COMPLETED")

    def * : ProvenShape[Shipment] =
      (id,
       version,
       timeAdded,
       timeModified,
       state,
       courierName,
       trackingNumber,
       originCentreId,
       originLocationId,
       destinationCentreId,
       destinationLocationId,
       timePacked,
       timeSent,
       timeReceived,
       timeUnpacked,
       timeCompleted) <> (toShipment, fromShipment)

    def toShipment(
        tuple: (ShipmentId, Long, OffsetDateTime, Option[OffsetDateTime], String, String, String, CentreId,
            LocationId, CentreId, LocationId, Option[OffsetDateTime], Option[OffsetDateTime],
            Option[OffsetDateTime], Option[OffsetDateTime], Option[OffsetDateTime])
      ): Shipment = {
      // all but fifth element of "tuple""
      val attrsTuple = (tuple._1,
                        tuple._2,
                        tuple._3,
                        tuple._4,
                        tuple._6,
                        tuple._7,
                        tuple._8,
                        tuple._9,
                        tuple._10,
                        tuple._11,
                        tuple._12,
                        tuple._13,
                        tuple._14,
                        tuple._15,
                        tuple._16)
      tuple._5 match {
        case Shipment.createdState.id   => (CreatedShipment.apply _).tupled(attrsTuple)
        case Shipment.packedState.id    => (PackedShipment.apply _).tupled(attrsTuple)
        case Shipment.sentState.id      => (SentShipment.apply _).tupled(attrsTuple)
        case Shipment.receivedState.id  => (ReceivedShipment.apply _).tupled(attrsTuple)
        case Shipment.unpackedState.id  => (UnpackedShipment.apply _).tupled(attrsTuple)
        case Shipment.completedState.id => (CompletedShipment.apply _).tupled(attrsTuple)
        case Shipment.lostState.id      => (LostShipment.apply _).tupled(attrsTuple)
      }
    }

    def fromShipment(shipment: Shipment): Option[
      (ShipmentId,
       Long,
       OffsetDateTime,
       Option[OffsetDateTime],
       String,
       String,
       String,
       CentreId,
       LocationId,
       CentreId,
       LocationId,
       Option[OffsetDateTime],
       Option[OffsetDateTime],
       Option[OffsetDateTime],
       Option[OffsetDateTime],
       Option[OffsetDateTime])
    ] =
      Some(
        (shipment.id,
         shipment.version,
         shipment.timeAdded,
         shipment.timeModified,
         shipment.state.id,
         shipment.courierName,
         shipment.trackingNumber,
         shipment.originCentreId,
         shipment.originLocationId,
         shipment.destinationCentreId,
         shipment.destinationLocationId,
         shipment.timePacked,
         shipment.timeSent,
         shipment.timeReceived,
         shipment.timeUnpacked,
         shipment.timeCompleted)
      )

  }

  val shipments = TableQuery[Shipments]

  class ShipmentSpecimens(tag: Tag) extends Table[ShipmentSpecimen](tag, "SHIPMENT_SPECMENS") {
    def id                  = column[ShipmentSpecimenId]("ID", O.PrimaryKey)
    def version             = column[Long]("VERSION")
    def timeAdded           = column[OffsetDateTime]("TIME_ADDED")
    def timeModified        = column[Option[OffsetDateTime]]("TIME_MODIFIED")
    def shipmentId          = column[ShipmentId]("SHIPMENT_ID")
    def specimenId          = column[SpecimenId]("SPECIMEN_ID")
    def state               = column[ShipmentItemState.Value]("STATE")
    def shipmentContainerId = column[Option[ShipmentContainerId]]("SHIPMENT_CONTAINER_ID")

    def * : ProvenShape[ShipmentSpecimen] =
      (id, version, timeAdded, timeModified, shipmentId, specimenId, state, shipmentContainerId) <> ((ShipmentSpecimen.apply _).tupled, ShipmentSpecimen.unapply)
  }

  val shipmentSpecimens = TableQuery[ShipmentSpecimens]
}
