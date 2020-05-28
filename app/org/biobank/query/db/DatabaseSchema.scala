package org.biobank.query.db

import java.sql.Timestamp
import java.time.{OffsetDateTime, ZoneId}
import org.biobank.domain.{EntityState, LocationId, Slug}
import org.biobank.domain.centres._
import org.biobank.domain.containers._
import org.biobank.domain.participants._
import org.biobank.dto._
import org.biobank.dto.centres._
import slick.jdbc.JdbcProfile
import slick.lifted.ProvenShape
import play.api.db.slick.DatabaseConfigProvider
import play.api.db.slick.HasDatabaseConfigProvider
import shapeless.{::, HNil}
import slickless._

trait DatabaseSchema extends HasDatabaseConfigProvider[JdbcProfile] {
  protected val dbConfigProvider: DatabaseConfigProvider

  import dbConfig.profile.api._

  implicit val EntityStateMapper =
    MappedColumnType.base[EntityState, String](_.id, new EntityState(_))

  implicit val SlugMapper =
    MappedColumnType.base[Slug, String](_.id, Slug.apply)

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

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  class SequenceNumbers(tag: Tag) extends Table[SequenceNumber](tag, "SEQUENCE_NUMBERS") {
    def persistenceId  = column[String]("PERSITENCE_ID", O.PrimaryKey)
    def sequenceNumber = column[Long]("SEQUENCE_NUMBER")

    def * : ProvenShape[SequenceNumber] =
      (persistenceId, sequenceNumber) <> (SequenceNumber.tupled, SequenceNumber.unapply)
  }

  val sequenceNumbers = TableQuery[SequenceNumbers]

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  class Shipments(tag: Tag) extends Table[ShipmentDto](tag, "SHIPMENTS") {
    def id                      = column[ShipmentId]("ID", O.PrimaryKey)
    def version                 = column[Long]("VERSION")
    def timeAdded               = column[OffsetDateTime]("TIME_ADDED")
    def timeModified            = column[Option[OffsetDateTime]]("TIME_MODIFIED")
    def state                   = column[EntityState]("STATE")
    def courierName             = column[String]("COURIER_NAME")
    def trackingNumber          = column[String]("TRACKING_NUMBER")
    def originCentreId          = column[CentreId]("ORIGIN_CENTRE_ID")
    def originCentreSlug        = column[Slug]("ORIGIN_CENTRE_SLUG")
    def originCentreName        = column[String]("ORIGIN_CENTRE_NAME")
    def originLocationId        = column[LocationId]("ORIGIN_LOCATION_ID")
    def originLocationSlug      = column[Slug]("ORIGIN_LOCATION_SLUG")
    def originLocationName      = column[String]("ORIGIN_LOCATION_NAME")
    def destinationCentreId     = column[CentreId]("DESTINATION_CENTRE_ID")
    def destinationCentreSlug   = column[Slug]("DESTINATION_CENTRE_SLUG")
    def destinationCentreName   = column[String]("DESTINATION_CENTRE_NAME")
    def destinationLocationId   = column[LocationId]("DESTINATION_LOCATION_ID")
    def destinationLocationSlug = column[Slug]("DESTINATION_LOCATION_SLUG")
    def destinationLocationName = column[String]("DESTINATION_LOCATION_NAME")
    def timePacked              = column[Option[OffsetDateTime]]("TIME_PACKED")
    def timeSent                = column[Option[OffsetDateTime]]("TIME_SENT")
    def timeReceived            = column[Option[OffsetDateTime]]("TIME_RECEIVED")
    def timeUnpacked            = column[Option[OffsetDateTime]]("TIME_UNPACKED")
    def timeCompleted           = column[Option[OffsetDateTime]]("TIME_COMPLETED")
    def specimenCount           = column[Int]("SPECIMEN_COUNT")
    def presentSpecimenCount    = column[Int]("PRESENT_SPECIMEN_COUNT")
    def containerCount          = column[Int]("CONTAINER_COUNT")

    def * =
      (id ::
        version ::
        timeAdded ::
        timeModified ::
        state ::
        courierName ::
        trackingNumber ::
        originCentreId ::
        originCentreSlug ::
        originCentreName ::
        originLocationId ::
        originLocationSlug ::
        originLocationName ::
        destinationCentreId ::
        destinationCentreSlug ::
        destinationCentreName ::
        destinationLocationId ::
        destinationLocationSlug ::
        destinationLocationName ::
        timePacked ::
        timeSent ::
        timeReceived ::
        timeUnpacked ::
        timeCompleted ::
        specimenCount ::
        presentSpecimenCount ::
        containerCount ::
        HNil) <> (toShipment, fromShipment)

    type ShipmentDtoHList =
      ShipmentId :: // id
      Long :: // version
      OffsetDateTime :: // timeAdded
      Option[OffsetDateTime] :: // timeModified
      EntityState :: // state
      String :: // courierName
      String :: // trackingNumber
      CentreId :: // origin centre id
      Slug :: // origin centre slug
      String :: // origin centre name
      LocationId :: // origin location id
      Slug :: // origin location slug
      String :: // origin location name
      CentreId :: // destination centre id
      Slug :: // destination centre slug
      String :: // destination centre name
      LocationId :: // destination location id
      Slug :: // destination location slug
      String :: // destination location name
      Option[OffsetDateTime] :: // timePacked
      Option[OffsetDateTime] :: // timeSent
      Option[OffsetDateTime] :: // timeReceived
      Option[OffsetDateTime] :: // timeUnpacked
      Option[OffsetDateTime] :: // timeCompleted
      Int :: // specimenCount
      Int :: // presentSpecimenCount
      Int :: // containerCount
      HNil

    def toShipment(hlist: ShipmentDtoHList): ShipmentDto = hlist match {
      case id ::
            version ::
            timeAdded ::
            timeModified ::
            state ::
            courierName ::
            trackingNumber ::
            originCentreId ::
            originCentreSlug ::
            originCentreName ::
            originLocationId ::
            originLocationSlug ::
            originLocationName ::
            destinationCentreId ::
            destinationCentreSlug ::
            destinationCentreName ::
            destinationLocationId ::
            destinationLocationSlug ::
            destinationLocationName ::
            timePacked ::
            timeSent ::
            timeReceived ::
            timeUnpacked ::
            timeCompleted ::
            specimenCount ::
            presentSpecimenCount ::
            containerCount ::
            HNil =>
        val origin = CentreLocationInfo(originCentreId,
                                        originCentreSlug,
                                        originCentreName,
                                        LocationInfoDto(originLocationId,
                                                        originLocationSlug,
                                                        originLocationName),
                                        s"$originCentreName:$originLocationName")
        val destination = CentreLocationInfo(destinationCentreId,
                                             destinationCentreSlug,
                                             destinationCentreName,
                                             LocationInfoDto(destinationLocationId,
                                                             destinationLocationSlug,
                                                             destinationLocationName),
                                             s"$destinationCentreName:$destinationLocationName")

        ShipmentDto(id,
                    version,
                    timeAdded,
                    timeModified,
                    state,
                    courierName,
                    trackingNumber,
                    origin,
                    destination,
                    timePacked,
                    timeSent,
                    timeReceived,
                    timeUnpacked,
                    timeCompleted,
                    specimenCount,
                    presentSpecimenCount,
                    containerCount)

    }

    def fromShipment(shipment: ShipmentDto): Option[ShipmentDtoHList] = {
      Some(
        shipment.id ::
          shipment.version ::
          shipment.timeAdded ::
          shipment.timeModified ::
          shipment.state ::
          shipment.courierName ::
          shipment.trackingNumber ::
          shipment.origin.id ::
          shipment.origin.slug ::
          shipment.origin.name ::
          shipment.origin.location.id ::
          shipment.origin.location.slug ::
          shipment.origin.location.name ::
          shipment.destination.id ::
          shipment.destination.slug ::
          shipment.destination.name ::
          shipment.destination.location.id ::
          shipment.destination.location.slug ::
          shipment.destination.location.name ::
          shipment.timePacked ::
          shipment.timeSent ::
          shipment.timeReceived ::
          shipment.timeUnpacked ::
          shipment.timeCompleted ::
          shipment.specimenCount ::
          shipment.presentSpecimenCount ::
          shipment.containerCount ::
          HNil
      )
    }
  }

  val shipments = TableQuery[Shipments]

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  class ShipmentSpecimens(tag: Tag) extends Table[ShipmentSpecimenDto](tag, "SHIPMENT_SPECIMENS") {
    def id                  = column[ShipmentSpecimenId]("ID", O.PrimaryKey)
    def version             = column[Long]("VERSION")
    def timeAdded           = column[OffsetDateTime]("TIME_ADDED")
    def timeModified        = column[Option[OffsetDateTime]]("TIME_MODIFIED")
    def state               = column[EntityState]("STATE")
    def shipmentId          = column[ShipmentId]("SHIPMENT_ID")
    def specimenId          = column[SpecimenId]("SPECIMEN_ID")
    def shipmentContainerId = column[Option[ShipmentContainerId]]("SHIPMENT_CONTAINER_ID")

    def shipment = foreignKey("shipment_id", shipmentId, shipments)(_.id, onDelete = ForeignKeyAction.Cascade)

    def * : ProvenShape[ShipmentSpecimenDto] =
      (id, version, timeAdded, timeModified, state, shipmentId, specimenId, shipmentContainerId) <> ((ShipmentSpecimenDto.apply _).tupled, ShipmentSpecimenDto.unapply)
  }

  val shipmentSpecimens = TableQuery[ShipmentSpecimens]
}
