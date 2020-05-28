package org.biobank.domain.centres

import cats.data.Validated._
import cats.implicits._
import java.time.OffsetDateTime
import org.biobank.domain._
import org.slf4j.{Logger, LoggerFactory}
import play.api.libs.json._
import org.biobank.validation.Validation._

trait ShipmentValidations {

  case object CourierNameEmpty extends ValidationError {
    def errorMessage: String = "courier name is empty"
  }

  case object TrackingNumberEmpty extends ValidationError {
    def errorMessage: String = "tracking number is empty"
  }

  case object OriginCentreIdEmpty extends ValidationError {
    def errorMessage: String = "origin centre id name is empty"
  }

  case object OriginLocationIdEmpty extends ValidationError {
    def errorMessage: String = "origin location id name is empty"
  }

  case object DestinationCentreIdEmpty extends ValidationError {
    def errorMessage: String = "destination centre id name is empty"
  }

  case object DestinationLocationIdEmpty extends ValidationError {
    def errorMessage: String = "destination location id name is empty"
  }

  case class InvalidState(msg: String) extends ValidationError {
    def errorMessage: String = msg
  }

  case object TimePackedUndefined extends ValidationError {
    def errorMessage: String = "packed time does not exist"
  }

  case object TimeSentBeforePacked extends ValidationError {
    def errorMessage: String = "sent time is before packed time is invalid"
  }

  case object TimeSentUndefined extends ValidationError {
    def errorMessage: String = "sent time does not exist"
  }

  case object TimeReceivedBeforeSent extends ValidationError {
    def errorMessage: String = "received time before sent time is invalid"
  }

  case object TimeReceivedUndefined extends ValidationError {
    def errorMessage: String = "received time does not exist"
  }

  case object TimeUnpackedBeforeReceived extends ValidationError {
    def errorMessage: String = "unpacked time before received time is invalid"
  }

  case object TimeUnpackedUndefined extends ValidationError {
    def errorMessage: String = "unpacked time does not exist"
  }

  case object TimeCompletedBeforeUnpacked extends ValidationError {
    def errorMessage: String = "completed time before unpacked time is inalid"
  }

  def validateTimeAfter(
      afterMaybe:   Option[OffsetDateTime],
      beforeMaybe:  Option[OffsetDateTime],
      errUndefined: ValidationError,
      errNotAfter:  ValidationError
    ): ValidationResult[Option[OffsetDateTime]] =
    beforeMaybe match {
      case None =>
        if (afterMaybe.isDefined) errUndefined.invalidNec
        else afterMaybe.validNec
      case Some(before) =>
        if (afterMaybe.isEmpty || afterMaybe.exists(after => after.isAfter(before))) {
          afterMaybe.validNec
        } else {
          errNotAfter.invalidNec
        }
    }

}

final case class ShipmentId(id: String) extends IdentifiedValueObject[String]

final case class ShipmentSourceDest(
    originCentre:        Centre,
    originLocation:      Location,
    destinationCentre:   Centre,
    destinationLocation: Location)

object ShipmentId {

  // Do not want JSON to create a sub object, we just want it to be converted
  // to a single string
  implicit val shipmentIdFormat: Format[ShipmentId] = new Format[ShipmentId] {

    override def writes(id: ShipmentId): JsValue = JsString(id.id)

    override def reads(json: JsValue): JsResult[ShipmentId] =
      Reads.StringReads.reads(json).map(ShipmentId.apply _)
  }

}

/**
 * Represents a transfer of [org.biobank.domain.participants.Specimen]s and / or
 * [org.biobank.domain.containers.Container]s from one [org.biobank.domain.centres.Centre] to another.
 *
 * @see org.biobank.domain.centres.ShipmentSpecimen
 * @see org.biobank.domain.centres.ShipmentContainer
 */
sealed trait Shipment extends ConcurrencySafeEntity[ShipmentId] with HasState with ShipmentValidations {

  protected val log: Logger = LoggerFactory.getLogger(this.getClass)

  val courierName:           String
  val trackingNumber:        String
  val originCentreId:        CentreId
  val originLocationId:      LocationId
  val destinationCentreId:   CentreId
  val destinationLocationId: LocationId
  val timePacked:            Option[OffsetDateTime]
  val timeSent:              Option[OffsetDateTime]
  val timeReceived:          Option[OffsetDateTime]
  val timeUnpacked:          Option[OffsetDateTime]
  val timeCompleted:         Option[OffsetDateTime]

  def isCreated(): ValidationResult[CreatedShipment] =
    InvalidState(s"shipment not created: ${this.id}").invalidNec

  def isPacked(): ValidationResult[PackedShipment] =
    InvalidState(s"shipment not packed: ${this.id}").invalidNec

  def isSent(): ValidationResult[SentShipment] =
    InvalidState(s"shipment not sent: ${this.id}").invalidNec

  def isReceived(): ValidationResult[ReceivedShipment] =
    InvalidState(s"shipment not received: ${this.id}").invalidNec

  def isUnpacked(): ValidationResult[UnpackedShipment] =
    InvalidState(s"shipment not unpacked: ${this.id}").invalidNec

  def isCreatedOrUnpacked(): ValidationResult[Shipment] =
    InvalidState(s"shipment not created or unpacked: ${this.id}").invalidNec

  override def toString: String =
    s"""|Shipment:{
        |  id:             $id,
        |  version:        $version,
        |  timeAdded:      $timeAdded,
        |  timeModified:   $timeModified,
        |  state:          $state,
        |  courierName:    $courierName,
        |  trackingNumber: $trackingNumber,
        |  originCentreId:   $originCentreId,
        |  originLocationId: $originLocationId,
        |  destinationCentreId:     $destinationCentreId,
        |  destinationLocationId:   $destinationLocationId,
        |  timePacked:     $timePacked,
        |  timeSent:       $timeSent,
        |  timeReceived:   $timeReceived,
        |  timeUnpacked:   $timeUnpacked,
        |  timeCompleted:  $timeCompleted
        |}""".stripMargin
}

object Shipment extends ShipmentValidations {

  val createdState:   EntityState = new EntityState("created")
  val packedState:    EntityState = new EntityState("packed")
  val sentState:      EntityState = new EntityState("sent")
  val receivedState:  EntityState = new EntityState("received")
  val unpackedState:  EntityState = new EntityState("unpacked")
  val completedState: EntityState = new EntityState("completed")
  val lostState:      EntityState = new EntityState("lost")

  val shipmentStates: List[EntityState] =
    List(createdState, packedState, sentState, receivedState, unpackedState, completedState, lostState)

  @SuppressWarnings(Array("org.wartremover.warts.Option2Iterable"))
  implicit val shipmentFormat: Format[Shipment] = new Format[Shipment] {
    override def writes(shipment: Shipment): JsValue =
      ConcurrencySafeEntity.toJson(shipment) ++
        Json.obj("state"                 -> shipment.state.id,
                 "courierName"           -> shipment.courierName,
                 "trackingNumber"        -> shipment.trackingNumber,
                 "originCentreId"        -> shipment.originCentreId,
                 "originLocationId"      -> shipment.originLocationId.id,
                 "destinationCentreId"   -> shipment.destinationCentreId,
                 "destinationLocationId" -> shipment.destinationLocationId.id) ++
        JsObject(
          Seq[(String, JsValue)]() ++
            shipment.timePacked.map("timePacked"       -> Json.toJson(_)) ++
            shipment.timeSent.map("timeSent"           -> Json.toJson(_)) ++
            shipment.timeReceived.map("timeReceived"   -> Json.toJson(_)) ++
            shipment.timeUnpacked.map("timeUnpacked"   -> Json.toJson(_)) ++
            shipment.timeCompleted.map("timeCompleted" -> Json.toJson(_))
        )

    override def reads(json: JsValue): JsResult[Shipment] = (json \ "state") match {
      case JsDefined(JsString(createdState.id))   => json.validate[CreatedShipment]
      case JsDefined(JsString(packedState.id))    => json.validate[PackedShipment]
      case JsDefined(JsString(sentState.id))      => json.validate[SentShipment]
      case JsDefined(JsString(receivedState.id))  => json.validate[ReceivedShipment]
      case JsDefined(JsString(unpackedState.id))  => json.validate[UnpackedShipment]
      case JsDefined(JsString(completedState.id)) => json.validate[CompletedShipment]
      case JsDefined(JsString(lostState.id))      => json.validate[LostShipment]
      case _                                      => JsError("error")
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val createdShipmentReads: Reads[CreatedShipment] = Json.reads[CreatedShipment]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val packedShipmentReads: Reads[PackedShipment] = Json.reads[PackedShipment]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val sentShipmentReads: Reads[SentShipment] = Json.reads[SentShipment]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val receivedShipmentReads: Reads[ReceivedShipment] = Json.reads[ReceivedShipment]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val unpackedShipmentReads: Reads[UnpackedShipment] = Json.reads[UnpackedShipment]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val completedShipmentReads: Reads[CompletedShipment] = Json.reads[CompletedShipment]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val lostShipmentReads: Reads[LostShipment] = Json.reads[LostShipment]

}

final case class CreatedShipment(
    id:                    ShipmentId,
    version:               Long,
    timeAdded:             OffsetDateTime,
    timeModified:          Option[OffsetDateTime],
    courierName:           String,
    trackingNumber:        String,
    originCentreId:        CentreId,
    originLocationId:      LocationId,
    destinationCentreId:   CentreId,
    destinationLocationId: LocationId,
    timePacked:            Option[OffsetDateTime],
    timeSent:              Option[OffsetDateTime],
    timeReceived:          Option[OffsetDateTime],
    timeUnpacked:          Option[OffsetDateTime],
    timeCompleted:         Option[OffsetDateTime])
    extends Shipment with ShipmentValidations {

  import CreatedShipment._

  val state: EntityState = Shipment.createdState

  override def isCreated(): ValidationResult[CreatedShipment] = this.validNec

  override def isCreatedOrUnpacked(): ValidationResult[CreatedShipment] = this.validNec

  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  def withCourier(
      name:         String,
      timeModified: OffsetDateTime = OffsetDateTime.now
    ): ValidationResult[CreatedShipment] =
    validateCourierName(name).map { name =>
      copy(courierName = name, version = version + 1, timeModified = Some(timeModified))
    }

  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  def withTrackingNumber(
      trackingNumber: String,
      timeModified:   OffsetDateTime = OffsetDateTime.now
    ): ValidationResult[CreatedShipment] =
    validateTrackingNumber(trackingNumber).map { _ =>
      copy(trackingNumber = trackingNumber, version = version + 1, timeModified = Some(timeModified))
    }

  /**
   * Must be a centre's location.
   */
  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  def withOrigin(
      centreId:     CentreId,
      locationId:   LocationId,
      timeModified: OffsetDateTime = OffsetDateTime.now
    ): ValidationResult[CreatedShipment] =
    validateOriginCentre(centreId, locationId).map {
      case _ =>
        copy(originCentreId   = centreId,
             originLocationId = locationId,
             version          = version + 1,
             timeModified     = Some(timeModified))
    }

  /**
   * Must be a centre's location.
   */
  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  def withDestination(
      centreId:     CentreId,
      locationId:   LocationId,
      timeModified: OffsetDateTime = OffsetDateTime.now
    ): ValidationResult[CreatedShipment] =
    (validateId(centreId, DestinationCentreIdEmpty),
     validateNonEmptyString(locationId.id, DestinationLocationIdEmpty)).mapN {
      case _ =>
        copy(destinationCentreId   = centreId,
             destinationLocationId = locationId,
             version               = version + 1,
             timeModified          = Some(timeModified))
    }

  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  def pack(timePacked: OffsetDateTime, timeModified: OffsetDateTime = OffsetDateTime.now): PackedShipment =
    PackedShipment(id                    = this.id,
                   version               = this.version + 1,
                   timeAdded             = this.timeAdded,
                   timeModified          = Some(timeModified),
                   courierName           = this.courierName,
                   trackingNumber        = this.trackingNumber,
                   originCentreId        = this.originCentreId,
                   originLocationId      = this.originLocationId,
                   destinationCentreId   = this.destinationCentreId,
                   destinationLocationId = this.destinationLocationId,
                   timePacked            = Some(timePacked),
                   timeSent              = None,
                   timeReceived          = None,
                   timeUnpacked          = None,
                   timeCompleted         = None)

  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  def skipToSent(
      timePacked:   OffsetDateTime,
      timeSent:     OffsetDateTime,
      timeModified: OffsetDateTime = OffsetDateTime.now
    ): ValidationResult[SentShipment] =
    if (timeSent.isBefore(timePacked)) {
      TimeSentBeforePacked.invalidNec
    } else {
      SentShipment(id                    = this.id,
                   version               = this.version + 1,
                   timeAdded             = this.timeAdded,
                   timeModified          = Some(timeModified),
                   courierName           = this.courierName,
                   trackingNumber        = this.trackingNumber,
                   originCentreId        = this.originCentreId,
                   originLocationId      = this.originLocationId,
                   destinationCentreId   = this.destinationCentreId,
                   destinationLocationId = this.destinationLocationId,
                   timePacked            = Some(timePacked),
                   timeSent              = Some(timeSent),
                   timeReceived          = None,
                   timeUnpacked          = None,
                   timeCompleted         = None).validNec
    }

}

object CreatedShipment extends ShipmentValidations {

  def validateCourierName(name: String): ValidationResult[String] =
    validateNonEmptyString(name, CourierNameEmpty)

  def validateTrackingNumber(trackingNumber: String): ValidationResult[String] =
    validateNonEmptyString(trackingNumber, TrackingNumberEmpty)

  def validateOriginCentre(centreId: CentreId, locationId: LocationId): ValidationResult[Unit] =
    (validateId(centreId, OriginCentreIdEmpty), validateNonEmptyString(locationId.id, OriginLocationIdEmpty))
      .mapN((_, _) => ())

  def validateDestinationCentre(centreId: CentreId, locationId: LocationId): ValidationResult[Unit] =
    (validateId(centreId, DestinationCentreIdEmpty),
     validateNonEmptyString(locationId.id, DestinationLocationIdEmpty)).mapN((_, _) => ())

  def create(
      id:                    ShipmentId,
      version:               Long,
      timeAdded:             OffsetDateTime,
      courierName:           String,
      trackingNumber:        String,
      originCentreId:        CentreId,
      originLocationId:      LocationId,
      destinationCentreId:   CentreId,
      destinationLocationId: LocationId
    ): ValidationResult[CreatedShipment] =
    validate(id,
             version,
             courierName,
             trackingNumber,
             originCentreId,
             originLocationId,
             destinationCentreId,
             destinationLocationId)
      .map(
        _ =>
          CreatedShipment(id,
                          version,
                          timeAdded,
                          None,
                          courierName,
                          trackingNumber,
                          originCentreId,
                          originLocationId,
                          destinationCentreId,
                          destinationLocationId,
                          None,
                          None,
                          None,
                          None,
                          None)
      )

  def validate(
      id:                    ShipmentId,
      version:               Long,
      courierName:           String,
      trackingNumber:        String,
      originCentreId:        CentreId,
      originLocationId:      LocationId,
      destinationCentreId:   CentreId,
      destinationLocationId: LocationId
    ): ValidationResult[Unit] =
    (validateId(id),
     validateVersion(version),
     validateCourierName(courierName),
     validateTrackingNumber(trackingNumber),
     validateOriginCentre(originCentreId, originLocationId),
     validateDestinationCentre(destinationCentreId, destinationLocationId)).mapN((_, _, _, _, _, _) => ())

}

@SuppressWarnings(
  Array("org.wartremover.warts.DefaultArguments",
        "org.wartremover.warts.JavaSerializable",
        "org.wartremover.warts.Product",
        "org.wartremover.warts.Serializable")
)
final case class PackedShipment(
    id:                    ShipmentId,
    version:               Long,
    timeAdded:             OffsetDateTime,
    timeModified:          Option[OffsetDateTime],
    courierName:           String,
    trackingNumber:        String,
    originCentreId:        CentreId,
    originLocationId:      LocationId,
    destinationCentreId:   CentreId,
    destinationLocationId: LocationId,
    timePacked:            Option[OffsetDateTime],
    timeSent:              Option[OffsetDateTime],
    timeReceived:          Option[OffsetDateTime],
    timeUnpacked:          Option[OffsetDateTime],
    timeCompleted:         Option[OffsetDateTime])
    extends Shipment with ShipmentValidations {

  val state: EntityState = Shipment.packedState

  override def isPacked(): ValidationResult[PackedShipment] = this.validNec

  /**
   * Returns shipment to created state.
   *
   */
  def created(timeModified: OffsetDateTime = OffsetDateTime.now): CreatedShipment =
    CreatedShipment(id                    = this.id,
                    version               = this.version + 1,
                    timeAdded             = this.timeAdded,
                    timeModified          = Some(timeModified),
                    courierName           = this.courierName,
                    trackingNumber        = this.trackingNumber,
                    originCentreId        = this.originCentreId,
                    originLocationId      = this.originLocationId,
                    destinationCentreId   = this.destinationCentreId,
                    destinationLocationId = this.destinationLocationId,
                    timePacked            = None,
                    timeSent              = None,
                    timeReceived          = None,
                    timeUnpacked          = None,
                    timeCompleted         = None)

  def send(
      timeSent:     OffsetDateTime,
      timeModified: OffsetDateTime = OffsetDateTime.now
    ): ValidationResult[SentShipment] = {
    val valid = timePacked match {
      case Some(tp) => if (timeSent.isBefore(tp)) TimeSentBeforePacked.invalidNec else timeSent.validNec
      case None     => TimePackedUndefined.invalidNec
    }

    valid.map { _ =>
      SentShipment(id                    = this.id,
                   version               = this.version + 1,
                   timeAdded             = this.timeAdded,
                   timeModified          = Some(timeModified),
                   courierName           = this.courierName,
                   trackingNumber        = this.trackingNumber,
                   originCentreId        = this.originCentreId,
                   originLocationId      = this.originLocationId,
                   destinationCentreId   = this.destinationCentreId,
                   destinationLocationId = this.destinationLocationId,
                   timePacked            = this.timePacked,
                   timeSent              = Some(timeSent),
                   timeReceived          = None,
                   timeUnpacked          = None,
                   timeCompleted         = None)
    }
  }

}

@SuppressWarnings(
  Array("org.wartremover.warts.DefaultArguments",
        "org.wartremover.warts.JavaSerializable",
        "org.wartremover.warts.Product",
        "org.wartremover.warts.Serializable")
)
final case class SentShipment(
    id:                    ShipmentId,
    version:               Long,
    timeAdded:             OffsetDateTime,
    timeModified:          Option[OffsetDateTime],
    courierName:           String,
    trackingNumber:        String,
    originCentreId:        CentreId,
    originLocationId:      LocationId,
    destinationCentreId:   CentreId,
    destinationLocationId: LocationId,
    timePacked:            Option[OffsetDateTime],
    timeSent:              Option[OffsetDateTime],
    timeReceived:          Option[OffsetDateTime],
    timeUnpacked:          Option[OffsetDateTime],
    timeCompleted:         Option[OffsetDateTime])
    extends Shipment with ShipmentValidations {

  val state: EntityState = Shipment.sentState

  override def isSent(): ValidationResult[SentShipment] = this.validNec

  def backToPacked(timeModified: OffsetDateTime = OffsetDateTime.now): PackedShipment =
    PackedShipment(id                    = this.id,
                   version               = this.version + 1,
                   timeAdded             = this.timeAdded,
                   timeModified          = Some(timeModified),
                   courierName           = this.courierName,
                   trackingNumber        = this.trackingNumber,
                   originCentreId        = this.originCentreId,
                   originLocationId      = this.originLocationId,
                   destinationCentreId   = this.destinationCentreId,
                   destinationLocationId = this.destinationLocationId,
                   timePacked            = this.timePacked,
                   timeSent              = None,
                   timeReceived          = None,
                   timeUnpacked          = None,
                   timeCompleted         = None)

  def receive(
      timeReceived: OffsetDateTime,
      timeModified: OffsetDateTime = OffsetDateTime.now
    ): ValidationResult[ReceivedShipment] = {
    val valid = timeSent match {
      case Some(ts) =>
        if (timeReceived.isBefore(ts)) TimeReceivedBeforeSent.invalidNec else timeReceived.validNec
      case None => TimeSentUndefined.invalidNec
    }

    valid map { _ =>
      ReceivedShipment(id                    = this.id,
                       version               = this.version + 1,
                       timeAdded             = this.timeAdded,
                       timeModified          = Some(timeModified),
                       courierName           = this.courierName,
                       trackingNumber        = this.trackingNumber,
                       originCentreId        = this.originCentreId,
                       originLocationId      = this.originLocationId,
                       destinationCentreId   = this.destinationCentreId,
                       destinationLocationId = this.destinationLocationId,
                       timePacked            = this.timePacked,
                       timeSent              = this.timeSent,
                       timeReceived          = Some(timeReceived),
                       timeUnpacked          = None,
                       timeCompleted         = None)
    }
  }

  def skipToUnpacked(
      timeReceived: OffsetDateTime,
      timeUnpacked: OffsetDateTime,
      timeModified: OffsetDateTime = OffsetDateTime.now
    ): ValidationResult[UnpackedShipment] = {
    val valid = timeSent match {
      case Some(ts) =>
        val validUnpacked = !timeUnpacked.isBefore(timeReceived)
        val validReceived = !timeReceived.isBefore(ts)
        if (validUnpacked && validReceived) {
          timeReceived.validNec
        } else if (!validUnpacked) {
          TimeUnpackedBeforeReceived.invalidNec
        } else {
          TimeReceivedBeforeSent.invalidNec
        }
      case None => TimeSentUndefined.invalidNec
    }

    valid map { _ =>
      UnpackedShipment(id                    = this.id,
                       version               = this.version + 1,
                       timeAdded             = this.timeAdded,
                       timeModified          = Some(timeModified),
                       courierName           = this.courierName,
                       trackingNumber        = this.trackingNumber,
                       originCentreId        = this.originCentreId,
                       originLocationId      = this.originLocationId,
                       destinationCentreId   = this.destinationCentreId,
                       destinationLocationId = this.destinationLocationId,
                       timePacked            = this.timePacked,
                       timeSent              = this.timeSent,
                       timeReceived          = Some(timeReceived),
                       timeUnpacked          = Some(timeUnpacked),
                       timeCompleted         = None)
    }

  }

  def lost: LostShipment =
    LostShipment(id                    = this.id,
                 version               = this.version + 1,
                 timeAdded             = this.timeAdded,
                 timeModified          = Some(OffsetDateTime.now),
                 courierName           = this.courierName,
                 trackingNumber        = this.trackingNumber,
                 originCentreId        = this.originCentreId,
                 originLocationId      = this.originLocationId,
                 destinationCentreId   = this.destinationCentreId,
                 destinationLocationId = this.destinationLocationId,
                 timePacked            = this.timePacked,
                 timeSent              = this.timeSent,
                 timeReceived          = None,
                 timeUnpacked          = None,
                 timeCompleted         = None)
}

@SuppressWarnings(
  Array("org.wartremover.warts.DefaultArguments",
        "org.wartremover.warts.JavaSerializable",
        "org.wartremover.warts.Product",
        "org.wartremover.warts.Serializable")
)
final case class ReceivedShipment(
    id:                    ShipmentId,
    version:               Long,
    timeAdded:             OffsetDateTime,
    timeModified:          Option[OffsetDateTime],
    courierName:           String,
    trackingNumber:        String,
    originCentreId:        CentreId,
    originLocationId:      LocationId,
    destinationCentreId:   CentreId,
    destinationLocationId: LocationId,
    timePacked:            Option[OffsetDateTime],
    timeSent:              Option[OffsetDateTime],
    timeReceived:          Option[OffsetDateTime],
    timeUnpacked:          Option[OffsetDateTime],
    timeCompleted:         Option[OffsetDateTime])
    extends Shipment with ShipmentValidations {

  val state: EntityState = Shipment.receivedState

  override def isReceived(): ValidationResult[ReceivedShipment] = this.validNec

  def backToSent(timeModified: OffsetDateTime = OffsetDateTime.now): SentShipment =
    SentShipment(id                    = this.id,
                 version               = this.version + 1,
                 timeAdded             = this.timeAdded,
                 timeModified          = Some(timeModified),
                 courierName           = this.courierName,
                 trackingNumber        = this.trackingNumber,
                 originCentreId        = this.originCentreId,
                 originLocationId      = this.originLocationId,
                 destinationCentreId   = this.destinationCentreId,
                 destinationLocationId = this.destinationLocationId,
                 timePacked            = this.timePacked,
                 timeSent              = this.timeSent,
                 timeReceived          = None,
                 timeUnpacked          = None,
                 timeCompleted         = None)

  def unpack(
      timeUnpacked: OffsetDateTime,
      timeModified: OffsetDateTime = OffsetDateTime.now
    ): ValidationResult[UnpackedShipment] = {
    val valid = timeReceived match {
      case Some(tr) =>
        if (timeUnpacked.isBefore(tr)) TimeUnpackedBeforeReceived.invalidNec else timeUnpacked.validNec
      case None => TimeReceivedUndefined.invalidNec
    }

    valid.map { _ =>
      UnpackedShipment(id                    = this.id,
                       version               = this.version + 1,
                       timeAdded             = this.timeAdded,
                       timeModified          = Some(timeModified),
                       courierName           = this.courierName,
                       trackingNumber        = this.trackingNumber,
                       originCentreId        = this.originCentreId,
                       originLocationId      = this.originLocationId,
                       destinationCentreId   = this.destinationCentreId,
                       destinationLocationId = this.destinationLocationId,
                       timePacked            = this.timePacked,
                       timeSent              = this.timeSent,
                       timeReceived          = this.timeReceived,
                       timeUnpacked          = Some(timeUnpacked),
                       timeCompleted         = None)
    }
  }
}

@SuppressWarnings(
  Array("org.wartremover.warts.DefaultArguments",
        "org.wartremover.warts.JavaSerializable",
        "org.wartremover.warts.Product",
        "org.wartremover.warts.Serializable")
)
final case class UnpackedShipment(
    id:                    ShipmentId,
    version:               Long,
    timeAdded:             OffsetDateTime,
    timeModified:          Option[OffsetDateTime],
    courierName:           String,
    trackingNumber:        String,
    originCentreId:        CentreId,
    originLocationId:      LocationId,
    destinationCentreId:   CentreId,
    destinationLocationId: LocationId,
    timePacked:            Option[OffsetDateTime],
    timeSent:              Option[OffsetDateTime],
    timeReceived:          Option[OffsetDateTime],
    timeUnpacked:          Option[OffsetDateTime],
    timeCompleted:         Option[OffsetDateTime])
    extends Shipment with ShipmentValidations {

  val state: EntityState = Shipment.unpackedState

  override def isUnpacked(): ValidationResult[UnpackedShipment] = this.validNec

  override def isCreatedOrUnpacked(): ValidationResult[UnpackedShipment] = this.validNec

  def backToReceived(timeModified: OffsetDateTime = OffsetDateTime.now): ReceivedShipment =
    ReceivedShipment(id                    = this.id,
                     version               = this.version + 1,
                     timeAdded             = this.timeAdded,
                     timeModified          = Some(timeModified),
                     courierName           = this.courierName,
                     trackingNumber        = this.trackingNumber,
                     originCentreId        = this.originCentreId,
                     originLocationId      = this.originLocationId,
                     destinationCentreId   = this.destinationCentreId,
                     destinationLocationId = this.destinationLocationId,
                     timePacked            = this.timePacked,
                     timeSent              = this.timeSent,
                     timeReceived          = this.timeReceived,
                     timeUnpacked          = None,
                     timeCompleted         = None)

  def complete(
      timeCompleted: OffsetDateTime,
      timeModified:  OffsetDateTime = OffsetDateTime.now
    ): ValidationResult[CompletedShipment] = {
    val valid = timeUnpacked match {
      case Some(tu) =>
        if (timeCompleted.isBefore(tu)) TimeCompletedBeforeUnpacked.invalidNec else timeCompleted.validNec
      case None => TimeUnpackedUndefined.invalidNec
    }

    valid.map { _ =>
      CompletedShipment(id                    = this.id,
                        version               = this.version + 1,
                        timeAdded             = this.timeAdded,
                        timeModified          = Some(timeModified),
                        courierName           = this.courierName,
                        trackingNumber        = this.trackingNumber,
                        originCentreId        = this.originCentreId,
                        originLocationId      = this.originLocationId,
                        destinationCentreId   = this.destinationCentreId,
                        destinationLocationId = this.destinationLocationId,
                        timePacked            = this.timePacked,
                        timeSent              = this.timeSent,
                        timeReceived          = this.timeReceived,
                        timeUnpacked          = this.timeUnpacked,
                        timeCompleted         = Some(timeCompleted))
    }
  }
}

@SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
final case class CompletedShipment(
    id:                    ShipmentId,
    version:               Long,
    timeAdded:             OffsetDateTime,
    timeModified:          Option[OffsetDateTime],
    courierName:           String,
    trackingNumber:        String,
    originCentreId:        CentreId,
    originLocationId:      LocationId,
    destinationCentreId:   CentreId,
    destinationLocationId: LocationId,
    timePacked:            Option[OffsetDateTime],
    timeSent:              Option[OffsetDateTime],
    timeReceived:          Option[OffsetDateTime],
    timeUnpacked:          Option[OffsetDateTime],
    timeCompleted:         Option[OffsetDateTime])
    extends Shipment with ShipmentValidations {

  val state: EntityState = Shipment.completedState

  def backToUnpacked(timeModified: OffsetDateTime = OffsetDateTime.now): UnpackedShipment =
    UnpackedShipment(id                    = this.id,
                     version               = this.version + 1,
                     timeAdded             = this.timeAdded,
                     timeModified          = Some(timeModified),
                     courierName           = this.courierName,
                     trackingNumber        = this.trackingNumber,
                     originCentreId        = this.originCentreId,
                     originLocationId      = this.originLocationId,
                     destinationCentreId   = this.destinationCentreId,
                     destinationLocationId = this.destinationLocationId,
                     timePacked            = this.timePacked,
                     timeSent              = this.timeSent,
                     timeReceived          = this.timeReceived,
                     timeUnpacked          = this.timeUnpacked,
                     timeCompleted         = None)

}

final case class LostShipment(
    id:                    ShipmentId,
    version:               Long,
    timeAdded:             OffsetDateTime,
    timeModified:          Option[OffsetDateTime],
    courierName:           String,
    trackingNumber:        String,
    originCentreId:        CentreId,
    originLocationId:      LocationId,
    destinationCentreId:   CentreId,
    destinationLocationId: LocationId,
    timePacked:            Option[OffsetDateTime],
    timeSent:              Option[OffsetDateTime],
    timeReceived:          Option[OffsetDateTime],
    timeUnpacked:          Option[OffsetDateTime],
    timeCompleted:         Option[OffsetDateTime])
    extends Shipment with ShipmentValidations {

  val state: EntityState = Shipment.lostState

  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  def backToSent(timeModified: OffsetDateTime = OffsetDateTime.now): SentShipment =
    SentShipment(id                    = this.id,
                 version               = this.version + 1,
                 timeAdded             = this.timeAdded,
                 timeModified          = Some(timeModified),
                 courierName           = this.courierName,
                 trackingNumber        = this.trackingNumber,
                 originCentreId        = this.originCentreId,
                 originLocationId      = this.originLocationId,
                 destinationCentreId   = this.destinationCentreId,
                 destinationLocationId = this.destinationLocationId,
                 timePacked            = this.timePacked,
                 timeSent              = this.timeSent,
                 timeReceived          = None,
                 timeUnpacked          = None,
                 timeCompleted         = None)

}
