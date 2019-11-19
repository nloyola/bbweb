package org.biobank.domain.centres

import java.time.OffsetDateTime
import org.biobank.domain._
import org.biobank.ValidationKey
import org.slf4j.{Logger, LoggerFactory}
import play.api.libs.json._
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._

final case class ShipmentId(id: String) extends IdentifiedValueObject[String]

object ShipmentId {

  // Do not want JSON to create a sub object, we just want it to be converted
  // to a single string
  implicit val shipmentIdReader: Format[ShipmentId] = new Format[ShipmentId] {

    override def writes(id: ShipmentId): JsValue = JsString(id.id)

    override def reads(json: JsValue): JsResult[ShipmentId] =
      Reads.StringReads.reads(json).map(ShipmentId.apply _)
  }

}

trait ShipmentPredicates {
  type ShipmentFilter = Shipment => Boolean

  val originCentreIdIsOneOf: Set[CentreId] => ShipmentFilter =
    centreIds => shipment => centreIds.contains(shipment.originCentreId)

  val destinationCentreIdIsOneOf: Set[CentreId] => ShipmentFilter =
    centreIds => shipment => centreIds.contains(shipment.destinationCentreId)

  val withCentreIdIsOneOf: Set[CentreId] => ShipmentFilter =
    centreIds =>
      shipment =>
        centreIds.contains(shipment.destinationCentreId) || centreIds.contains(shipment.originCentreId)

  val courierNameIsOneOf: Set[String] => ShipmentFilter =
    courierNames => shipment => courierNames.contains(shipment.courierName)

  val trackingNumberIsOneOf: Set[String] => ShipmentFilter =
    trackingNumbers => shipment => trackingNumbers.contains(shipment.trackingNumber)

  val stateIsOneOf: Set[EntityState] => ShipmentFilter =
    states => shipment => states.contains(shipment.state)

  val courierNameIsLike: Set[String] => ShipmentFilter =
    courierNames =>
      shipment => {
        val lc = shipment.courierName.toLowerCase
        courierNames.forall(n => lc.contains(n.toLowerCase))
      }

  val trackingNumberIsLike: Set[String] => ShipmentFilter =
    trackingNumbers =>
      shipment => {
        val lc = shipment.trackingNumber.toLowerCase
        trackingNumbers.forall(n => lc.contains(n.toLowerCase))
      }

}

/**
 * Represents a transfer of [org.biobank.domain.participants.Specimen]s and / or
 * [org.biobank.domain.containers.Container]s from one [org.biobank.domain.centres.Centre] to another.
 *
 * @see org.biobank.domain.centres.ShipmentSpecimen
 * @see org.biobank.domain.centres.ShipmentContainer
 */
sealed trait Shipment extends ConcurrencySafeEntity[ShipmentId] with HasState {
  import org.biobank.CommonValidations._

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

  def isCreated: DomainValidation[CreatedShipment] =
    InvalidState(s"shipment not created: ${this.id}").failureNel[CreatedShipment]

  def isPacked: DomainValidation[PackedShipment] =
    InvalidState(s"shipment not packed: ${this.id}").failureNel[PackedShipment]

  def isSent: DomainValidation[SentShipment] =
    InvalidState(s"shipment not sent: ${this.id}").failureNel[SentShipment]

  def isReceived: DomainValidation[ReceivedShipment] =
    InvalidState(s"shipment not received: ${this.id}").failureNel[ReceivedShipment]

  def isUnpacked: DomainValidation[UnpackedShipment] =
    InvalidState(s"shipment not unpacked: ${this.id}").failureNel[UnpackedShipment]

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

trait ShipmentValidations {

  case object CourierNameInvalid extends ValidationKey

  case object TrackingNumberInvalid extends ValidationKey

  case object OriginCentreIdInvalid extends ValidationKey

  case object OriginLocationIdInvalid extends ValidationKey

  case object DestinationCentreIdInvalid extends ValidationKey

  case object DestinationLocationIdInvalid extends ValidationKey

  case object TimePackedUndefined extends ValidationKey

  case object TimeSentBeforePacked extends ValidationKey

  case object TimeSentUndefined extends ValidationKey

  case object TimeReceivedBeforeSent extends ValidationKey

  case object TimeReceivedUndefined extends ValidationKey

  case object TimeUnpackedBeforeReceived extends ValidationKey

  case object TimeCompletedBeforeUnpacked extends ValidationKey

  def validateTimeAfter(
      afterMaybe:   Option[OffsetDateTime],
      beforeMaybe:  Option[OffsetDateTime],
      errUndefined: ValidationKey,
      errNotAfter:  ValidationKey
    ): DomainValidation[Option[OffsetDateTime]] =
    beforeMaybe.fold {
      if (afterMaybe.isDefined) errUndefined.failureNel[Option[OffsetDateTime]]
      else afterMaybe.successNel[String]
    } { before =>
      if (afterMaybe.isEmpty || afterMaybe.exists(after => after.isAfter(before))) {
        afterMaybe.successNel[String]
      } else {
        errNotAfter.failureNel[Option[OffsetDateTime]]
      }
    }

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

  implicit val createdShipmentReads:   Reads[CreatedShipment]   = Json.reads[CreatedShipment]
  implicit val packedShipmentReads:    Reads[PackedShipment]    = Json.reads[PackedShipment]
  implicit val sentShipmentReads:      Reads[SentShipment]      = Json.reads[SentShipment]
  implicit val receivedShipmentReads:  Reads[ReceivedShipment]  = Json.reads[ReceivedShipment]
  implicit val unpackedShipmentReads:  Reads[UnpackedShipment]  = Json.reads[UnpackedShipment]
  implicit val completedShipmentReads: Reads[CompletedShipment] = Json.reads[CompletedShipment]
  implicit val lostShipmentReads:      Reads[LostShipment]      = Json.reads[LostShipment]

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
    extends { val state: EntityState = Shipment.createdState } with Shipment with ShipmentValidations {

  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  override def isCreated: DomainValidation[CreatedShipment] = this.successNel[String]

  def withCourier(name: String): DomainValidation[CreatedShipment] =
    validateNonEmptyString(name, CourierNameInvalid).map { name =>
      copy(courierName = name, version = version + 1, timeModified = Some(OffsetDateTime.now))
    }

  def withTrackingNumber(trackingNumber: String): DomainValidation[CreatedShipment] =
    validateNonEmptyString(trackingNumber, TrackingNumberInvalid).map { _ =>
      copy(trackingNumber = trackingNumber, version = version + 1, timeModified = Some(OffsetDateTime.now))
    }

  /**
   * Must be a centre's location.
   */
  def withOrigin(centreId: CentreId, locationId: LocationId): DomainValidation[CreatedShipment] =
    (validateId(centreId, OriginCentreIdInvalid) |@|
      validateNonEmptyString(locationId.id, LocationIdInvalid)) {
      case (_, _) =>
        copy(originCentreId   = centreId,
             originLocationId = locationId,
             version          = version + 1,
             timeModified     = Some(OffsetDateTime.now))
    }

  /**
   * Must be a centre's location.
   */
  def withDestination(centreId: CentreId, locationId: LocationId): DomainValidation[CreatedShipment] =
    (validateId(centreId, DestinationCentreIdInvalid) |@|
      validateNonEmptyString(locationId.id, LocationIdInvalid)) {
      case (_, _) =>
        copy(destinationCentreId   = centreId,
             destinationLocationId = locationId,
             version               = version + 1,
             timeModified          = Some(OffsetDateTime.now))
    }

  def pack(timePacked: OffsetDateTime): PackedShipment =
    PackedShipment(id                    = this.id,
                   version               = this.version + 1,
                   timeAdded             = this.timeAdded,
                   timeModified          = Some(OffsetDateTime.now),
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

  def skipToSent(timePacked: OffsetDateTime, timeSent: OffsetDateTime): DomainValidation[SentShipment] =
    if (timeSent.isBefore(timePacked)) {
      TimeSentBeforePacked.failureNel[SentShipment]
    } else {
      SentShipment(id                    = this.id,
                   version               = this.version + 1,
                   timeAdded             = this.timeAdded,
                   timeModified          = Some(OffsetDateTime.now),
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
                   timeCompleted         = None).successNel[String]
    }

}

object CreatedShipment extends ShipmentValidations {
  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

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
    ): DomainValidation[CreatedShipment] =
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
    ): DomainValidation[Unit] =
    (validateId(id) |@|
      validateVersion(version) |@|
      validateNonEmptyString(courierName, CourierNameInvalid) |@|
      validateNonEmptyString(trackingNumber, TrackingNumberInvalid) |@|
      validateId(originCentreId, OriginCentreIdInvalid) |@|
      validateNonEmptyString(originLocationId.id, OriginLocationIdInvalid) |@|
      validateId(destinationCentreId, DestinationCentreIdInvalid) |@|
      validateNonEmptyString(destinationLocationId.id, DestinationLocationIdInvalid)) {
      case _ => ()
    }

}

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
    extends { val state: EntityState = Shipment.packedState } with Shipment with ShipmentValidations {

  override def isPacked: DomainValidation[PackedShipment] = this.successNel[String]

  /**
   * Returns shipment to created state.
   *
   */
  def created: CreatedShipment =
    CreatedShipment(id                    = this.id,
                    version               = this.version + 1,
                    timeAdded             = this.timeAdded,
                    timeModified          = Some(OffsetDateTime.now),
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

  def send(timeSent: OffsetDateTime): DomainValidation[SentShipment] = {
    for {
      timePacked <- this.timePacked.toSuccessNel(TimePackedUndefined.toString)
      validTime <- if (timeSent.isBefore(timePacked)) {
                    TimeSentBeforePacked.failureNel[Unit]
                  } else {
                    ().successNel[String]
                  }
    } yield SentShipment(id                    = this.id,
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
                         timeSent              = Some(timeSent),
                         timeReceived          = None,
                         timeUnpacked          = None,
                         timeCompleted         = None)
  }

}

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
    extends { val state: EntityState = Shipment.sentState } with Shipment with ShipmentValidations {

  override def isSent: DomainValidation[SentShipment] = this.successNel[String]

  def backToPacked: PackedShipment =
    PackedShipment(id                    = this.id,
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
                   timeSent              = None,
                   timeReceived          = None,
                   timeUnpacked          = None,
                   timeCompleted         = None)

  def receive(timeReceived: OffsetDateTime): DomainValidation[ReceivedShipment] = {
    for {
      timeSent <- this.timeSent.toSuccessNel(TimeSentUndefined.toString)
      validTime <- if (timeReceived.isBefore(timeSent)) {
                    TimeReceivedBeforeSent.failureNel[Unit]
                  } else {
                    ().successNel[String]
                  }
    } yield ReceivedShipment(id                    = this.id,
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
                             timeReceived          = Some(timeReceived),
                             timeUnpacked          = None,
                             timeCompleted         = None)
  }

  def skipToUnpacked(
      timeReceived: OffsetDateTime,
      timeUnpacked: OffsetDateTime
    ): DomainValidation[UnpackedShipment] = {
    for {
      timeSent <- this.timeSent.toSuccessNel(TimeSentUndefined.toString)
      validTimes <- if (timeReceived.isBefore(timeSent)) {
                     TimeReceivedBeforeSent.failureNel[Unit]
                   } else if (timeUnpacked.isBefore(timeReceived)) {
                     TimeUnpackedBeforeReceived.failureNel[Unit]
                   } else {
                     ().successNel[String]
                   }
    } yield UnpackedShipment(id                    = this.id,
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
                             timeReceived          = Some(timeReceived),
                             timeUnpacked          = Some(timeUnpacked),
                             timeCompleted         = None)

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
    extends { val state: EntityState = Shipment.receivedState } with Shipment with ShipmentValidations {

  override def isReceived: DomainValidation[ReceivedShipment] = this.successNel[String]

  def backToSent: SentShipment =
    SentShipment(id                    = this.id,
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

  def unpack(timeUnpacked: OffsetDateTime): DomainValidation[UnpackedShipment] = {
    for {
      timeReceived <- this.timeReceived.toSuccessNel(TimeReceivedUndefined.toString)
      validTime <- if (timeUnpacked.isBefore(timeReceived)) {
                    TimeUnpackedBeforeReceived.failureNel[Unit]
                  } else {
                    ().successNel[String]
                  }
    } yield UnpackedShipment(id                    = this.id,
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
                             timeReceived          = this.timeReceived,
                             timeUnpacked          = Some(timeUnpacked),
                             timeCompleted         = None)
  }
}

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
    extends { val state: EntityState = Shipment.unpackedState } with Shipment with ShipmentValidations {

  override def isUnpacked: DomainValidation[UnpackedShipment] = this.successNel[String]

  def backToReceived: ReceivedShipment =
    ReceivedShipment(id                    = this.id,
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
                     timeReceived          = this.timeReceived,
                     timeUnpacked          = None,
                     timeCompleted         = None)

  def complete(timeCompleted: OffsetDateTime): DomainValidation[CompletedShipment] = {
    for {
      timeUnpacked <- this.timeUnpacked.toSuccessNel(TimeReceivedUndefined.toString)
      validTime <- if (timeCompleted.isBefore(timeUnpacked)) {
                    TimeCompletedBeforeUnpacked.failureNel[Unit]
                  } else {
                    ().successNel[String]
                  }
    } yield CompletedShipment(id                    = this.id,
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
                              timeReceived          = this.timeReceived,
                              timeUnpacked          = this.timeUnpacked,
                              timeCompleted         = Some(timeCompleted))
  }
}

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
    extends { val state: EntityState = Shipment.completedState } with Shipment with ShipmentValidations {

  def backToUnpacked: UnpackedShipment =
    UnpackedShipment(id                    = this.id,
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
    extends { val state: EntityState = Shipment.lostState } with Shipment with ShipmentValidations {

  def backToSent: SentShipment =
    SentShipment(id                    = this.id,
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
