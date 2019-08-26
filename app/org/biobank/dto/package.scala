package org.biobank

import java.time.format.DateTimeFormatter
import org.biobank.domain._
import org.biobank.domain.annotations.Annotation
import org.biobank.domain.centres.Shipment
import org.biobank.domain.containers._
import org.biobank.dto.access.{UserMembershipDto, UserRoleDto}
import org.biobank.services.centres.CentreLocationInfo
import play.api.libs.json._

package dto {

  trait Dto

  final case class EntityInfoDto(id: String, slug: Slug, name: String) {

    override def toString: String =
      s"""|${this.getClass.getSimpleName}: {
          |  id:   $id,
          |  slug: $slug,
          |  name: $name
          |}""".stripMargin
  }

  object EntityInfoDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply[T <: ConcurrencySafeEntity[_] with HasSlug with HasName](entity: T): EntityInfoDto =
      EntityInfoDto(entity.id.toString, entity.slug, entity.name)

    def compareByName(a: EntityInfoDto, b: EntityInfoDto): Boolean = (a.name compareToIgnoreCase b.name) < 0

    implicit val entityInfoDtoFormat: Format[EntityInfoDto] = Json.format[EntityInfoDto]

  }

  final case class EntitySetDto(allEntities: Boolean, entityData: Set[EntityInfoDto])

  object EntitySetDto {

    implicit val entitySetInfoDtoFormat: Format[EntitySetDto] = Json.format[EntitySetDto]

  }

  final case class EntityInfoAndStateDto(id: String, slug: Slug, name: String, state: String)

  object EntityInfoAndStateDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply[T <: ConcurrencySafeEntity[_] with HasSlug with HasName with HasState](
        entity: T
      ): EntityInfoAndStateDto =
      EntityInfoAndStateDto(entity.id.toString, entity.slug, entity.name, entity.state.id)

    def compareByName(a: EntityInfoAndStateDto, b: EntityInfoAndStateDto): Boolean =
      (a.name compareToIgnoreCase b.name) < 0

    implicit val entityInfoAndStateDtoFormat: Format[EntityInfoAndStateDto] =
      Json.format[EntityInfoAndStateDto]
  }

  final case class CentreDto(
      id:           String,
      version:      Long,
      timeAdded:    String,
      timeModified: Option[String],
      state:        String,
      slug:         Slug,
      name:         String,
      description:  Option[String],
      studyNames:   Set[EntityInfoAndStateDto],
      locations:    Set[Location])

  object CentreDto {

    implicit val centreDtoFormat: Format[CentreDto] = Json.format[CentreDto]

  }

  final case class ContainerTypeDto(
      id:           String,
      version:      Long,
      timeAdded:    String,
      timeModified: Option[String],
      slug:         Slug,
      name:         String,
      description:  Option[String],
      centre:       Option[EntityInfoAndStateDto],
      schema:       EntityInfoAndStateDto,
      shared:       Boolean,
      enabled:      Boolean)

  object ContainerTypeDto {

    implicit val containerTypeDtoFormat: Format[ContainerTypeDto] = Json.format[ContainerTypeDto]

  }

  trait ContainerDto

  object ContainerDto {

    implicit val containerDtoWrites: Writes[ContainerDto] = new Writes[ContainerDto] {

      def writes(container: ContainerDto): JsValue =
        container match {
          case c: StorageContainerDto => Json.toJson(c)(StorageContainerDto.storageContainerDtoFormat)
          //case c: SpecimenContainer => Json.toJson(c)
        }
    }

  }

  final case class StorageContainerDto(
      id:               String,
      version:          Long,
      timeAdded:        String,
      timeModified:     Option[String],
      slug:             Slug,
      inventoryId:      String,
      label:            String,
      enabled:          Boolean,
      sharedProperties: Option[ContainerSharedProperties],
      containerType:    EntityInfoDto,
      parent:           Option[EntityInfoDto],
      position:         Option[ContainerSchemaPosition],
      constraints:      Option[EntityInfoDto])
      extends ContainerDto

  object StorageContainerDto {

    implicit val storageContainerDtoFormat: Format[StorageContainerDto] = Json.format[StorageContainerDto]

  }

  final case class AggregateCountsDto(studies: Long, centres: Long, users: Long)

  object AggregateCountsDto {

    implicit val aggregateCountsDtoWriter: Writes[AggregateCountsDto] = Json.writes[AggregateCountsDto]

  }

  final case class CollectionSpecimenDefinitionNames(
      id:                      String,
      slug:                    Slug,
      name:                    String,
      specimenDefinitionNames: Set[EntityInfoDto])

  object CollectionSpecimenDefinitionNames {

    implicit val specimenDefinitionNamesFormat: Format[CollectionSpecimenDefinitionNames] =
      Json.format[CollectionSpecimenDefinitionNames]

  }

  final case class ProcessedSpecimenDefinitionNames(
      id:                     String,
      slug:                   Slug,
      name:                   String,
      specimenDefinitionName: EntityInfoDto)

  object ProcessedSpecimenDefinitionNames {

    implicit val specimenDefinitionNamesFormat: Format[ProcessedSpecimenDefinitionNames] =
      Json.format[ProcessedSpecimenDefinitionNames]

  }

  final case class ParticipantDto(
      id:           String,
      slug:         Slug,
      study:        EntityInfoDto,
      version:      Long,
      timeAdded:    String,
      timeModified: Option[String],
      uniqueId:     String,
      annotations:  Set[Annotation])

  object ParticipantDto {

    implicit val participantDtoForma: Format[ParticipantDto] = Json.format[ParticipantDto]

  }

  final case class CollectionEventDto(
      id:                      String,
      slug:                    Slug,
      participantId:           String,
      participantSlug:         String,
      collectionEventTypeId:   String,
      collectionEventTypeSlug: String,
      version:                 Long,
      timeAdded:               String,
      timeModified:            Option[String],
      timeCompleted:           String,
      visitNumber:             Int,
      annotations:             Set[Annotation])

  object CollectionEventDto {

    implicit val collectionEventDtoWriter: Writes[CollectionEventDto] = Json.writes[CollectionEventDto]

  }

  final case class UserDto(
      id:           String,
      version:      Long,
      timeAdded:    String,
      timeModified: Option[String],
      state:        String,
      slug:         Slug,
      name:         String,
      email:        String,
      avatarUrl:    Option[String],
      roles:        Set[UserRoleDto],
      membership:   Option[UserMembershipDto])

  object UserDto {

    implicit val userDtoFormat: Format[UserDto] = Json.format[UserDto]

  }

  final case class SpecimenDto(
      id:                      String,
      version:                 Long,
      timeAdded:               String,
      timeModified:            Option[String],
      state:                   String,
      slug:                    Slug,
      inventoryId:             String,
      collectionEventId:       String,
      specimenDefinitionId:    String,
      specimenDefinitionName:  String,
      specimenDefinitionUnits: String,
      originLocationInfo:      CentreLocationInfo,
      locationInfo:            CentreLocationInfo,
      containerId:             Option[String],
      position:                Option[String],
      timeCreated:             String,
      amount:                  BigDecimal,
      units:                   String,
      isDefaultAmount:         Boolean,
      eventTypeName:           String) {
    override def toString: String =
      s"""|${this.getClass.getSimpleName}: {
          |  id:                      $id,
          |  version:                 $version,
          |  timeAdded:               $timeAdded,
          |  timeModified:            $timeModified,
          |  state:                   $state,
          |  slug:                    $slug,
          |  inventoryId:             $inventoryId,
          |  collectionEventId:       $collectionEventId,
          |  specimenDefinitionId:    $specimenDefinitionId,
          |  specimenDefinitionName:  $specimenDefinitionName,
          |  specimenDefinitionUnits: $specimenDefinitionUnits,
          |  originLocationInfo:      $originLocationInfo,
          |  locationInfo:            $locationInfo,
          |  containerId:             $containerId,
          |  position:                $position,
          |  timeCreated:             $timeCreated,
          |  amount:                  $amount,
          |  units:                   $units,
          |  isDefaultAmount:         $isDefaultAmount,
          |  eventTypeName:           $eventTypeName
          |}""".stripMargin

  }

  object SpecimenDto {

    implicit val specimenDtoFormat: Format[SpecimenDto] = Json.format[SpecimenDto]

  }

  final case class ShipmentDto(
      id:               String,
      version:          Long,
      timeAdded:        String,
      timeModified:     Option[String],
      state:            String,
      courierName:      String,
      trackingNumber:   String,
      fromLocationInfo: CentreLocationInfo,
      toLocationInfo:   CentreLocationInfo,
      timePacked:       Option[String],
      timeSent:         Option[String],
      timeReceived:     Option[String],
      timeUnpacked:     Option[String],
      timeCompleted:    Option[String],
      specimenCount:    Int,
      containerCount:   Int)

  object ShipmentDto {

    val sort2Compare: Map[String, (ShipmentDto, ShipmentDto) => Boolean] =
      Map[String, (ShipmentDto, ShipmentDto) => Boolean]("courierName" -> compareByCourier,
                                                         "trackingNumber"   -> compareByTrackingNumber,
                                                         "state"            -> compareByState,
                                                         "fromLocationName" -> compareByFromLocation,
                                                         "toLocationName"   -> compareByToLocation)

    def create(
        shipment:         Shipment,
        fromLocationInfo: CentreLocationInfo,
        toLocationInfo:   CentreLocationInfo,
        specimenCount:    Int,
        containerCount:   Int
      ): ShipmentDto =
      ShipmentDto(id               = shipment.id.id,
                  version          = shipment.version,
                  timeAdded        = shipment.timeAdded.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                  timeModified     = shipment.timeModified.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                  courierName      = shipment.courierName,
                  trackingNumber   = shipment.trackingNumber,
                  fromLocationInfo = fromLocationInfo,
                  toLocationInfo   = toLocationInfo,
                  timePacked       = shipment.timePacked.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                  timeSent         = shipment.timeSent.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                  timeReceived     = shipment.timeReceived.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                  timeUnpacked     = shipment.timeUnpacked.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                  timeCompleted    = shipment.timeCompleted.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                  specimenCount    = specimenCount,
                  containerCount   = containerCount,
                  state            = shipment.state.id)

    def compareByCourier(a: ShipmentDto, b: ShipmentDto): Boolean =
      (a.courierName compareToIgnoreCase b.courierName) < 0

    def compareByTrackingNumber(a: ShipmentDto, b: ShipmentDto): Boolean =
      (a.trackingNumber compareToIgnoreCase b.trackingNumber) < 0

    def compareByState(a: ShipmentDto, b: ShipmentDto): Boolean =
      (a.state.toString compareToIgnoreCase b.state.toString) < 0

    def compareByFromLocation(a: ShipmentDto, b: ShipmentDto): Boolean =
      (a.fromLocationInfo.name compareToIgnoreCase b.fromLocationInfo.name) < 0

    def compareByToLocation(a: ShipmentDto, b: ShipmentDto): Boolean =
      (a.toLocationInfo.name compareToIgnoreCase b.toLocationInfo.name) < 0

    implicit val shipmentDtoFormat: Format[ShipmentDto] = Json.format[ShipmentDto]

  }

  final case class ShipmentSpecimenDto(
      id:                  String,
      version:             Long,
      timeAdded:           String,
      timeModified:        Option[String],
      state:               String,
      shipmentId:          String,
      shipmentContainerId: Option[String],
      specimen:            SpecimenDto) {

    override def toString: String =
      s"""|${this.getClass.getSimpleName}: {
          |  id:                  $id,
          |  version:             $version,
          |  timeAdded:           $timeAdded,
          |  timeModified:        $timeModified,
          |  state:               $state,
          |  shipmentId:          $shipmentId,
          |  shipmentContainerId: $shipmentContainerId,
          |  specimen:            $specimen,
          |}""".stripMargin

  }

  object ShipmentSpecimenDto {

    val sort2Compare: Map[String, (ShipmentSpecimenDto, ShipmentSpecimenDto) => Boolean] =
      Map[String, (ShipmentSpecimenDto, ShipmentSpecimenDto) => Boolean](
        "inventoryId" -> compareByInventoryId,
        "state"       -> compareByState,
        "specName"    -> compareBySpecName,
        "timeCreated" -> compareByTimeCreated
      )

    def compareByInventoryId(a: ShipmentSpecimenDto, b: ShipmentSpecimenDto): Boolean =
      (a.specimen.inventoryId compareTo b.specimen.inventoryId) < 0

    def compareByState(a: ShipmentSpecimenDto, b: ShipmentSpecimenDto): Boolean =
      (a.state compareTo b.state) < 0

    def compareBySpecName(a: ShipmentSpecimenDto, b: ShipmentSpecimenDto): Boolean =
      (a.specimen.specimenDefinitionName compareTo b.specimen.specimenDefinitionName) < 0

    def compareByTimeCreated(a: ShipmentSpecimenDto, b: ShipmentSpecimenDto): Boolean =
      (a.specimen.timeCreated compareTo b.specimen.timeCreated) < 0

    implicit val shipmentSpecimenDtoFormat: Format[ShipmentSpecimenDto] = Json.format[ShipmentSpecimenDto]
  }

}
