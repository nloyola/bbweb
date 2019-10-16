package org.biobank

import java.time.format.DateTimeFormatter
import org.biobank.domain._
import org.biobank.domain.AnatomicalSourceType._
import org.biobank.domain.PreservationType._
import org.biobank.domain.PreservationTemperature._
import org.biobank.domain.SpecimenType._
import org.biobank.domain.annotations.Annotation
import org.biobank.domain.centres._
import org.biobank.domain.containers._
import org.biobank.domain.studies._
import org.biobank.domain.participants._
import org.biobank.dto.access.{UserMembershipDto, UserRoleDto}
import play.api.libs.json._

package dto {

  trait Dto

  trait EntityInfo {
    val id:   String
    val slug: Slug
    val name: String
  }

  final case class EntityInfoDto(id: String, slug: Slug, name: String) extends EntityInfo

  object EntityInfoDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply[T <: IdentifiedDomainObject[_] with HasSlug with HasName](entity: T): EntityInfoDto =
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

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(centre: Centre, studies: Set[Study]): CentreDto =
      CentreDto(id           = centre.id.id,
                version      = centre.version,
                timeAdded    = centre.timeAdded.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                timeModified = centre.timeModified.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                state        = centre.state.id,
                slug         = centre.slug,
                name         = centre.name,
                description  = centre.description,
                studyNames   = studies.map(EntityInfoAndStateDto(_)),
                locations    = centre.locations)

    implicit val centreDtoFormat: Format[CentreDto] = Json.format[CentreDto]

  }

  final case class CentreLocationInfo(
      id:           String,
      slug:         Slug,
      name:         String,
      location:     EntityInfoDto,
      combinedName: String)
      extends EntityInfo

  object CentreLocationInfo {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(centre: Centre, location: Location): CentreLocationInfo =
      CentreLocationInfo(id           = centre.id.id,
                         slug         = centre.slug,
                         name         = centre.name,
                         location     = EntityInfoDto(location),
                         combinedName = s"${centre.name}: ${location.name}")

    implicit val centreLocationInfoFormat: Format[CentreLocationInfo] = Json.format[CentreLocationInfo]

  }

  final case class ContainerSchemaDto(
      id:           String,
      version:      Long,
      timeAdded:    String,
      timeModified: Option[String],
      slug:         Slug,
      name:         String,
      description:  Option[String],
      shared:       Boolean,
      centre:       EntityInfoDto,
      labels:       Set[String]) {

    override def toString: String =
      s"""|${this.getClass.getSimpleName}: {
          |  id:           $id,
          |  version:      $version,
          |  timeAdded:    $timeAdded,
          |  timeModified: $timeModified,
          |  slug:         $slug,
          |  name:         $name,
          |  description:  $description,
          |  shared:       $shared,
          |  centre:       $centre,
          |  labels:       $labels
          |}""".stripMargin
  }

  object ContainerSchemaDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(schema: ContainerSchema, centre: Centre): ContainerSchemaDto =
      ContainerSchemaDto(id        = schema.id.id,
                         version   = schema.version,
                         timeAdded = schema.timeAdded.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                         timeModified =
                           schema.timeModified.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                         slug        = schema.slug,
                         name        = schema.name,
                         description = schema.description,
                         shared      = schema.shared,
                         centre      = EntityInfoDto(centre),
                         labels      = schema.labels)

    implicit val containerSchemaDtoFormat: Format[ContainerSchemaDto] = Json.format[ContainerSchemaDto]

  }

  final case class ContainerTypeDto(
      id:           String,
      version:      Long,
      timeAdded:    String,
      timeModified: Option[String],
      slug:         Slug,
      name:         String,
      description:  Option[String],
      centre:       EntityInfoAndStateDto,
      schema:       EntityInfoDto,
      shared:       Boolean,
      enabled:      Boolean,
      storageType:  String) {

    override def toString: String =
      s"""|${this.getClass.getSimpleName}: {
          |  id:           $id,
          |  version:      $version,
          |  timeAdded:    $timeAdded,
          |  timeModified: $timeModified,
          |  slug:         $slug,
          |  name:         $name,
          |  description:  $description,
          |  centre:       $centre,
          |  schema:       $schema,
          |  shared:       $shared,
          |  enabled       $enabled,
          |  storageType   $storageType
          |}""".stripMargin

  }

  object ContainerTypeDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(containerType: ContainerType, centre: Centre, schema: ContainerSchema): ContainerTypeDto =
      ContainerTypeDto(id        = containerType.id.id,
                       version   = containerType.version,
                       timeAdded = containerType.timeAdded.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                       timeModified =
                         containerType.timeModified.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                       slug        = containerType.slug,
                       name        = containerType.name,
                       description = containerType.description,
                       centre      = EntityInfoAndStateDto(centre),
                       schema      = EntityInfoDto(schema),
                       shared      = containerType.shared,
                       enabled     = containerType.enabled,
                       storageType = containerType.storageType.id)

    implicit val containerTypeDtoFormat: Format[ContainerTypeDto] = Json.format[ContainerTypeDto]

  }

  final case class ContainerConstraintsDto(
      name:              String,
      description:       Option[String],
      anatomicalSources: Set[AnatomicalSourceType],
      preservationTypes: Set[PreservationType],
      specimenTypes:     Set[SpecimenType])

  object ContainerConstraintsDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(constraints: ContainerConstraints): ContainerConstraintsDto =
      ContainerConstraintsDto(name              = constraints.name,
                              description       = constraints.description,
                              anatomicalSources = constraints.anatomicalSources,
                              preservationTypes = constraints.preservationTypes,
                              specimenTypes     = constraints.specimenTypes)

    implicit val containerConstraintsDtoFormat: Format[ContainerConstraintsDto] =
      Json.format[ContainerConstraintsDto]

  }

  trait ContainerDto {
    val id:            String
    val version:       Long
    val timeAdded:     String
    val timeModified:  Option[String]
    val slug:          Slug
    val inventoryId:   String
    val containerType: EntityInfoDto
    val storageType:   String

    override def toString: String = Json.prettyPrint(Json.toJson(this))

  }

  object ContainerDto {

    implicit val containerDtoWrites: Format[ContainerDto] = new Format[ContainerDto] {

      override def writes(container: ContainerDto): JsValue =
        container match {
          case c: RootContainerDto     => Json.toJson(c)(RootContainerDto.rootContainerDtoFormat)
          case c: StorageContainerDto  => Json.toJson(c)(StorageContainerDto.storageContainerDtoFormat)
          case c: SpecimenContainerDto => Json.toJson(c)(SpecimenContainerDto.specimenContainerDtoFormat)
        }

      override def reads(json: JsValue): JsResult[ContainerDto] = (json \ "storageType") match {
        case JsDefined(JsString(Container.rootStorage.id))      => json.validate[RootContainerDto]
        case JsDefined(JsString(Container.containerStorage.id)) => json.validate[StorageContainerDto]
        case JsDefined(JsString(Container.specimenStorage.id))  => json.validate[SpecimenContainerDto]
        case _                                                  => JsError("error")
      }
    }

  }

  final case class RootContainerDto(
      id:                 String,
      version:            Long,
      timeAdded:          String,
      timeModified:       Option[String],
      slug:               Slug,
      label:              String,
      inventoryId:        String,
      enabled:            Boolean,
      containerType:      EntityInfoDto,
      centreLocationInfo: CentreLocationInfo,
      temperature:        PreservationTemperature,
      constraints:        Option[ContainerConstraintsDto],
      storageType:        String)
      extends ContainerDto {}

  object RootContainerDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(
        c:             RootContainer,
        containerType: ContainerType,
        centre:        Centre,
        location:      Location
      ): RootContainerDto =
      RootContainerDto(id                 = c.id.id,
                       version            = c.version,
                       timeAdded          = c.timeAdded.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                       timeModified       = c.timeModified.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                       slug               = c.slug,
                       label              = c.label,
                       inventoryId        = c.inventoryId,
                       enabled            = c.enabled,
                       containerType      = EntityInfoDto(containerType),
                       centreLocationInfo = CentreLocationInfo(centre, location),
                       temperature        = c.temperature,
                       constraints        = c.constraints.map(ContainerConstraintsDto(_)),
                       storageType        = c.storageType.id)

    implicit val rootContainerDtoFormat: Format[RootContainerDto] = Json.format[RootContainerDto]

  }

  final case class ContainerInfoDto(id: String, slug: String, label: String)

  object ContainerInfoDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(container: Container): ContainerInfoDto =
      container match {
        case c: RootContainer     => apply(c)
        case c: StorageContainer  => apply(c)
        case c: SpecimenContainer => apply(c)
      }

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(container: RootContainer): ContainerInfoDto =
      ContainerInfoDto(id = container.id.id, slug = container.slug.id, label = container.label)

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(container: StorageContainer): ContainerInfoDto =
      ContainerInfoDto(id = container.id.id, slug = container.slug.id, label = container.schemaLabel.label)

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(container: SpecimenContainer): ContainerInfoDto =
      ContainerInfoDto(id = container.id.id, slug = container.slug.id, label = container.schemaLabel.label)

    implicit val containerInfoDtoFormat: Format[ContainerInfoDto] = Json.format[ContainerInfoDto]

  }

  final case class StorageContainerDto(
      id:            String,
      version:       Long,
      timeAdded:     String,
      timeModified:  Option[String],
      slug:          Slug,
      inventoryId:   String,
      enabled:       Boolean,
      containerType: EntityInfoDto,
      parent:        ContainerInfoDto,
      label:         String,
      constraints:   Option[ContainerConstraintsDto],
      storageType:   String)
      extends ContainerDto {}

  object StorageContainerDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(c: StorageContainer, containerType: ContainerType, parent: Container): StorageContainerDto = {
      val constraintsDto = c.constraints match {
        case Some(constraints) => Some(ContainerConstraintsDto(constraints))
        case _                 => None
      }

      StorageContainerDto(id            = c.id.id,
                          version       = c.version,
                          timeAdded     = c.timeAdded.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                          timeModified  = c.timeModified.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                          slug          = c.slug,
                          inventoryId   = c.inventoryId,
                          enabled       = c.enabled,
                          containerType = EntityInfoDto(containerType),
                          parent        = ContainerInfoDto(parent),
                          label         = c.schemaLabel.label,
                          constraints   = constraintsDto,
                          storageType   = c.storageType.id)
    }

    implicit val storageContainerDtoFormat: Format[StorageContainerDto] = Json.format[StorageContainerDto]

  }

  final case class SpecimenContainerDto(
      id:            String,
      version:       Long,
      timeAdded:     String,
      timeModified:  Option[String],
      slug:          Slug,
      inventoryId:   String,
      containerType: EntityInfoDto,
      parent:        ContainerInfoDto,
      label:         String,
      storageType:   String)
      extends ContainerDto {}

  object SpecimenContainerDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(c: SpecimenContainer, containerType: ContainerType, parent: Container): SpecimenContainerDto =
      SpecimenContainerDto(id            = c.id.id,
                           version       = c.version,
                           timeAdded     = c.timeAdded.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                           timeModified  = c.timeModified.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                           slug          = c.slug,
                           inventoryId   = c.inventoryId,
                           containerType = EntityInfoDto(containerType),
                           parent        = ContainerInfoDto(parent),
                           label         = c.schemaLabel.label,
                           storageType   = c.storageType.id)

    implicit val specimenContainerDtoFormat: Format[SpecimenContainerDto] = Json.format[SpecimenContainerDto]

  }

  final case class ContainerInfo(id: String, slug: String, inventoryId: String, label: String)

  object ContainerInfo {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(container: Container): ContainerInfo = {
      ContainerInfo(container.id.id, container.slug.id, container.inventoryId, container.getLabel)
    }

    implicit val containerInfoFormat: Format[ContainerInfo] = Json.format[ContainerInfo]

  }

  final case class ContainerChildrenInfo(container: ContainerInfo, children: Set[ContainerInfo])

  object ContainerChildrenInfo {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(container: Container, children: Set[ContainerInfo]): ContainerChildrenInfo = {
      ContainerChildrenInfo(ContainerInfo(container), children)
    }

    implicit val containerChildrenInfoFormat: Format[ContainerChildrenInfo] =
      Json.format[ContainerChildrenInfo]

  }

  final case class AggregateCountsDto(studies: Long, centres: Long, users: Long)

  object AggregateCountsDto {

    implicit val aggregateCountsDtoWriter: Writes[AggregateCountsDto] = Json.writes[AggregateCountsDto]

  }

  final case class CollectedSpecimenDefinitionNames(
      id:                      String,
      slug:                    Slug,
      name:                    String,
      specimenDefinitionNames: Set[EntityInfoDto])

  object CollectedSpecimenDefinitionNames {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(eventType: CollectionEventType): CollectedSpecimenDefinitionNames = {
      val definitionNames = eventType.specimenDefinitions.map(sd => EntityInfoDto(sd))
      CollectedSpecimenDefinitionNames(eventType.id.id, eventType.slug, eventType.name, definitionNames)
    }

    implicit val specimenDefinitionNamesFormat: Format[CollectedSpecimenDefinitionNames] =
      Json.format[CollectedSpecimenDefinitionNames]

  }

  final case class ProcessedSpecimenDefinitionName(
      id:                     String,
      slug:                   Slug,
      name:                   String,
      specimenDefinitionName: EntityInfoDto)

  object ProcessedSpecimenDefinitionName {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(processingType: ProcessingType): ProcessedSpecimenDefinitionName =
      ProcessedSpecimenDefinitionName(processingType.id.id,
                                      processingType.slug,
                                      processingType.name,
                                      EntityInfoDto(processingType.output.specimenDefinition))

    implicit val specimenDefinitionNamesFormat: Format[ProcessedSpecimenDefinitionName] =
      Json.format[ProcessedSpecimenDefinitionName]

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

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(participant: Participant, study: Study): ParticipantDto =
      ParticipantDto(id        = participant.id.id,
                     slug      = participant.slug,
                     study     = EntityInfoDto(study),
                     version   = participant.version,
                     timeAdded = participant.timeAdded.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                     timeModified =
                       participant.timeModified.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                     uniqueId    = participant.uniqueId,
                     annotations = participant.annotations)

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

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(
        event:       CollectionEvent,
        participant: Participant,
        eventType:   CollectionEventType
      ): CollectionEventDto =
      CollectionEventDto(id                      = event.id.id,
                         participantId           = participant.id.id,
                         participantSlug         = participant.slug.id,
                         collectionEventTypeId   = eventType.id.id,
                         collectionEventTypeSlug = eventType.slug.id,
                         version                 = event.version,
                         timeAdded               = event.timeAdded.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                         timeModified =
                           event.timeModified.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                         slug          = event.slug,
                         timeCompleted = event.timeCompleted.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                         visitNumber   = event.visitNumber,
                         annotations   = event.annotations)

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
      label:                   Option[String],
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
          |  label:                   $label,
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
      id:             String,
      version:        Long,
      timeAdded:      String,
      timeModified:   Option[String],
      state:          String,
      courierName:    String,
      trackingNumber: String,
      origin:         CentreLocationInfo,
      destination:    CentreLocationInfo,
      timePacked:     Option[String],
      timeSent:       Option[String],
      timeReceived:   Option[String],
      timeUnpacked:   Option[String],
      timeCompleted:  Option[String],
      specimenCount:  Int,
      containerCount: Int)

  object ShipmentDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(
        shipment:       Shipment,
        origin:         CentreLocationInfo,
        destination:    CentreLocationInfo,
        specimenCount:  Int,
        containerCount: Int
      ): ShipmentDto =
      ShipmentDto(id             = shipment.id.id,
                  version        = shipment.version,
                  timeAdded      = shipment.timeAdded.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                  timeModified   = shipment.timeModified.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                  courierName    = shipment.courierName,
                  trackingNumber = shipment.trackingNumber,
                  origin         = origin,
                  destination    = destination,
                  timePacked     = shipment.timePacked.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                  timeSent       = shipment.timeSent.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                  timeReceived   = shipment.timeReceived.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                  timeUnpacked   = shipment.timeUnpacked.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                  timeCompleted  = shipment.timeCompleted.map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                  specimenCount  = specimenCount,
                  containerCount = containerCount,
                  state          = shipment.state.id)

    val sort2Compare: Map[String, (ShipmentDto, ShipmentDto) => Boolean] =
      Map[String, (ShipmentDto, ShipmentDto) => Boolean]("courierName"    -> compareByCourier,
                                                         "trackingNumber" -> compareByTrackingNumber,
                                                         "state"          -> compareByState,
                                                         "origin"         -> compareByOrigin,
                                                         "destination"    -> compareByDestination)

    def compareByCourier(a: ShipmentDto, b: ShipmentDto): Boolean =
      (a.courierName compareToIgnoreCase b.courierName) < 0

    def compareByTrackingNumber(a: ShipmentDto, b: ShipmentDto): Boolean =
      (a.trackingNumber compareToIgnoreCase b.trackingNumber) < 0

    def compareByState(a: ShipmentDto, b: ShipmentDto): Boolean =
      (a.state.toString compareToIgnoreCase b.state.toString) < 0

    def compareByOrigin(a: ShipmentDto, b: ShipmentDto): Boolean =
      (a.origin.combinedName compareToIgnoreCase b.origin.combinedName) < 0

    def compareByDestination(a: ShipmentDto, b: ShipmentDto): Boolean =
      (a.destination.combinedName compareToIgnoreCase b.destination.combinedName) < 0

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

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(shipmentSpecimen: ShipmentSpecimen, specimen: SpecimenDto): ShipmentSpecimenDto =
      ShipmentSpecimenDto(id      = shipmentSpecimen.id.id,
                          version = shipmentSpecimen.version,
                          timeAdded =
                            shipmentSpecimen.timeAdded.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                          timeModified = shipmentSpecimen.timeModified
                            .map(_.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
                          shipmentId          = shipmentSpecimen.shipmentId.id,
                          shipmentContainerId = shipmentSpecimen.shipmentContainerId.map(id => id.id),
                          state               = shipmentSpecimen.state.toString,
                          specimen            = specimen)

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
