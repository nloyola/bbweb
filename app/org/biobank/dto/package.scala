package org.biobank

import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import org.biobank.domain._
import org.biobank.domain.annotations.Annotation
import org.biobank.domain.centres._
import org.biobank.domain.containers._
import org.biobank.domain.studies._
import org.biobank.domain.participants._
import org.biobank.domain.users.{User, UserId}
import org.biobank.dto.access.{UserMembershipDto, UserRoleDto}
import play.api.libs.json._

package dto {

  trait Dto

  trait EntityInfo[ID] extends HasSlug {
    val id:   ID
    val slug: Slug
  }

  trait NamedEntityInfo[ID] extends EntityInfo[ID] with HasName {
    val name: String
  }

  trait EntitySetDto[T <: NamedEntityInfo[_]] {
    val allEntities: Boolean
    val entityData:  Set[T]
  }

  trait EntityInfoAndState[ID] extends NamedEntityInfo[ID] {
    val state: EntityState
  }

  final case class StudyInfoDto(id: StudyId, slug: Slug, name: String) extends NamedEntityInfo[StudyId]

  object StudyInfoDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(study: Study): StudyInfoDto = StudyInfoDto(study.id, study.slug, study.name)

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val studyInfoDtoFormat: Format[StudyInfoDto] = Json.format[StudyInfoDto]

  }

  final case class StudyInfoAndStateDto(id: StudyId, slug: Slug, name: String, state: EntityState)
      extends EntityInfoAndState[StudyId]

  object StudyInfoAndStateDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(study: Study): StudyInfoAndStateDto =
      StudyInfoAndStateDto(study.id, study.slug, study.name, study.state)

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val studyInfoAndStateDtoFormat: Format[StudyInfoAndStateDto] =
      Json.format[StudyInfoAndStateDto]

  }

  final case class StudySetDto(allEntities: Boolean, entityData: Set[StudyInfoDto])
      extends EntitySetDto[StudyInfoDto]

  object StudySetDto {

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val studySetDtoFormat: Format[StudySetDto] = Json.format[StudySetDto]

  }

  final case class CollectionEventTypeInfoDto(id: CollectionEventTypeId, slug: Slug, name: String)
      extends NamedEntityInfo[CollectionEventTypeId]

  object CollectionEventTypeInfoDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(eventType: CollectionEventType): CollectionEventTypeInfoDto =
      CollectionEventTypeInfoDto(eventType.id, eventType.slug, eventType.name)

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val collectionEventTypeInfoDtoFormat: Format[CollectionEventTypeInfoDto] =
      Json.format[CollectionEventTypeInfoDto]

  }

  final case class CentreInfoDto(id: CentreId, slug: Slug, name: String) extends NamedEntityInfo[CentreId]

  object CentreInfoDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(centre: Centre): CentreInfoDto = CentreInfoDto(centre.id, centre.slug, centre.name)

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val centreInfoDtoFormat: Format[CentreInfoDto] = Json.format[CentreInfoDto]

  }

  final case class CentreSetDto(allEntities: Boolean, entityData: Set[CentreInfoDto])
      extends EntitySetDto[CentreInfoDto]

  object CentreSetDto {

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val centreSetDtoFormat: Format[CentreSetDto] = Json.format[CentreSetDto]

  }

  final case class CentreInfoAndStateDto(id: CentreId, slug: Slug, name: String, state: EntityState)
      extends EntityInfoAndState[CentreId]

  object CentreInfoAndStateDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(centre: Centre): CentreInfoAndStateDto =
      CentreInfoAndStateDto(centre.id, centre.slug, centre.name, centre.state)

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val centreInfoAndStateDtoFormat: Format[CentreInfoAndStateDto] =
      Json.format[CentreInfoAndStateDto]

  }

  final case class LocationInfoDto(id: LocationId, slug: Slug, name: String)
      extends NamedEntityInfo[LocationId]

  object LocationInfoDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(location: Location): LocationInfoDto =
      LocationInfoDto(location.id, location.slug, location.name)

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val locationInfoDtoFormat: Format[LocationInfoDto] = Json.format[LocationInfoDto]

  }

  final case class UserInfoDto(id: UserId, slug: Slug, name: String) extends NamedEntityInfo[UserId]

  object UserInfoDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(user: User): UserInfoDto = UserInfoDto(user.id, user.slug, user.name)

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val userInfoDtoFormat: Format[UserInfoDto] = Json.format[UserInfoDto]

  }

  final case class UserInfoAndStateDto(id: UserId, slug: Slug, name: String, state: EntityState)
      extends EntityInfoAndState[UserId]

  object UserInfoAndStateDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(user: User): UserInfoAndStateDto =
      UserInfoAndStateDto(user.id, user.slug, user.name, user.state)

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(user: UserDto): UserInfoAndStateDto =
      UserInfoAndStateDto(user.id, user.slug, user.name, user.state)

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val userInfoAndStateDtoFormat: Format[UserInfoAndStateDto] =
      Json.format[UserInfoAndStateDto]

  }

  final case class ContainerTypeInfoDto(id: ContainerTypeId, slug: Slug, name: String)
      extends NamedEntityInfo[ContainerTypeId]

  object ContainerTypeInfoDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(containerType: ContainerType): ContainerTypeInfoDto =
      ContainerTypeInfoDto(containerType.id, containerType.slug, containerType.name)

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val containerTypeInfoDtoFormat: Format[ContainerTypeInfoDto] = Json.format[ContainerTypeInfoDto]

  }

  final case class ContainerSchemaInfoDto(id: ContainerSchemaId, slug: Slug, name: String)
      extends NamedEntityInfo[ContainerSchemaId]

  object ContainerSchemaInfoDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(containerSchema: ContainerSchema): ContainerSchemaInfoDto =
      ContainerSchemaInfoDto(containerSchema.id, containerSchema.slug, containerSchema.name)

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val containerSchemaInfoDtoFormat: Format[ContainerSchemaInfoDto] =
      Json.format[ContainerSchemaInfoDto]

  }

  final case class SpecimenDefinitionInfoDto(id: SpecimenDefinitionId, slug: Slug, name: String)
      extends NamedEntityInfo[SpecimenDefinitionId]

  object SpecimenDefinitionInfoDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(specimenDefinition: SpecimenDefinition): SpecimenDefinitionInfoDto =
      SpecimenDefinitionInfoDto(specimenDefinition.id, specimenDefinition.slug, specimenDefinition.name)

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val specimenDefinitionInfoDtoFormat: Format[SpecimenDefinitionInfoDto] =
      Json.format[SpecimenDefinitionInfoDto]

  }

  final case class ContainerSchemaDto(
      id:           ContainerSchemaId,
      version:      Long,
      timeAdded:    OffsetDateTime,
      timeModified: Option[OffsetDateTime],
      slug:         Slug,
      name:         String,
      description:  Option[String],
      shared:       Boolean,
      centre:       CentreInfoDto,
      labels:       Set[String]) {

    override def toString: String = s"|${this.getClass.getSimpleName}: ${Json.prettyPrint(Json.toJson(this))}"
  }

  object ContainerSchemaDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(schema: ContainerSchema, centre: Centre): ContainerSchemaDto =
      ContainerSchemaDto(id           = schema.id,
                         version      = schema.version,
                         timeAdded    = schema.timeAdded,
                         timeModified = schema.timeModified,
                         slug         = schema.slug,
                         name         = schema.name,
                         description  = schema.description,
                         shared       = schema.shared,
                         centre       = CentreInfoDto(centre),
                         labels       = schema.labels)

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val containerSchemaDtoFormat: Format[ContainerSchemaDto] = Json.format[ContainerSchemaDto]

  }

  final case class ContainerTypeDto(
      id:           ContainerTypeId,
      version:      Long,
      timeAdded:    OffsetDateTime,
      timeModified: Option[OffsetDateTime],
      slug:         Slug,
      name:         String,
      description:  Option[String],
      centre:       CentreInfoAndStateDto,
      schema:       ContainerSchemaInfoDto,
      shared:       Boolean,
      enabled:      Boolean,
      storageType:  String) {

    override def toString: String = s"|${this.getClass.getSimpleName}: ${Json.prettyPrint(Json.toJson(this))}"

  }

  object ContainerTypeDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(containerType: ContainerType, centre: Centre, schema: ContainerSchema): ContainerTypeDto =
      ContainerTypeDto(id           = containerType.id,
                       version      = containerType.version,
                       timeAdded    = containerType.timeAdded,
                       timeModified = containerType.timeModified,
                       slug         = containerType.slug,
                       name         = containerType.name,
                       description  = containerType.description,
                       centre       = CentreInfoAndStateDto(centre),
                       schema       = ContainerSchemaInfoDto(schema),
                       shared       = containerType.shared,
                       enabled      = containerType.enabled,
                       storageType  = containerType.storageType.id)

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val containerTypeDtoFormat: Format[ContainerTypeDto] = Json.format[ContainerTypeDto]

  }

  final case class AggregateCountsDto(studies: Long, centres: Long, users: Long)

  object AggregateCountsDto {

    implicit val aggregateCountsDtoWriter: Writes[AggregateCountsDto] = Json.writes[AggregateCountsDto]

  }

  final case class CollectedSpecimenDefinitionNames(
      id:                      CollectionEventTypeId,
      slug:                    Slug,
      name:                    String,
      specimenDefinitionNames: Set[SpecimenDefinitionInfoDto])
      extends NamedEntityInfo[CollectionEventTypeId]

  object CollectedSpecimenDefinitionNames {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(eventType: CollectionEventType): CollectedSpecimenDefinitionNames = {
      val definitionNames = eventType.specimenDefinitions.map(sd => SpecimenDefinitionInfoDto(sd))
      CollectedSpecimenDefinitionNames(eventType.id, eventType.slug, eventType.name, definitionNames)
    }

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val specimenDefinitionNamesFormat: Format[CollectedSpecimenDefinitionNames] =
      Json.format[CollectedSpecimenDefinitionNames]

  }

  final case class ProcessedSpecimenDefinitionName(
      id:                     ProcessingTypeId,
      slug:                   Slug,
      name:                   String,
      specimenDefinitionName: SpecimenDefinitionInfoDto)
      extends NamedEntityInfo[ProcessingTypeId]

  object ProcessedSpecimenDefinitionName {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(processingType: ProcessingType): ProcessedSpecimenDefinitionName =
      ProcessedSpecimenDefinitionName(processingType.id,
                                      processingType.slug,
                                      processingType.name,
                                      SpecimenDefinitionInfoDto(processingType.output.specimenDefinition))

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val specimenDefinitionNamesFormat: Format[ProcessedSpecimenDefinitionName] =
      Json.format[ProcessedSpecimenDefinitionName]

  }

  final case class ParticipantDto(
      id:           ParticipantId,
      slug:         Slug,
      study:        StudyInfoDto,
      version:      Long,
      timeAdded:    OffsetDateTime,
      timeModified: Option[OffsetDateTime],
      uniqueId:     String,
      annotations:  Set[Annotation])
      extends Dto {

    override def toString: String = s"|${this.getClass.getSimpleName}: ${Json.prettyPrint(Json.toJson(this))}"

  }

  object ParticipantDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(participant: Participant, study: Study): ParticipantDto =
      ParticipantDto(id           = participant.id,
                     slug         = participant.slug,
                     study        = StudyInfoDto(study),
                     version      = participant.version,
                     timeAdded    = participant.timeAdded,
                     timeModified = participant.timeModified,
                     uniqueId     = participant.uniqueId,
                     annotations  = participant.annotations)

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val participantDtoFormat: Format[ParticipantDto] = Json.format[ParticipantDto]

  }

  final case class ParticipantInfoDto(id: ParticipantId, slug: Slug, uniqueId: String)
      extends EntityInfo[ParticipantId]

  object ParticipantInfoDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(participant: Participant): ParticipantInfoDto =
      ParticipantInfoDto(participant.id, participant.slug, participant.uniqueId)

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val participantInfoDtoFormat: Format[ParticipantInfoDto] = Json.format[ParticipantInfoDto]

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

  final case class CollectionEventInfoDto(id: CollectionEventId, slug: Slug, visitNumber: Int)
      extends EntityInfo[CollectionEventId]

  object CollectionEventInfoDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(collectionEvent: CollectionEvent): CollectionEventInfoDto =
      CollectionEventInfoDto(collectionEvent.id, collectionEvent.slug, collectionEvent.visitNumber)

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val collectionEventInfoDtoFormat: Format[CollectionEventInfoDto] =
      Json.format[CollectionEventInfoDto]

  }

  final case class UserDto(
      id:           UserId,
      version:      Long,
      timeAdded:    OffsetDateTime,
      timeModified: Option[OffsetDateTime],
      state:        EntityState,
      slug:         Slug,
      name:         String,
      email:        String,
      avatarUrl:    Option[String],
      roles:        Set[UserRoleDto],
      membership:   Option[UserMembershipDto])

  object UserDto {

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val userDtoFormat: Format[UserDto] = Json.format[UserDto]

  }

}
