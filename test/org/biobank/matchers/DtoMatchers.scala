package org.biobank.matchers

import java.time.OffsetDateTime
import org.biobank.domain._
import org.biobank.domain.access._
import org.biobank.domain.centres._
import org.biobank.domain.containers._
import org.biobank.domain.participants._
import org.biobank.domain.studies._
import org.biobank.domain.users._
import org.biobank.dto._
import org.biobank.dto.access._
import play.api.libs.json._
import org.scalatest.Matchers._
import org.scalatest.matchers.{MatchResult, Matcher}

trait DtoMatchers {
  import JsonMatchers._
  import DateMatchers._

  def matchDtoToCentre(centre: Centre) =
    new Matcher[CentreDto] {

      def apply(left: CentreDto) = {
        val dtoStudyIds = left.studyNames.map(sn => StudyId(sn.id)).toList.sortBy(_.id)
        val timeAddedMatcher =
          beTimeWithinSeconds(centre.timeAdded, 5L)(OffsetDateTime.parse(left.timeAdded))
        val timeModifiedMatcher = beOptionalTimeWithinSeconds(centre.timeModified, 5L)
          .apply(left.timeModified.map(OffsetDateTime.parse))

        val matchers = Map(("id" -> (left.id equals centre.id.id)),
                           ("version"      -> (left.version equals centre.version)),
                           ("timeAdded"    -> (timeAddedMatcher.matches)),
                           ("timeModified" -> (timeModifiedMatcher.matches)),
                           ("slug"         -> (left.slug equals centre.slug)),
                           ("state"        -> (left.state equals centre.state.id)),
                           ("name"         -> (left.name equals centre.name)),
                           ("description"  -> (left.description equals centre.description)),
                           ("studyIds"     -> (dtoStudyIds equals centre.studyIds.toList.sortBy(_.id))),
                           ("locations"    -> (left.locations equals centre.locations)))

        val nonMatching = matchers filter { case (k, v) => !v } keys

        MatchResult(nonMatching.size <= 0,
                    "dto does not match entity for the following attributes: {0},\ndto: {1},\nentity: {2}",
                    "dto matches entity: dto: {1},\nentity: {2}",
                    IndexedSeq(nonMatching.mkString(", "), left, centre))
      }
    }

  def matchDtoToShipment(shipment: Shipment) =
    new Matcher[ShipmentDto] {

      def apply(left: ShipmentDto) = {
        val timeAddedMatcher =
          beTimeWithinSeconds(shipment.timeAdded, 5L)(OffsetDateTime.parse(left.timeAdded))

        val fromLocationInfoMatcher =
          matchCentreLocationInfo(shipment.fromCentreId, shipment.fromLocationId)
            .apply(left.fromLocationInfo)

        val toLocationInfoMatcher =
          matchCentreLocationInfo(shipment.toCentreId, shipment.toLocationId)
            .apply(left.toLocationInfo)

        val matchers =
          Map(("id"               -> (left.id equals shipment.id.id)),
              ("version"          -> (left.version equals shipment.version)),
              ("timeAdded"        -> (timeAddedMatcher.matches)),
              ("timeModified"     -> optionalTimeWithinSeconds(left.timeModified, shipment.timeModified, 5L).matches),
              ("state"            -> (left.state equals shipment.state.id)),
              ("courierName"      -> (left.courierName equals shipment.courierName)),
              ("trackingNumber"   -> (left.trackingNumber equals shipment.trackingNumber)),
              ("fromLocationInfo" -> (fromLocationInfoMatcher.matches)),
              ("toLocationInfo"   -> (toLocationInfoMatcher.matches)),
              ("timePacked"       -> optionalTimeWithinSeconds(left.timePacked, shipment.timePacked, 5L).matches),
              ("timeSent"         -> optionalTimeWithinSeconds(left.timeSent, shipment.timeSent, 5L).matches),
              ("timeReceived"     -> optionalTimeWithinSeconds(left.timeReceived, shipment.timeReceived, 5L).matches),
              ("timeUnpacked"     -> optionalTimeWithinSeconds(left.timeUnpacked, shipment.timeUnpacked, 5L).matches),
              ("timeCompleted"    -> optionalTimeWithinSeconds(left.timeCompleted, shipment.timeCompleted, 5L).matches))

        val nonMatching = matchers filter { case (k, v) => !v } keys

        MatchResult(nonMatching.size <= 0,
                    "dto does not match entity for the following attributes: {0},\ndto: {1},\nentity: {2}",
                    "dto matches entity: dto: {1},\nentity: {2}",
                    IndexedSeq(nonMatching.mkString(", "), left, shipment))
      }
    }

  def matchDtoToSpecimen(specimen: Specimen) =
    new Matcher[SpecimenDto] {

      def apply(left: SpecimenDto) = {
        val timeAddedMatcher =
          beTimeWithinSeconds(specimen.timeAdded, 5L)(OffsetDateTime.parse(left.timeAdded))

        val timeCreatedMatcher =
          beTimeWithinSeconds(specimen.timeCreated, 5L)(OffsetDateTime.parse(left.timeCreated))

        val matchers =
          Map(("id"                   -> (left.id equals specimen.id.id)),
              ("version"              -> (left.version equals specimen.version)),
              ("timeAdded"            -> timeAddedMatcher.matches),
              ("timeModified"         -> optionalTimeWithinSeconds(left.timeModified, specimen.timeModified, 5L).matches),
              ("state"                -> (left.state equals specimen.state.id)),
              ("inventoryId"          -> (left.inventoryId equals specimen.inventoryId)),
              ("specimenDefinitionId" -> (left.specimenDefinitionId equals specimen.specimenDefinitionId.id)),
              ("originLocationInfo"   -> (left.originLocationInfo.locationId equals specimen.originLocationId.id)),
              ("locationInfo"         -> (left.locationInfo.locationId equals specimen.locationId.id)),
              ("containerId"          -> (left.containerId equals specimen.containerId)),
              ("label"                -> (left.label equals specimen.schemaLabel.map(_.label))),
              ("timeCreated"          -> timeCreatedMatcher.matches))

        val nonMatching = matchers filter { case (k, v) => !v } keys

        MatchResult(nonMatching.size <= 0,
                    "dto does not match specimen for the following attributes: {0},\ndto: {1},\nentity: {2}",
                    "dto matches specimen: dto: {1},\nentity: {2}",
                    IndexedSeq(nonMatching.mkString(", "), left, specimen))
      }
    }

  def matchDtoToShipmentSpecimen(shipmentSpecimen: ShipmentSpecimen) =
    new Matcher[ShipmentSpecimenDto] {

      def apply(left: ShipmentSpecimenDto) = {
        val timeAddedMatcher =
          beTimeWithinSeconds(shipmentSpecimen.timeAdded, 5L)(OffsetDateTime.parse(left.timeAdded))

        val matchers =
          Map(("id"        -> (left.id equals shipmentSpecimen.id.id)),
              ("version"   -> (left.version equals shipmentSpecimen.version)),
              ("timeAdded" -> (timeAddedMatcher.matches)),
              ("timeModified" -> optionalTimeWithinSeconds(left.timeModified,
                                                           shipmentSpecimen.timeModified,
                                                           5L).matches),
              ("state"      -> (left.state equals shipmentSpecimen.state.toString)),
              ("shipmentId" -> (left.shipmentId equals shipmentSpecimen.shipmentId.id)),
              ("shipmentContainerId" -> (left.shipmentContainerId equals
                shipmentSpecimen.shipmentContainerId.map(_.toString))),
              ("specimenId" -> (left.specimen.id equals shipmentSpecimen.specimenId.id)))

        val nonMatching = matchers filter { case (k, v) => !v } keys

        MatchResult(nonMatching.size <= 0,
                    "dto does not match entity for the following attributes: {0},\ndto: {1},\nentity: {2}",
                    "dto matches entity: dto: {1},\nentity: {2}",
                    IndexedSeq(nonMatching.mkString(", "), left, shipmentSpecimen))
      }
    }

  def matchDtoToUser(user: User) =
    new Matcher[UserDto] {

      def apply(left: UserDto) = {
        val timeAddedMatcher =
          beTimeWithinSeconds(user.timeAdded, 5L)(OffsetDateTime.parse(left.timeAdded))

        val timeModifiedMatcher = beOptionalTimeWithinSeconds(user.timeModified, 5L)
          .apply(left.timeModified.map(OffsetDateTime.parse))

        val matchers = Map(("id" -> (left.id equals user.id.id)),
                           ("version"      -> (left.version equals user.version)),
                           ("timeAdded"    -> timeAddedMatcher.matches),
                           ("timeModified" -> (timeModifiedMatcher.matches)),
                           ("state"        -> (left.state equals user.state.id)),
                           ("slug"         -> (left.slug equals user.slug)),
                           ("name"         -> (left.name equals user.name)),
                           ("email"        -> (left.email equals user.email)),
                           ("avatarUrl"    -> (left.avatarUrl equals user.avatarUrl)))

        val nonMatching = matchers filter { case (k, v) => !v } keys

        MatchResult(nonMatching.size <= 0,
                    "dto does not match user for the following attributes: {0},\ndto: {1},\nuser: {2}",
                    "dto matches user: dto: {1},\nuser: {2}",
                    IndexedSeq(nonMatching.mkString(", "), left, user))
      }
    }

  def matchDtoToRole(role: Role) =
    new Matcher[RoleDto] {

      def apply(left: RoleDto) = {
        val dtoUserIds     = left.userData.toList.map(ud   => UserId(ud.id)).sortBy(_.id)
        val dtoParentIds   = left.parentData.toList.map(pd => AccessItemId(pd.id)).sortBy(_.id)
        val dtoChildrenIds = left.childData.toList.map(cd  => AccessItemId(cd.id)).sortBy(_.id)

        val matchers = Map(("id" -> (left.id equals role.id.id)),
                           ("version"     -> (left.version equals role.version)),
                           ("slug"        -> (left.slug equals role.slug)),
                           ("name"        -> (left.name equals role.name)),
                           ("description" -> (left.description equals role.description)),
                           ("userIds"     -> (dtoUserIds equals role.userIds.toList.sortBy(_.id))),
                           ("parentIds"   -> (dtoParentIds equals role.parentIds.toList.sortBy(_.id))),
                           ("childrenIds" -> (dtoChildrenIds equals role.childrenIds.toList.sortBy(_.id))))

        val nonMatching = matchers filter { case (k, v) => !v } keys

        MatchResult(nonMatching.size <= 0,
                    "dto does not match role for the following attributes: {0},\ndto: {1},\nrole: {2}",
                    "dto matches role: dto: {1},\nrole: {2}",
                    IndexedSeq(nonMatching.mkString(", "), left, role))
      }
    }

  def matchEntityInfoDtoToRole(role: Role) =
    new Matcher[EntityInfoDto] {

      def apply(left: EntityInfoDto) = {
        val matchers = Map(("id" -> (left.id equals role.id.id)),
                           ("slug" -> (left.slug equals role.slug)),
                           ("name" -> (left.name equals role.name)))

        val nonMatching = matchers filter { case (k, v) => !v } keys

        MatchResult(
          nonMatching.size <= 0,
          "entity info dto does not match role for the following attributes: {0},\ndto: {1},\nrole: {2}",
          "entity info dto matches role: dto: {1},\nrole: {2}",
          IndexedSeq(nonMatching.mkString(", "), left, role)
        )
      }
    }

  def matchDtoToAccessItem(role: AccessItem) =
    new Matcher[AccessItemNameDto] {

      def apply(left: AccessItemNameDto) = {
        val matchers = Map(("id" -> (left.id equals role.id.id)),
                           ("slug"           -> (left.slug equals role.slug)),
                           ("name"           -> (left.name equals role.name)),
                           ("accessItemType" -> (left.accessItemType equals role.accessItemType.id)))

        val nonMatching = matchers filter { case (k, v) => !v } keys

        MatchResult(nonMatching.size <= 0,
                    "dto does not match role for the following attributes: {0},\ndto: {1},\nrole: {2}",
                    "dto matches role: dto: {1},\nrole: {2}",
                    IndexedSeq(nonMatching.mkString(", "), left, role))
      }
    }

  def matchEntityInfoDtoToMembership(role: Membership) =
    new Matcher[EntityInfoDto] {

      def apply(left: EntityInfoDto) = {
        val matchers = Map(("id" -> (left.id equals role.id.id)),
                           ("slug" -> (left.slug equals role.slug)),
                           ("name" -> (left.name equals role.name)))

        val nonMatching = matchers filter { case (k, v) => !v } keys

        MatchResult(nonMatching.size <= 0,
                    "dto does not match role for the following attributes: {0},\ndto: {1},\nrole: {2}",
                    "dto matches role: dto: {1},\nrole: {2}",
                    IndexedSeq(nonMatching.mkString(", "), left, role))
      }
    }

  def matchDtoToUserRole(role: Role) =
    new Matcher[UserRoleDto] {

      def apply(left: UserRoleDto) = {
        val matchers = Map(("id" -> (left.id equals role.id.id)),
                           ("version" -> (left.version equals role.version)),
                           ("slug"    -> (left.slug equals role.slug)),
                           ("name"    -> (left.name equals role.name)))

        val nonMatching = matchers filter { case (k, v) => !v } keys

        MatchResult(nonMatching.size <= 0,
                    "dto does not match role for the following attributes: {0},\ndto: {1},\nrole: {2}",
                    "dto matches role: dto: {1},\nrole: {2}",
                    IndexedSeq(nonMatching.mkString(", "), left, role))
      }
    }

  def matchDtoToMembership(membership: Membership) =
    new Matcher[MembershipDto] {

      def apply(left: MembershipDto) = {
        val timeAddedMatcher =
          beTimeWithinSeconds(membership.timeAdded, 5L)(OffsetDateTime.parse(left.timeAdded))

        val timeModifiedMatcher = beOptionalTimeWithinSeconds(membership.timeModified, 5L)
          .apply(left.timeModified.map(OffsetDateTime.parse))

        val dtoUserIds = left.userData
          .map { ud =>
            UserId(ud.id)
          }
          .toList
          .sortBy(_.id)
        val studyEntitySetMatcher  = matchDtoToEntitySetDto(membership.studyData)(left.studyData)
        val centreEntitySetMatcher = matchDtoToEntitySetDto(membership.centreData)(left.centreData)

        val matchers = Map(("id" -> (left.id equals membership.id.id)),
                           ("version"      -> (left.version equals membership.version)),
                           ("timeAdded"    -> timeAddedMatcher.matches),
                           ("timeModified" -> (timeModifiedMatcher.matches)),
                           ("slug"         -> (left.slug equals membership.slug)),
                           ("name"         -> (left.name equals membership.name)),
                           ("description"  -> (left.description equals membership.description)),
                           ("userIds"      -> (dtoUserIds equals membership.userIds.toList.sortBy(_.id))),
                           ("studyData"    -> (studyEntitySetMatcher.matches)),
                           ("centreData"   -> (centreEntitySetMatcher.matches)))

        val nonMatching = matchers filter { case (k, v) => !v } keys

        MatchResult(
          nonMatching.size <= 0,
          "dto does not match membership for the following attributes: {0},\ndto: {1},\nmembership: {2}",
          "dto matches membership: dto: {1},\nmembership: {2}",
          IndexedSeq(nonMatching.mkString(", "), left, membership)
        )
      }
    }

  def matchDtoToUserMembership(membership: UserMembership) =
    new Matcher[UserMembershipDto] {

      def apply(left: UserMembershipDto) = {
        val timeAddedMatcher =
          beTimeWithinSeconds(membership.timeAdded, 5L)(OffsetDateTime.parse(left.timeAdded))

        val timeModifiedMatcher = beOptionalTimeWithinSeconds(membership.timeModified, 5L)
          .apply(left.timeModified.map(OffsetDateTime.parse))

        val studyEntitySetMatcher  = matchDtoToEntitySetDto(membership.studyData)(left.studyData)
        val centreEntitySetMatcher = matchDtoToEntitySetDto(membership.centreData)(left.centreData)

        val matchers = Map(("id" -> (left.id equals membership.id.id)),
                           ("version"      -> (left.version equals membership.version)),
                           ("timeAdded"    -> timeAddedMatcher.matches),
                           ("timeModified" -> (timeModifiedMatcher.matches)),
                           ("slug"         -> (left.slug equals membership.slug)),
                           ("name"         -> (left.name equals membership.name)),
                           ("description"  -> (left.description equals membership.description)),
                           ("studyData"    -> (studyEntitySetMatcher.matches)),
                           ("centreData"   -> (centreEntitySetMatcher.matches)))

        val nonMatching = matchers filter { case (k, v) => !v } keys

        MatchResult(nonMatching.size <= 0,
                    "dto does not match membership for the following attributes: {0},\ndto: {1},\nrole: {2}",
                    "dto matches membership: dto: {1},\nrole: {2}",
                    IndexedSeq(nonMatching.mkString(", "), left, membership))
      }
    }

  def matchDtoToParticipant(participant: Participant) =
    new Matcher[ParticipantDto] {

      def apply(left: ParticipantDto) = {
        val timeAddedMatcher =
          beTimeWithinSeconds(participant.timeAdded, 5L)(OffsetDateTime.parse(left.timeAdded))

        val timeModifiedMatcher = beOptionalTimeWithinSeconds(participant.timeModified, 5L)
          .apply(left.timeModified.map(OffsetDateTime.parse))

        val matchers = Map(("id" -> (left.id equals participant.id.id)),
                           ("version"      -> (left.version equals participant.version)),
                           ("timeAdded"    -> timeAddedMatcher.matches),
                           ("timeModified" -> (timeModifiedMatcher.matches)),
                           ("studyId"      -> (left.study.id equals participant.studyId.id)),
                           ("slug"         -> (left.slug equals participant.slug)),
                           ("uniqueId"     -> (left.uniqueId equals participant.uniqueId)),
                           ("annotations"  -> (left.annotations equals participant.annotations)))

        val nonMatching = matchers filter { case (k, v) => !v } keys

        MatchResult(nonMatching.size <= 0,
                    "dto does not match participant for the following attributes: {0},\ndto: {1},\nrole: {2}",
                    "dto matches participant: dto: {1},\nrole: {2}",
                    IndexedSeq(nonMatching.mkString(", "), left, participant))
      }
    }

  def matchDtoToEntitySetDto(entitySet: MembershipEntitySet[_]) =
    new Matcher[EntitySetDto] {

      def apply(left: EntitySetDto) = {
        val dtoEntityIds = left.entityData.map { ed =>
          ed.id
        }
        val entityIds = entitySet.ids.map { id =>
          id.toString
        }

        val matchers = Map(("allEntities" -> (left.allEntities equals entitySet.allEntities)),
                           ("ids" -> (dtoEntityIds equals entityIds)))

        val nonMatching = matchers filter { case (k, v) => !v } keys

        MatchResult(nonMatching.size <= 0,
                    "dto does not match entitySet for the following attributes: {0},\ndto: {1},\nrole: {2}",
                    "dto matches entitySet: dto: {1},\nrole: {2}",
                    IndexedSeq(nonMatching.mkString(", "), left, entitySet))
      }
    }

  def matchEntityInfoAndStateDtos(dtos: Seq[EntityInfoAndStateDto]) =
    new Matcher[JsValue] {

      def apply(left: JsValue) = {
        val replyDtos = (left).validate[Seq[EntityInfoAndStateDto]]
        val validJs   = jsSuccess(replyDtos)

        if (!validJs.matches) {
          validJs
        } else {
          val m: Matcher[Seq[EntityInfoAndStateDto]] = equal(dtos)
          m(replyDtos.get)
        }
      }
    }

  def matchCentreLocationInfo(centreId: CentreId, locationId: LocationId) =
    new Matcher[CentreLocationInfo] {

      def apply(left: CentreLocationInfo) = {
        val centreIdsMatch = left.centreId equals centreId.id
        if (!centreIdsMatch) {
          MatchResult(false, s"centreIds do not match: expected: $centreId, actual ${left.centreId}", "")
        } else {
          MatchResult(left.locationId equals locationId.id,
                      s"locationIds do not match: expected: $locationId, actual ${left.locationId}",
                      s"locationIds match: expected: $locationId, actual ${left.locationId}")
        }
      }
    }

  def matchDtoToContainerSchema(schema: ContainerSchema) =
    new Matcher[ContainerSchemaDto] {

      def apply(left: ContainerSchemaDto) = {
        val timeAddedMatcher =
          beTimeWithinSeconds(schema.timeAdded, 5L)(OffsetDateTime.parse(left.timeAdded))

        val timeModifiedMatcher = beOptionalTimeWithinSeconds(schema.timeModified, 5L)
          .apply(left.timeModified.map(OffsetDateTime.parse))

        val matchers =
          Map(("id"           -> (left.id equals schema.id.id)),
              ("version"      -> (left.version equals schema.version)),
              ("timeAdded"    -> (timeAddedMatcher.matches)),
              ("timeModified" -> (timeModifiedMatcher.matches)),
              ("slug"         -> (left.slug equals schema.slug)),
              ("name"         -> (left.name equals schema.name)),
              ("description"  -> (left.description equals schema.description)),
              ("shared"       -> (left.shared equals schema.shared)),
              ("centreId"     -> (left.centre.id equals schema.centreId.id)))

        val nonMatching = matchers filter { case (k, v) => !v } keys

        MatchResult(nonMatching.size <= 0,
                    "dto does not match entity for the following attributes: {0},\ndto: {1},\nentity: {2}",
                    "dto matches entity: dto: {1},\nentity: {2}",
                    IndexedSeq(nonMatching.mkString(", "), left, schema))
      }
    }

  def matchDtoToContainerType(containerType: ContainerType) =
    new Matcher[ContainerTypeDto] {

      def apply(left: ContainerTypeDto) = {
        val timeAddedMatcher =
          beTimeWithinSeconds(containerType.timeAdded, 5L)(OffsetDateTime.parse(left.timeAdded))

        val timeModifiedMatcher = beOptionalTimeWithinSeconds(containerType.timeModified, 5L)
          .apply(left.timeModified.map(OffsetDateTime.parse))

        val matchers =
          Map(("id"           -> (left.id equals containerType.id.id)),
              ("version"      -> (left.version equals containerType.version)),
              ("timeAdded"    -> (timeAddedMatcher.matches)),
              ("timeModified" -> (timeModifiedMatcher.matches)),
              ("slug"         -> (left.slug equals containerType.slug)),
              ("name"         -> (left.name equals containerType.name)),
              ("description"  -> (left.description equals containerType.description)),
              ("centreId"     -> (left.centre.id equals containerType.centreId.id)),
              ("schemaId"     -> (left.schema.id equals containerType.schemaId.id)),
              ("shared"       -> (left.shared equals containerType.shared)),
              ("enabled"      -> (left.enabled equals containerType.enabled)),
              ("storageType"  -> (left.storageType equals containerType.storageType.id)))

        val nonMatching = matchers filter { case (k, v) => !v } keys

        MatchResult(nonMatching.size <= 0,
                    "dto does not match entity for the following attributes: {0},\ndto: {1},\nentity: {2}",
                    "dto matches entity: dto: {1},\nentity: {2}",
                    IndexedSeq(nonMatching.mkString(", "), left, containerType))
      }
    }

  def matchDtoToContainer(container: Container) =
    new Matcher[ContainerDto] {

      def apply(left: ContainerDto) =
        (left, container) match {
          case (l: RootContainerDto, c:     RootContainer)     => matchDtoToRootContainer(c).apply(l)
          case (l: StorageContainerDto, c:  StorageContainer)  => matchDtoToStorageContainer(c).apply(l)
          case (l: SpecimenContainerDto, c: SpecimenContainer) => matchDtoToSpecimenContainer(c).apply(l)
          case _ =>
            MatchResult(
              false,
              s"dto and container do not have maching scala types: ${left.getClass.getSimpleName}, ${container.getClass.getSimpleName}",
              ""
            )
        }
    }

  def matchDtoToRootContainer(container: RootContainer) =
    new Matcher[RootContainerDto] {

      def apply(left: RootContainerDto) = {
        val constraintsMatcher = matchContainerConstraints(container.constraints).apply(left.constraints)
        val matchers =
          Map(("label"              -> (left.label equals container.label)),
              ("enabled"            -> (left.enabled equals container.enabled)),
              ("centreLocationInfo" -> (left.centreLocationInfo.locationId equals container.locationId.id)),
              ("temperature"        -> (left.temperature equals container.temperature)),
              ("constraints"        -> (constraintsMatcher.matches))) ++
            dtoAndContainerMatches(left, container)

        val nonMatching = matchers filter { case (k, v) => !v } keys

        MatchResult(nonMatching.size <= 0,
                    "dto does not match entity for the following attributes: {0},\ndto: {1},\nentity: {2}",
                    "dto matches entity: dto: {1},\nentity: {2}",
                    IndexedSeq(nonMatching.mkString(", "), left, container))
      }
    }

  def matchDtoToStorageContainer(container: StorageContainer) =
    new Matcher[StorageContainerDto] {

      def apply(left: StorageContainerDto) = {
        val constraintsMatcher = matchContainerConstraints(container.constraints).apply(left.constraints)
        val matchers =
          Map(("enabled"     -> (left.enabled equals container.enabled)),
              ("constraints" -> (constraintsMatcher.matches)),
              ("parentId"    -> (left.parent.id equals container.parentId.id)),
              ("label"       -> (left.label equals container.schemaLabel.label))) ++
            dtoAndContainerMatches(left, container)

        val nonMatching = matchers filter { case (k, v) => !v } keys

        MatchResult(nonMatching.size <= 0,
                    "dto does not match entity for the following attributes: {0},\ndto: {1},\nentity: {2}",
                    "dto matches entity: dto: {1},\nentity: {2}",
                    IndexedSeq(nonMatching.mkString(", "), left, container))
      }
    }

  def matchDtoToSpecimenContainer(container: SpecimenContainer) =
    new Matcher[SpecimenContainerDto] {

      def apply(left: SpecimenContainerDto) = {
        val nonMatching =
          Map(("parentId" -> (left.parent.id equals container.parentId.id)),
              ("label"    -> (left.label equals container.schemaLabel.label))) ++
            dtoAndContainerMatches(left, container) filter { case (k, v) => !v } keys

        MatchResult(nonMatching.size <= 0,
                    "dto does not match entity for the following attributes: {0},\ndto: {1},\nentity: {2}",
                    "dto matches entity: dto: {1},\nentity: {2}",
                    IndexedSeq(nonMatching.mkString(", "), left, container))
      }
    }

  def dtoAndContainerMatches(left: ContainerDto, container: Container) = {
    val timeAddedMatcher =
      beTimeWithinSeconds(container.timeAdded, 5L)(OffsetDateTime.parse(left.timeAdded))

    val timeModifiedMatcher = beOptionalTimeWithinSeconds(container.timeModified, 5L)
      .apply(left.timeModified.map(OffsetDateTime.parse))

    Map(("id"              -> (left.id equals container.id.id)),
        ("version"         -> (left.version equals container.version)),
        ("timeAdded"       -> (timeAddedMatcher.matches)),
        ("timeModified"    -> (timeModifiedMatcher.matches)),
        ("slug"            -> (left.slug equals container.slug)),
        ("inventoryId"     -> (left.inventoryId equals container.inventoryId)),
        ("containerTypeId" -> (left.containerType.id equals container.containerTypeId.id)))
  }

  def matchContainerConstraints(constraints: ContainerConstraints) =
    new Matcher[ContainerConstraintsDto] {

      def apply(left: ContainerConstraintsDto) = {
        val matchers =
          Map(("name"        -> (left.name equals constraints.name)),
              ("description" -> (left.description equals constraints.description)),
              ("anatomicalSources" -> (left.anatomicalSources.map(_.toString).toSeq.sorted
                equals constraints.anatomicalSources.map(_.toString).toSeq.sorted)),
              ("preservationTypes" -> (left.preservationTypes.map(_.toString).toSeq.sorted
                equals constraints.preservationTypes.map(_.toString).toSeq.sorted)),
              ("specimenTypes" -> (left.specimenTypes.map(_.toString).toSeq.sorted
                equals constraints.specimenTypes.map(_.toString).toSeq.sorted)))

        val nonMatching = matchers filter { case (k, v) => !v } keys

        MatchResult(nonMatching.size <= 0,
                    "dto does not match entity for the following attributes: {0},\ndto: {1},\nentity: {2}",
                    "dto matches entity: dto: {1},\nentity: {2}",
                    IndexedSeq(nonMatching.mkString(", "), left, constraints))
      }
    }

  def matchDtoToContainerInfo(container: Container) =
    new Matcher[ContainerInfo] {

      def apply(left: ContainerInfo) = {
        val matchers =
          Map(("id"          -> (left.id equals container.id.id)),
              ("slug"        -> (left.slug equals container.slug.id)),
              ("inventoryId" -> (left.inventoryId equals container.inventoryId)),
              ("label"       -> (left.label equals container.getLabel)))

        val nonMatching = matchers filter { case (k, v) => !v } keys

        MatchResult(nonMatching.size <= 0,
                    "dto does not match entity for the following attributes: {0},\ndto: {1},\nentity: {2}",
                    "dto matches entity: dto: {1},\nentity: {2}",
                    IndexedSeq(nonMatching.mkString(", "), left, container))
      }
    }

  private def matchContainerConstraints(
      constraints: Option[ContainerConstraints]
    ): Matcher[Option[ContainerConstraintsDto]] =
    new Matcher[Option[ContainerConstraintsDto]] {

      def apply(left: Option[ContainerConstraintsDto]) = {
        (left, constraints) match {
          case (Some(l), Some(c)) => matchContainerConstraints(c).apply(l)
          case (None, Some(c)) =>
            MatchResult(false, "dto does not have constraints when container does", "", "")
          case (Some(c), None) => MatchResult(false, "dto has constraints when container does not", "", "")
          case (None, None)    => MatchResult(true, "", "", "")
        }
      }
    }

  private def optionalTimeWithinSeconds(
      expected: Option[String],
      actual:   Option[OffsetDateTime],
      seconds:  Long
    ) =
    beOptionalTimeWithinSeconds(actual, seconds).apply(expected.map(OffsetDateTime.parse))

}

object DtoMatchers extends DtoMatchers
