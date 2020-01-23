package org.biobank.dto

import java.time.OffsetDateTime
import org.biobank.domain.Slug
import org.biobank.domain.access._
import play.api.libs.json._

package access {

  final case class AccessItemInfoDto(
      id:             AccessItemId,
      slug:           Slug,
      name:           String,
      accessItemType: AccessItemType)
      extends NamedEntityInfo[AccessItemId]

  object AccessItemInfoDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(item: AccessItem): AccessItemInfoDto =
      AccessItemInfoDto(item.id, item.slug, item.name, item.accessItemType)

    def compareByName(a: AccessItemInfoDto, b: AccessItemInfoDto): Boolean =
      (a.name compareToIgnoreCase b.name) < 0

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val accessItemInfoDtoFormat: Format[AccessItemInfoDto] = Json.format[AccessItemInfoDto]

  }

  final case class RoleDto(
      id:             AccessItemId,
      version:        Long,
      timeAdded:      OffsetDateTime,
      timeModified:   Option[OffsetDateTime],
      accessItemType: String,
      slug:           Slug,
      name:           String,
      description:    Option[String],
      userData:       Set[UserInfoDto],
      parentData:     Set[AccessItemInfoDto],
      childData:      Set[AccessItemInfoDto])
      extends Dto {

    override def toString: String = s"|${this.getClass.getSimpleName}: ${Json.prettyPrint(Json.toJson(this))}"
  }

  object RoleDto {

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val roleDtoFormat: Format[RoleDto] = Json.format[RoleDto]

  }

  final case class UserRoleDto(
      id:        AccessItemId,
      version:   Long,
      slug:      Slug,
      name:      String,
      childData: Set[AccessItemInfoDto])
      extends Dto

  object UserRoleDto {

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val userRoleDtoFormat: Format[UserRoleDto] = Json.format[UserRoleDto]

  }

  final case class MembershipDto(
      id:           MembershipId,
      version:      Long,
      timeAdded:    OffsetDateTime,
      timeModified: Option[OffsetDateTime],
      slug:         Slug,
      name:         String,
      description:  Option[String],
      userData:     Set[UserInfoDto],
      studyData:    StudySetDto,
      centreData:   CentreSetDto)
      extends Dto {

    override def toString: String = s"|${this.getClass.getSimpleName}: ${Json.prettyPrint(Json.toJson(this))}"

  }

  object MembershipDto {

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val membershipDtoFormat: Format[MembershipDto] = Json.format[MembershipDto]

  }

  final case class UserMembershipDto(
      id:           MembershipId,
      version:      Long,
      timeAdded:    OffsetDateTime,
      timeModified: Option[OffsetDateTime],
      slug:         Slug,
      name:         String,
      description:  Option[String],
      studyData:    StudySetDto,
      centreData:   CentreSetDto)
      extends Dto

  object UserMembershipDto {

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val userMembershipDtoFormat: Format[UserMembershipDto] = Json.format[UserMembershipDto]

  }

  final case class MembershipInfoDto(id: MembershipId, slug: Slug, name: String)
      extends NamedEntityInfo[MembershipId]

  object MembershipInfoDto {

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def apply(membership: Membership): MembershipInfoDto =
      MembershipInfoDto(membership.id, membership.slug, membership.name)

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val membershipInfoDtoFormat: Format[MembershipInfoDto] = Json.format[MembershipInfoDto]

  }

}
