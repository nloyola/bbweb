package org.biobank.dto

import org.biobank.domain.Slug
import play.api.libs.json._

package access {

  final case class AccessItemNameDto(id: String, slug: Slug, name: String, accessItemType: String) extends Dto

  object AccessItemNameDto {

    def compareByName(a: AccessItemNameDto, b: AccessItemNameDto): Boolean =
      (a.name compareToIgnoreCase b.name) < 0

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val accessItemNameDtoFormat: Format[AccessItemNameDto] = Json.format[AccessItemNameDto]
  }

  final case class RoleDto(
      id:             String,
      version:        Long,
      timeAdded:      String,
      timeModified:   Option[String],
      accessItemType: String,
      slug:           Slug,
      name:           String,
      description:    Option[String],
      userData:       Set[NamedEntityInfoDto],
      parentData:     Set[NamedEntityInfoDto],
      childData:      Set[NamedEntityInfoDto])
      extends Dto {

    override def toString: String =
      s"""|${this.getClass.getSimpleName}: {
          |  id:             $id,
          |  version:        $version,
          |  timeAdded:      $timeAdded,
          |  timeModified:   $timeModified,
          |  accessItemType: $accessItemType,
          |  slug:           $slug,
          |  name:           $name,
          |  description:    $description,
          |  userData:       $userData,
          |  parentData:     $parentData,
          |  childData:      $childData
          |}""".stripMargin
  }

  object RoleDto {

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val roleDtoFormat: Format[RoleDto] = Json.format[RoleDto]

  }

  final case class UserRoleDto(
      id:        String,
      version:   Long,
      slug:      Slug,
      name:      String,
      childData: Set[NamedEntityInfoDto])
      extends Dto

  object UserRoleDto {

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val userRoleDtoFormat: Format[UserRoleDto] = Json.format[UserRoleDto]

  }

  final case class MembershipDto(
      id:           String,
      version:      Long,
      timeAdded:    String,
      timeModified: Option[String],
      slug:         Slug,
      name:         String,
      description:  Option[String],
      userData:     Set[NamedEntityInfoDto],
      studyData:    EntitySetDto,
      centreData:   EntitySetDto)
      extends Dto {
    override def toString: String =
      s"""|${this.getClass.getSimpleName}: {
          |  id:             $id,
          |  version:        $version,
          |  timeAdded:      $timeAdded,
          |  timeModified:   $timeModified,
          |  slug:           $slug,
          |  name:           $name,
          |  description:    $description,
          |  userData:       $userData,
          |  studyData:      $studyData,
          |  centreData:     $centreData
          |}""".stripMargin

  }

  object MembershipDto {

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val membershipDtoFormat: Format[MembershipDto] = Json.format[MembershipDto]

  }

  final case class UserMembershipDto(
      id:           String,
      version:      Long,
      timeAdded:    String,
      timeModified: Option[String],
      slug:         Slug,
      name:         String,
      description:  Option[String],
      studyData:    EntitySetDto,
      centreData:   EntitySetDto)
      extends Dto

  object UserMembershipDto {

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit val userMembershipDtoFormat: Format[UserMembershipDto] = Json.format[UserMembershipDto]

  }

}
