package org.biobank.dto.access

import java.time.OffsetDateTime
import org.biobank.domain.{HasName, HasOptionalDescription, Slug}
import org.biobank.domain.access.AccessItemId
import org.biobank.dto.EntityDto
import org.biobank.dto.users.UserInfoDto
import play.api.libs.json._

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
    extends EntityDto[AccessItemId] with HasName with HasOptionalDescription {

  override def toString: String = s"|${this.getClass.getSimpleName}: ${Json.prettyPrint(Json.toJson(this))}"
}

object RoleDto {

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val roleDtoFormat: Format[RoleDto] = Json.format[RoleDto]

}

final case class UserRoleDto(
    id:           AccessItemId,
    version:      Long,
    timeAdded:    OffsetDateTime,
    timeModified: Option[OffsetDateTime],
    slug:         Slug,
    name:         String,
    description:  Option[String],
    childData:    Set[AccessItemInfoDto])
    extends EntityDto[AccessItemId] with HasName with HasOptionalDescription

object UserRoleDto {

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val userRoleDtoFormat: Format[UserRoleDto] = Json.format[UserRoleDto]

}
