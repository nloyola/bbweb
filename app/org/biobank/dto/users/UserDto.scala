package org.biobank.dto.users

import java.time.OffsetDateTime
import org.biobank.domain.{EntityState, HasName, HasState, Slug}
import org.biobank.domain.users.{User, UserId}
import org.biobank.dto.{EntityDto, EntityInfoAndState, NamedEntityInfo}
import org.biobank.dto.access.{UserMembershipDto, UserRoleDto}
import play.api.libs.json._

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
    extends EntityDto[UserId] with HasState with HasName

object UserDto {

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val userDtoFormat: Format[UserDto] = Json.format[UserDto]

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
