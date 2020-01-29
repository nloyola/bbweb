package org.biobank.dto.access

import java.time.OffsetDateTime
import org.biobank.domain.{HasName, HasOptionalDescription, Slug}
import org.biobank.domain.access._
import org.biobank.dto.{EntityDto, NamedEntityInfo}
import org.biobank.dto.centres.CentreSetDto
import org.biobank.dto.studies.StudySetDto
import org.biobank.dto.users.UserInfoDto
import play.api.libs.json._

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
    extends EntityDto[MembershipId] with HasName with HasOptionalDescription {

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
    extends EntityDto[MembershipId] with HasName with HasOptionalDescription

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
