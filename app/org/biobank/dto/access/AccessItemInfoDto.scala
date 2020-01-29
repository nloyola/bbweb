package org.biobank.dto.access

import org.biobank.domain.Slug
import org.biobank.domain.access.{AccessItem, AccessItemId, AccessItemType}
import org.biobank.dto.NamedEntityInfo
import play.api.libs.json._

final case class AccessItemInfoDto(id: AccessItemId, slug: Slug, name: String, accessItemType: AccessItemType)
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
