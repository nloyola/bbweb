package org.biobank.dto

import org.biobank.domain.{Location, LocationId, Slug}
import play.api.libs.json._

final case class LocationInfoDto(id: LocationId, slug: Slug, name: String) extends NamedEntityInfo[LocationId]

object LocationInfoDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(location: Location): LocationInfoDto =
    LocationInfoDto(location.id, location.slug, location.name)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val locationInfoDtoFormat: Format[LocationInfoDto] = Json.format[LocationInfoDto]

}
