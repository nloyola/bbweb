package org.biobank.dto.centres

import org.biobank.domain.{Location, Slug}
import org.biobank.domain.centres.{Centre, CentreId}
import org.biobank.dto.{LocationInfoDto, NamedEntityInfo}
import play.api.libs.json._

final case class CentreLocationInfo(
    id:           CentreId,
    slug:         Slug,
    name:         String,
    location:     LocationInfoDto,
    combinedName: String)
    extends NamedEntityInfo[CentreId]

object CentreLocationInfo {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(centre: Centre, location: Location): CentreLocationInfo =
    CentreLocationInfo(id           = centre.id,
                       slug         = centre.slug,
                       name         = centre.name,
                       location     = LocationInfoDto(location),
                       combinedName = s"${centre.name}: ${location.name}")

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val centreLocationInfoFormat: Format[CentreLocationInfo] = Json.format[CentreLocationInfo]

}
