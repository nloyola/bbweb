package org.biobank.domain.containers

import org.biobank.domain.{DomainValidation, LocationId}
import org.biobank.domain.PreservationTemperature._
import org.biobank.domain.centres.CentreId
import play.api.libs.json._
import scalaz.Scalaz._
import scalaz.NonEmptyList._

/**
 * Only used by the root node in a [[Container]] tree, it has properties for all the Containers in the tree.
 * For example [[domain.PreservationTemperature]], [[domain.centres.Centre Centre]], and [[domain.Location
 * Location]].
 */
final case class ContainerSharedProperties(centreId:     CentreId,
                                           locationId:   LocationId,
                                           temperature:  PreservationTemperature) {
}

object ContainerSharedProperties extends ContainerValidations {
  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  def create(centreId:     CentreId,
             locationId:   LocationId,
             temperature:  PreservationTemperature): DomainValidation[ContainerSharedProperties] = {
    validate(centreId, locationId, temperature).map { _ =>
      ContainerSharedProperties(centreId, locationId, temperature)
    }
  }

  def validate(centreId:     CentreId,
               locationId:   LocationId,
               temperature:  PreservationTemperature): DomainValidation[Unit] = {
    (validateId(centreId, CentreIdInvalid) |@|
       (validateId(locationId, LocationIdInvalid))) { case _ => () }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def validate(sharedProperties: ContainerSharedProperties): DomainValidation[Unit] = {
    validate(sharedProperties.centreId, sharedProperties.locationId, sharedProperties.temperature)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def validate(sharedProperties: Option[ContainerSharedProperties]): DomainValidation[Unit] = {
    sharedProperties match {
      case Some(sp) => validate(sp.centreId, sp.locationId, sp.temperature)
      case None => ().successNel[String]
    }
  }

  implicit val containerSharedPropertiesFormat: Format[ContainerSharedProperties] =
    Json.format[ContainerSharedProperties]

}
