package org.biobank.services.centres

import cats.implicits._
import org.biobank.services._
import org.biobank.services.Comparator._
import org.biobank.domain.PredicateHelper
import org.biobank.domain.centres.ShipmentSpecimen
import org.biobank.dto.centres.ShipmentSpecimenDto
import org.biobank.validation.Validation._
import org.slf4j.{Logger, LoggerFactory}

/**
 * Functions that filter a set of shipment specimens from an expression contained in a filter string.
 *
 */
object ShipmentSpecimenFilter
    extends EntityFilterCats[ShipmentSpecimenDto] with EntityStateFilter[ShipmentSpecimenDto]
    with PredicateHelper {

  val log: Logger = LoggerFactory.getLogger(this.getClass)

  def filterShipmentSpecimens(
      shipmentSpecimens: Set[ShipmentSpecimenDto],
      filter:            FilterString
    ): ValidationResult[Set[ShipmentSpecimenDto]] = {
    filterEntitiesCats(shipmentSpecimens, filter, shipmentSpecimens.filter)
  }

  protected def predicateFromSelector(
      selector:   String,
      comparator: Comparator,
      args:       List[String]
    ): ServiceValidation[ShipmentSpecimenDto => Boolean] = ???

  protected def predicateFromSelectorCats(
      selector:   String,
      comparator: Comparator,
      args:       List[String]
    ): ValidationResult[ShipmentSpecimenDto => Boolean] =
    selector match {
      case "state" => stateFilter(comparator, args)
      case _ =>
        Error(s"invalid filter selector: $selector").invalidNec
    }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  private def stateFilter(
      comparator: Comparator,
      stateNames: List[String]
    ): ValidationResult[ShipmentSpecimenDto => Boolean] =
    stateFilterCats(comparator, stateNames, ShipmentSpecimen.states)
}
