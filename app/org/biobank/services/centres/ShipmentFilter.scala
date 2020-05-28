package org.biobank.services.centres

import cats.data._
import cats.implicits._
import com.google.inject.ImplementedBy
import javax.inject.Inject
import org.biobank.services._
import org.biobank.services.Comparator._
import org.biobank.domain.{EntityState, PredicateHelper}
import org.biobank.domain.centres._
import org.biobank.dto.centres._
import org.biobank.query.centres.ShipmentsReadRepository
import org.biobank.validation.Validation._
import org.slf4j.{Logger, LoggerFactory}

@ImplementedBy(classOf[ShipmentFilterImpl])
trait ShipmentFilter {

  def filterShipments(shipments: Set[ShipmentDto], filter: FilterString): ValidationResult[Set[ShipmentDto]]

}

trait ShipmentPredicates {
  type ShipmentFilter = ShipmentDto => Boolean

  type StringsToShipmentFilter = Set[String] => ShipmentFilter

  val originNameIsOneOf: Set[String] => ShipmentFilter =
    centreIds => shipment => centreIds.contains(shipment.origin.name)

  val destinationNameIsOneOf: Set[String] => ShipmentFilter =
    centreIds => shipment => centreIds.contains(shipment.destination.name)

  val withCentreNameIsOneOf: Set[String] => ShipmentFilter =
    centreIds =>
      shipment => centreIds.contains(shipment.destination.name) || centreIds.contains(shipment.origin.name)

  val courierNameIsOneOf: Set[String] => ShipmentFilter =
    courierNames => shipment => courierNames.contains(shipment.courierName)

  val trackingNumberIsOneOf: Set[String] => ShipmentFilter =
    trackingNumbers => shipment => trackingNumbers.contains(shipment.trackingNumber)

  val stateIsOneOf: Set[EntityState] => ShipmentFilter =
    states => shipment => states.contains(shipment.state)

  val courierNameIsLike: Set[String] => ShipmentFilter =
    courierNames =>
      shipment => {
        val lc = shipment.courierName.toLowerCase
        courierNames.forall(n => lc.contains(n.toLowerCase))
      }

  val trackingNumberIsLike: Set[String] => ShipmentFilter =
    trackingNumbers =>
      shipment => {
        val lc = shipment.trackingNumber.toLowerCase
        trackingNumbers.forall(n => lc.contains(n.toLowerCase))
      }

}

/**
 * Functions that filter a set of shipments from an expression contained in a filter string.
 *
 */
class ShipmentFilterImpl @Inject()(
    val shipmentRepository: ShipmentsReadRepository,
    val centreRepository:   CentreRepository)
    extends ShipmentFilter with EntityFilterCats[ShipmentDto] with PredicateHelper with ShipmentPredicates {

  val log: Logger = LoggerFactory.getLogger(this.getClass)

  def filterShipments(shipments: Set[ShipmentDto], filter: FilterString): ValidationResult[Set[ShipmentDto]] =
    filterEntitiesCats(shipments, filter, shipments.filter)

  protected def predicateFromSelector(
      selector:   String,
      comparator: Comparator,
      args:       List[String]
    ): ServiceValidation[ShipmentDto => Boolean] = ???

  protected def predicateFromSelectorCats(
      selector:   String,
      comparator: Comparator,
      args:       List[String]
    ): ValidationResult[ShipmentDto => Boolean] =
    selector match {
      case "origin"         => originCentreFilter(comparator, args)
      case "destination"    => destinationCentreFilter(comparator, args)
      case "withCentre"     => withCentreFilter(comparator, args)
      case "courierName"    => courierNameFilter(comparator, args)
      case "trackingNumber" => trackingNumberFilter(comparator, args)
      case "state"          => stateFilter(comparator, args)
      case _                => Error(s"invalid filter selector: $selector").invalidNec
    }

  private def originCentreFilter(comparator: Comparator, names: List[String]) = {
    centreNamesFilter(comparator,
                      originNameIsOneOf,
                      names,
                      Error(s"invalid filter on 'from centre' name: $comparator"))
  }

  private def destinationCentreFilter(comparator: Comparator, names: List[String]) =
    centreNamesFilter(comparator,
                      destinationNameIsOneOf,
                      names,
                      Error(s"invalid filter on 'to centre' name: $comparator"))

  private def withCentreFilter(comparator: Comparator, names: List[String]) =
    centreNamesFilter(comparator,
                      withCentreNameIsOneOf,
                      names,
                      Error(s"invalid filter on 'with centre' name: $comparator"))

  private def centreNamesFilter(
      comparator:     Comparator,
      shipmentFilter: Set[String] => ShipmentFilter,
      names:          List[String],
      error:          ValidationError
    ) = {
    val centreNamesSet = names.toSet
    comparator match {
      case Equal | In         => shipmentFilter(centreNamesSet).validNec
      case NotEqualTo | NotIn => complement(shipmentFilter(centreNamesSet)).validNec
      case _                  => error.invalidNec
    }
  }

  private def courierNameFilter(
      comparator: Comparator,
      names:      List[String]
    ): ValidationResult[ShipmentDto => Boolean] = {
    val nameSet = names.toSet
    comparator match {
      case Equal | In         => courierNameIsOneOf(nameSet).validNec
      case NotEqualTo | NotIn => complement(courierNameIsOneOf(nameSet)).validNec
      case Like               => courierNameIsLike(nameSet).validNec
      case _                  => Error(s"invalid filter on courier name: $comparator").invalidNec
    }
  }

  private def trackingNumberFilter(
      comparator:      Comparator,
      trackingNumbers: List[String]
    ): ValidationResult[ShipmentDto => Boolean] = {
    val trackingNumberSet = trackingNumbers.toSet
    comparator match {
      case Equal | In         => trackingNumberIsOneOf(trackingNumberSet).validNec
      case NotEqualTo | NotIn => complement(trackingNumberIsOneOf(trackingNumberSet)).validNec
      case Like               => trackingNumberIsLike(trackingNumberSet).validNec
      case _                  => Error(s"invalid filter on tracking number: $comparator").invalidNec
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def stateFilter(
      comparator: Comparator,
      stateNames: List[String]
    ): ValidationResult[ShipmentDto => Boolean] = {
    stateNames
      .map { str =>
        Validated
          .fromOption(Shipment.shipmentStates.find(_.id == str),
                      EntityCriteriaError(s"shipment state does not exist: $str")).toValidatedNec
      }.toList.sequence
      .andThen { states =>
        val stateSet = states.toSet

        comparator match {
          case Equal | In         => stateIsOneOf(stateSet).validNec
          case NotEqualTo | NotIn => complement(stateIsOneOf(stateSet)).validNec
          case _                  => Error(s"invalid filter on state: $comparator").invalidNec
        }
      }
  }
}
