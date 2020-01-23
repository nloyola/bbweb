package org.biobank.services.centres

import com.google.inject.ImplementedBy
import javax.inject.Inject
import org.biobank.services._
import org.biobank.services.Comparator._
import org.biobank.services.{ServiceError, ServiceValidation}
import org.biobank.domain.{EntityState, PredicateHelper}
import org.biobank.domain.centres._
import org.biobank.dto.centres._
import org.biobank.query.centres.ShipmentsReadRepository
import org.slf4j.{Logger, LoggerFactory}
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._

@ImplementedBy(classOf[ShipmentFilterImpl])
trait ShipmentFilter {

  def filterShipments(shipments: Set[ShipmentDto], filter: FilterString): ServiceValidation[Set[ShipmentDto]]

}

trait ShipmentPredicates {
  type ShipmentFilter = ShipmentDto => Boolean

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
    extends ShipmentFilter with EntityFilter[ShipmentDto] with PredicateHelper with ShipmentPredicates {
  import org.biobank.CommonValidations._

  val log: Logger = LoggerFactory.getLogger(this.getClass)

  def filterShipments(
      shipments: Set[ShipmentDto],
      filter:    FilterString
    ): ServiceValidation[Set[ShipmentDto]] =
    filterEntities(shipments, filter, shipments.filter)

  protected def predicateFromSelector(
      selector:   String,
      comparator: Comparator,
      args:       List[String]
    ): ServiceValidation[ShipmentDto => Boolean] =
    selector match {
      case "origin"         => originCentreFilter(comparator, args)
      case "destination"    => destinationCentreFilter(comparator, args)
      case "withCentre"     => withCentreFilter(comparator, args)
      case "courierName"    => courierNameFilter(comparator, args)
      case "trackingNumber" => trackingNumberFilter(comparator, args)
      case "state"          => stateFilter(comparator, args)
      case _ =>
        ServiceError(s"invalid filter selector: $selector").failureNel[ShipmentFilter]
    }

  private def originCentreFilter(comparator: Comparator, names: List[String]) =
    centreNamesFilter(comparator,
                      originNameIsOneOf,
                      names,
                      ServiceError(s"invalid filter on 'from centre' name: $comparator"))

  private def destinationCentreFilter(comparator: Comparator, names: List[String]) =
    centreNamesFilter(comparator,
                      destinationNameIsOneOf,
                      names,
                      ServiceError(s"invalid filter on 'to centre' name: $comparator"))

  private def withCentreFilter(comparator: Comparator, names: List[String]) =
    centreNamesFilter(comparator,
                      withCentreNameIsOneOf,
                      names,
                      ServiceError(s"invalid filter on 'with centre' name: $comparator"))

  private def centreNamesFilter(
      comparator:     Comparator,
      shipmentFilter: Set[String] => ShipmentFilter,
      names:          List[String],
      error:          ServiceError
    ) = {
    val centreNamesSet = names.toSet
    comparator match {
      case Equal | In =>
        shipmentFilter(centreNamesSet).successNel[String]
      case NotEqualTo | NotIn =>
        complement(shipmentFilter(centreNamesSet)).successNel[String]
      case _ =>
        error.failureNel[ShipmentFilter]
    }
  }

  private def courierNameFilter(comparator: Comparator, names: List[String]) = {
    val nameSet = names.toSet
    comparator match {
      case Equal | In =>
        courierNameIsOneOf(nameSet).successNel[String]
      case NotEqualTo | NotIn =>
        complement(courierNameIsOneOf(nameSet)).successNel[String]
      case Like =>
        courierNameIsLike(nameSet).successNel[String]
      case _ =>
        ServiceError(s"invalid filter on courier name: $comparator").failureNel[ShipmentFilter]
    }
  }

  private def trackingNumberFilter(comparator: Comparator, trackingNumbers: List[String]) = {
    val trackingNumberSet = trackingNumbers.toSet
    comparator match {
      case Equal | In =>
        trackingNumberIsOneOf(trackingNumberSet).successNel[String]
      case NotEqualTo | NotIn =>
        complement(trackingNumberIsOneOf(trackingNumberSet)).successNel[String]
      case Like =>
        trackingNumberIsLike(trackingNumberSet).successNel[String]
      case _ =>
        ServiceError(s"invalid filter on tracking number: $comparator").failureNel[ShipmentFilter]
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def stateFilter(comparator: Comparator, stateNames: List[String]) =
    stateNames
      .map { str =>
        Shipment.shipmentStates
          .find(_.id == str)
          .toSuccessNel(EntityCriteriaError(s"shipment state does not exist: $str").toString)
      }
      .toList.sequenceU
      .flatMap { states =>
        val stateSet = states.toSet

        comparator match {
          case Equal | In =>
            stateIsOneOf(stateSet).successNel[String]
          case NotEqualTo | NotIn =>
            complement(stateIsOneOf(stateSet)).successNel[String]
          case _ =>
            ServiceError(s"invalid filter on state: $comparator").failureNel[ShipmentFilter]
        }
      }
}
