package org.biobank.services.centres

import com.google.inject.ImplementedBy
import javax.inject.Inject
import org.biobank.services._
import org.biobank.services.Comparator._
import org.biobank.services.{ServiceError, ServiceValidation}
import org.biobank.domain.PredicateHelper
import org.biobank.domain.centres._
import org.slf4j.{Logger, LoggerFactory}
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._

@ImplementedBy(classOf[ShipmentFilterImpl])
trait ShipmentFilter {

  def filterShipments(shipments: Set[Shipment], filter: FilterString): ServiceValidation[Set[Shipment]]

}

/**
 * Functions that filter a set of shipments from an expression contained in a filter string.
 *
 */
class ShipmentFilterImpl @Inject()(
    val shipmentRepository: ShipmentsWriteRepository,
    val centreRepository:   CentreRepository)
    extends ShipmentFilter with EntityFilter[Shipment] with PredicateHelper with ShipmentPredicates {
  import org.biobank.CommonValidations._

  val log: Logger = LoggerFactory.getLogger(this.getClass)

  def filterShipments(shipments: Set[Shipment], filter: FilterString): ServiceValidation[Set[Shipment]] =
    filterEntities(shipments, filter, shipments.filter)

  protected def predicateFromSelector(
      selector:   String,
      comparator: Comparator,
      args:       List[String]
    ): ServiceValidation[Shipment => Boolean] =
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
    centreIdsFilter(comparator,
                    originCentreIdIsOneOf,
                    names,
                    ServiceError(s"invalid filter on 'from centre' name: $comparator"))

  private def destinationCentreFilter(comparator: Comparator, names: List[String]) =
    centreIdsFilter(comparator,
                    destinationCentreIdIsOneOf,
                    names,
                    ServiceError(s"invalid filter on 'to centre' name: $comparator"))

  private def withCentreFilter(comparator: Comparator, names: List[String]) =
    centreIdsFilter(comparator,
                    withCentreIdIsOneOf,
                    names,
                    ServiceError(s"invalid filter on 'with centre' name: $comparator"))

  private def centreIdsFilter(
      comparator:     Comparator,
      shipmentFilter: Set[CentreId] => ShipmentFilter,
      names:          List[String],
      error:          ServiceError
    ) = {
    val centreIds = centreRepository.getByNames(names.toSet).map(_.id)

    comparator match {
      case Equal | In =>
        shipmentFilter(centreIds).successNel[String]
      case NotEqualTo | NotIn =>
        complement(shipmentFilter(centreIds)).successNel[String]
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
