package org.biobank.services.centres

import org.biobank.services._
import org.biobank.services.Comparator._
import org.biobank.services.{ServiceValidation, ServiceError}
import org.biobank.domain.containers.{Container, ContainerPredicates}
import scalaz.Scalaz._

/**
 * Functions that filter a set of containers from an expression contained in a filter string.
 *
 */
object ContainerFilter
    extends EntityFilter[Container]
    with ContainerPredicates {

  def filterContainers(containers: Set[Container], filter: FilterString):ServiceValidation[Set[Container]] = {
    filterEntities(containers, filter, containers.filter)
  }

  protected def predicateFromSelector(selector: String, comparator: Comparator, args: List[String])
      : ServiceValidation[Container => Boolean] = {
    selector match {
      case "label"  => labelFilter(comparator, args)
      case _ =>
        ServiceError(s"invalid filter selector: $selector").failureNel[ContainerFilter]
    }
  }

  protected def labelFilter(comparator: Comparator, labels: List[String]) = {
    val labelSet = labels.toSet
    comparator match {
      case Equal =>
        labelIsOneOf(labelSet).successNel[String]
      case In =>
        labelIsOneOf(labelSet).successNel[String]
      case NotEqualTo | NotIn =>
        complement(labelIsOneOf(labelSet)).successNel[String]
      case Like =>
        labelIsLike(labelSet).successNel[String]
      case _ =>
        ServiceError(s"invalid filter on label: $comparator").failureNel[ContainerFilter]
    }
  }
}
