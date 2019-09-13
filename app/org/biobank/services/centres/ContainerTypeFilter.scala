package org.biobank.services.centres

import org.biobank.services._
import org.biobank.services.Comparator._
import org.biobank.services.{ServiceError, ServiceValidation}
import org.biobank.domain.containers.{ContainerType, ContainerTypePredicates}
import scalaz.Scalaz._

/**
 * Functions that filter a set of containerTypes from an expression contained in a filter string.
 *
 */
object ContainerTypeFilter
    extends EntityFilter[ContainerType] with EntityNameFilter[ContainerType] with ContainerTypePredicates {

  def filterContainerTypes(
      containerTypes: Set[ContainerType],
      filter:         FilterString
    ): ServiceValidation[Set[ContainerType]] =
    filterEntities(containerTypes, filter, containerTypes.filter)

  protected def predicateFromSelector(
      selector:   String,
      comparator: Comparator,
      args:       List[String]
    ): ServiceValidation[ContainerType => Boolean] =
    selector match {
      case "name" => nameFilter(comparator, args)
      case _ =>
        ServiceError(s"invalid filter selector: $selector").failureNel[ContainerTypeFilter]
    }
}
