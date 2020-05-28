package org.biobank.services.centres

import cats.implicits._
import org.biobank.services._
import org.biobank.services.Comparator._
import org.biobank.services.{ServiceError, ServiceValidation}
import org.biobank.domain.containers.{ContainerType, ContainerTypePredicates}
import org.biobank.validation.Validation._
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

  protected def predicateFromSelectorCats(
      selector:   String,
      comparator: Comparator,
      args:       List[String]
    ): ValidationResult[ContainerType => Boolean] =
    selector match {
      case "name" => nameFilterCats(comparator, args)
      case _      => Error(s"invalid filter selector: $selector").invalidNec
    }
}
