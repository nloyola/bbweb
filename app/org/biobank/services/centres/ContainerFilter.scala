package org.biobank.services.centres

import cats.implicits._
import org.biobank.services._
import org.biobank.services.Comparator._
import org.biobank.services.{ServiceError, ServiceValidation}
import org.biobank.domain.containers.{Container, ContainerPredicates}
import org.biobank.validation.Validation._
import scalaz.Scalaz._

/**
 * Functions that filter a set of containers from an expression contained in a filter string.
 *
 */
object ContainerFilter extends EntityFilter[Container] with ContainerPredicates {

  def filterContainers(containers: Set[Container], filter: FilterString): ServiceValidation[Set[Container]] =
    filterEntities(containers, filter, containers.filter)

  protected def predicateFromSelector(
      selector:   String,
      comparator: Comparator,
      args:       List[String]
    ): ServiceValidation[Container => Boolean] =
    selector match {
      case "label" => labelFilter(comparator, args)
      case _ =>
        ServiceError(s"invalid filter selector: $selector").failureNel[ContainerFilter]
    }

  protected def predicateFromSelectorCats(
      selector:   String,
      comparator: Comparator,
      args:       List[String]
    ): ValidationResult[Container => Boolean] =
    selector match {
      case "label" => labelFilterCats(comparator, args)
      case _       => Error(s"invalid filter selector: $selector").invalidNec
    }

  protected def labelFilter(comparator: Comparator, labels: List[String]) = {
    val labelSet = labels.toSet
    comparator match {
      case Equal              => labelIsOneOf(labelSet).successNel[String]
      case In                 => labelIsOneOf(labelSet).successNel[String]
      case NotEqualTo | NotIn => complement(labelIsOneOf(labelSet)).successNel[String]
      case Like               => labelIsLike(labelSet).successNel[String]
      case _                  => ServiceError(s"invalid filter on label: $comparator").failureNel[ContainerFilter]
    }
  }

  protected def labelFilterCats(
      comparator: Comparator,
      labels:     List[String]
    ): ValidationResult[ContainerFilter] = {
    val labelSet = labels.toSet
    comparator match {
      case Equal              => labelIsOneOf(labelSet).validNec
      case In                 => labelIsOneOf(labelSet).validNec
      case NotEqualTo | NotIn => complement(labelIsOneOf(labelSet)).validNec
      case Like               => labelIsLike(labelSet).validNec
      case _                  => Error(s"invalid filter on label: $comparator").invalidNec
    }
  }
}
