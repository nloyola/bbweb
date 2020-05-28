package org.biobank.services.centres

import cats.implicits._
import org.biobank.domain.containers.{ContainerSchema, ContainerSchemaPredicates}
import org.biobank.services._
import org.biobank.services.Comparator._
import org.biobank.services.{ServiceError, ServiceValidation}
import org.biobank.validation.Validation._
import scalaz.Scalaz._

/**
 * Functions that filter a set of studys from an expression contained in a filter string.
 *
 */
object ContainerSchemaFilter
    extends EntityFilter[ContainerSchema] with EntityNameFilter[ContainerSchema]
    with ContainerSchemaPredicates {

  def filterSchemas(
      schemas: Set[ContainerSchema],
      filter:  FilterString
    ): ServiceValidation[Set[ContainerSchema]] =
    filterEntities(schemas, filter, schemas.filter)

  protected def predicateFromSelector(
      selector:   String,
      comparator: Comparator,
      args:       List[String]
    ): ServiceValidation[ContainerSchema => Boolean] =
    selector match {
      case "name" => nameFilter(comparator, args)
      case _      => ServiceError(s"invalid filter selector: $selector").failureNel[ContainerSchemaFilter]
    }

  protected def predicateFromSelectorCats(
      selector:   String,
      comparator: Comparator,
      args:       List[String]
    ): ValidationResult[ContainerSchema => Boolean] =
    selector match {
      case "name" => nameFilterCats(comparator, args)
      case _      => Error(s"invalid filter selector: $selector").invalidNec
    }
}
