package org.biobank.services.centres

import org.biobank.domain.containers.{ContainerSchema, ContainerSchemaPredicates}
import org.biobank.services._
import org.biobank.services.Comparator._
import org.biobank.services.{ServiceError, ServiceValidation}
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

}
