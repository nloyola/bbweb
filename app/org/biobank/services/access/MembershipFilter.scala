package org.biobank.services.access

import cats.implicits._
import org.biobank.domain.access._
import org.biobank.services._
import org.biobank.services.{ServiceError, ServiceValidation}
import org.biobank.validation.Validation._
import scalaz.Scalaz._

/**
 * Functions that filter a set of memberships from an expression contained in a filter string.
 *
 */
object MembershipFilter
    extends EntityFilter[Membership] with EntityNameFilter[Membership] with MembershipPredicates {

  import org.biobank.services.Comparator._

  def filterMemberships(
      memberships: Set[Membership],
      filter:      FilterString
    ): ServiceValidation[Set[Membership]] =
    filterEntities(memberships, filter, memberships.filter)

  protected def predicateFromSelector(
      selector:   String,
      comparator: Comparator,
      args:       List[String]
    ): ServiceValidation[Membership => Boolean] =
    selector match {
      case "name" => nameFilter(comparator, args)
      case _ =>
        ServiceError(s"invalid filter selector: $selector").failureNel[MembershipFilter]
    }

  protected def predicateFromSelectorCats(
      selector:   String,
      comparator: Comparator,
      args:       List[String]
    ): ValidationResult[Membership => Boolean] =
    selector match {
      case "name" => nameFilterCats(comparator, args)
      case _      => Error(s"invalid filter selector: $selector").invalidNec
    }
}
