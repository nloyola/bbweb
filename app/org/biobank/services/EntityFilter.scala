package org.biobank.services

import cats.implicits._
import org.biobank.domain._
import org.biobank.validation.Validation._

/**
 * Functions that filter a set of studys from an expression contained in a filter string.
 *
 */
trait EntityFilter[T] extends PredicateHelper {

  import org.biobank.services.Comparator._
  import org.biobank.services.QueryFilterParserGrammar._

  def filterEntities(
      entities:   Set[T],
      filter:     FilterString,
      filterFunc: (T => Boolean) => Set[T]
    ): ServiceValidation[Set[T]] = {
    import scalaz.Scalaz._
    import scalaz.Validation.FlatMap._

    QueryFilterParser.expressions(filter).flatMap { filterExpression =>
      filterExpression match {
        case None =>
          entities.successNel[String]
        case Some(c: Comparison) =>
          comparisonToPredicates(c).map(filterFunc)
        case Some(e: AndExpression) =>
          comparisonToPredicates(e).map(filterFunc)
        case Some(e: OrExpression) =>
          comparisonToPredicates(e).map(filterFunc)
        case _ =>
          ServiceError(s"bad filter expression: $filterExpression").failureNel[Set[T]]
      }
    }
  }

  protected def predicateFromSelector(
      selector:   String,
      comparator: Comparator,
      args:       List[String]
    ): ServiceValidation[T => Boolean]

  @SuppressWarnings(Array("org.wartremover.warts.Recursion", "org.wartremover.warts.Any"))
  private def comparisonToPredicates(expression: Expression): ServiceValidation[T => Boolean] = {
    import scalaz.Scalaz._

    expression match {
      case Comparison(selector, comparator, args) =>
        predicateFromSelector(selector, comparator, args)
      case AndExpression(expressions) =>
        expressions.map(comparisonToPredicates).sequenceU.map(x => every(x: _*))
      case OrExpression(expressions) =>
        expressions.map(comparisonToPredicates).sequenceU.map(x => any(x: _*))
      case _ =>
        ServiceError(s"invalid filter expression: $expression").failureNel[T => Boolean]
    }
  }

}

trait EntityFilterCats[T] extends EntityFilter[T] {

  import org.biobank.services.Comparator._
  import org.biobank.services.QueryFilterParserGrammar._

  def filterEntitiesCats(
      entities:   Set[T],
      filter:     FilterString,
      filterFunc: (T => Boolean) => Set[T]
    ): ValidationResult[Set[T]] = {
    QueryFilterParser.expressionsCats(filter).andThen { filterExpression =>
      filterExpression match {
        case None => entities.validNec
        case Some(c: Comparison)    => comparisonToPredicatesCats(c).map(filterFunc)
        case Some(e: AndExpression) => comparisonToPredicatesCats(e).map(filterFunc)
        case Some(e: OrExpression)  => comparisonToPredicatesCats(e).map(filterFunc)
        case _ => Error(s"bad filter expression: $filterExpression").invalidNec
      }
    }
  }

  protected def predicateFromSelectorCats(
      selector:   String,
      comparator: Comparator,
      args:       List[String]
    ): ValidationResult[T => Boolean]

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  private def comparisonToPredicatesCats(expression: Expression): ValidationResult[T => Boolean] = {
    expression match {
      case Comparison(selector, comparator, args) =>
        predicateFromSelectorCats(selector, comparator, args)
      case AndExpression(expressions) =>
        expressions.map(comparisonToPredicatesCats).sequence.map(x => every(x: _*))
      case OrExpression(expressions) =>
        expressions.map(comparisonToPredicatesCats).sequence.map(x => any(x: _*))
      case _ =>
        Error(s"invalid filter expression: $expression").invalidNec
    }
  }

}
