package org.biobank.services

import cats.data._
import cats.implicits._
import org.biobank.domain.{EntityState, HasState, HasStatePredicates, PredicateHelper}
import org.biobank.validation.Validation._

trait EntityStateFilter[A <: HasState] extends PredicateHelper with HasStatePredicates[A] {
  import Comparator._

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  protected def stateFilter(
      comparator:  Comparator,
      stateNames:  List[String],
      validStates: List[EntityState]
    ): ServiceValidation[EntityStateFilter] = {
    import scalaz.Scalaz._
    import scalaz.Validation.FlatMap._
    import org.biobank.CommonValidations._

    stateNames
      .map { str =>
        validStates
          .find(_.id == str).toSuccessNel(InvalidState(s"entity state does not exist: $str").toString)
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
            ServiceError(s"invalid filter on state: $comparator").failureNel[EntityStateFilter]
        }
      }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  protected def stateFilterCats(
      comparator:  Comparator,
      stateNames:  List[String],
      validStates: List[EntityState]
    ): ValidationResult[EntityStateFilter] = {
    stateNames
      .map { str =>
        Validated
          .fromOption[ValidationError, EntityState](
            validStates.find(_.id == str),
            EntityCriteriaError(s"state does not exist: $str")
          ).toValidatedNec
      }
      .toList.sequence
      .andThen { states =>
        val stateSet = states.toSet
        comparator match {
          case Equal | In         => stateIsOneOf(stateSet).validNec
          case NotEqualTo | NotIn => complement(stateIsOneOf(stateSet)).validNec
          case _                  => Error(s"invalid filter on state: $comparator").invalidNec
        }
      }
  }

}
