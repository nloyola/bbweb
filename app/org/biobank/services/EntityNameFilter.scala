package org.biobank.services

import cats.implicits._
import org.biobank.domain.{HasName, HasNamePredicates, PredicateHelper}
import org.biobank.validation.Validation._
import scalaz.Scalaz._

trait EntityNameFilter[A <: HasName] extends PredicateHelper with HasNamePredicates[A] {
  import Comparator._

  protected def nameFilter(
      comparator: Comparator,
      names:      List[String]
    ): ServiceValidation[EntityNameFilter] = {
    val nameSet = names.toSet
    comparator match {
      case Equal              => nameIsOneOf(nameSet).successNel[String]
      case In                 => nameIsOneOf(nameSet).successNel[String]
      case NotEqualTo | NotIn => complement(nameIsOneOf(nameSet)).successNel[String]
      case Like               => nameIsLike(nameSet).successNel[String]
      case _                  => ServiceError(s"invalid filter on name: $comparator").failureNel[EntityNameFilter]
    }
  }

  protected def nameFilterCats(
      comparator: Comparator,
      names:      List[String]
    ): ValidationResult[EntityNameFilter] = {
    val nameSet = names.toSet
    comparator match {
      case Equal              => nameIsOneOf(nameSet).validNec
      case In                 => nameIsOneOf(nameSet).validNec
      case NotEqualTo | NotIn => complement(nameIsOneOf(nameSet)).validNec
      case Like               => nameIsLike(nameSet).validNec
      case _                  => Error(s"invalid filter on name: $comparator").invalidNec
    }
  }

}
