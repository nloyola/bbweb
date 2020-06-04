package org.biobank.validation

import cats.data._
import cats.implicits._
import org.biobank.domain.IdentifiedValueObject
import scala.concurrent.Future

object Validation {

  trait ValidationError {
    def errorMessage: String
  }

  trait BadRequest extends ValidationError

  trait Unauthorized extends ValidationError

  trait Forbidden extends ValidationError

  trait NotFound extends ValidationError

  type ValidationResult[A] = ValidatedNec[ValidationError, A]

  type FutureValidationResult[A] = EitherT[Future, NonEmptyChain[ValidationError], A]

  final case class Error(msg: String) extends Forbidden {
    def errorMessage: String = msg
  }

  case object NotPermitted extends Forbidden {
    def errorMessage: String = "not permitted"
  }

  case object NonEmptyString extends Forbidden {
    def errorMessage: String = "string cannot be empty"
  }

  case object IdEmpty extends Forbidden {
    def errorMessage: String = "id cannot be empty"
  }

  final case class IdNotFound(msg: String) extends NotFound {
    def errorMessage: String = msg
  }

  case object InvalidVersion extends Forbidden {
    def errorMessage: String = "version number is invalid"
  }

  final case class IllegalStateError(msg: String) extends Forbidden {
    def errorMessage: String = msg
  }

  final case class EntityCriteriaNotFound(msg: String) extends NotFound {
    def errorMessage: String = msg
  }

  final case class EntityCriteriaError(msg: String) extends Forbidden {
    def errorMessage: String = msg
  }

  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments", "org.wartremover.warts.Null"))
  def validateNonEmptyString(s: String, err: ValidationError = NonEmptyString): ValidationResult[String] =
    if ((s == null) || s.isEmpty()) err.invalidNec else s.validNec

  def validateVersion(v: Long): ValidationResult[Long] =
    if (v >= 0) v.validNec else InvalidVersion.invalidNec

  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  def validateId[T <: IdentifiedValueObject[_]](
      id:  T,
      err: ValidationError = IdEmpty
    ): ValidationResult[T] = {
    validateNonEmptyString(id.toString, err).map(_ => id)
  }

  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  def validateIdOption[T <: IdentifiedValueObject[_]](
      id:  Option[T],
      err: ValidationError = IdEmpty
    ): ValidationResult[Option[T]] =
    id.traverse(validateId(_, err))

}
