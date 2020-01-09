package org

import scala.util.matching.Regex
import scala.util.control.Exception._
import scala.concurrent.{ExecutionContext, Future}
import scalaz._
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._

package biobank {

  /**
   * Trait for validation errors
   */
  trait ValidationKey {
    def failure[T]:    Validation[String, T]    = this.toString.failure[T]
    def failureNel[T]: ValidationNel[String, T] = this.toString.failureNel[T]
    def nel:           NonEmptyList[String]     = NonEmptyList(this.toString)
  }

  trait ValidationMsgKey extends ValidationKey {
    def msg: String

    override val toString: String = s"${this.getClass.getSimpleName}: $msg"
  }

  object CommonValidations {

    final case class SystemErrorMsg(msg: String) extends ValidationMsgKey

    final case class InvalidVersion(msg: String) extends ValidationMsgKey

    case object NonEmptyString extends ValidationKey

    case object InternalServerError extends ValidationKey

    case object Unauthorized extends ValidationKey

    final case class InvalidNumberString(msg: String) extends ValidationMsgKey

    final case class IdNotFound(msg: String) extends ValidationMsgKey

    final case class EntityNotFound(msg: String) extends ValidationMsgKey

    final case class InvalidStatus(msg: String) extends ValidationMsgKey

    final case class InvalidState(msg: String) extends ValidationMsgKey

    case object InvalidToken extends ValidationKey

    case object InvalidPassword extends ValidationKey

    final case class EmailNotFound(msg: String) extends ValidationMsgKey

    final case class EmailNotAvailable(msg: String) extends ValidationMsgKey

    final case class EntityCriteriaNotFound(msg: String) extends ValidationMsgKey

    final case class EntityCriteriaError(msg: String) extends ValidationMsgKey

    final case class EntityRequired(msg: String) extends ValidationMsgKey

    case object LocationIdInvalid extends ValidationKey

    case object ContainerIdInvalid extends ValidationKey

    case object SpecimenIdRequired extends ValidationKey

    final case class EntityInUse(msg: String) extends ValidationMsgKey

    case object InvalidAlphaString extends ValidationKey

    case object InvalidAlnumString extends ValidationKey

    case object InvalidEmail extends ValidationKey

    case object InvalidUrl extends ValidationKey

    case object InvalidName extends ValidationMsgKey {
      val msg = "must contain only letters (a-z), digits (0-9), period or quote"
    }

    @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments", "org.wartremover.warts.Null"))
    def validateNonEmptyString(s: String, err: ValidationKey = NonEmptyString): SystemValidation[String] =
      if ((s == null) || s.isEmpty()) err.failureNel[String] else s.successNel[String]

    def validateString(s: String, err: ValidationKey): SystemValidation[String] =
      if (s == null) err.failureNel[String] else s.successNel[String]

    def validateNonEmptyStringOption(
        maybeString: Option[String],
        err:         ValidationKey
      ): SystemValidation[Option[String]] =
      maybeString match {
        case None => maybeString.successNel[String]
        case Some(s) =>
          validateNonEmptyString(s, err)
            .fold(err => err.failure[Option[String]], str => Some(str).successNel[String])
      }

    @SuppressWarnings(Array("org.wartremover.warts.Overloading", "org.wartremover.warts.DefaultArguments"))
    def validateString(
        str:       String,
        minLength: Long,
        err:       ValidationKey = NonEmptyString
      ): SystemValidation[String] =
      for {
        valid <- validateString(str, err)
        lenValid <- {
          if (str.length < minLength) err.failureNel[String]
          else str.successNel[String]
        }
      } yield lenValid

    @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
    def validateNumberString(
        str: String,
        err: ValidationKey = InvalidNumberString("")
      ): SystemValidation[String] =
      for {
        valid <- validateString(str, err)
        number <- {
          if (str.isEmpty) {
            err.toString.failureNel[Float]
          } else {
            catching[Float](classOf[NumberFormatException])
              .opt(str.toFloat)
              .toSuccessNel(err.toString)
          }
        }
      } yield valid

    @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
    def validateNumberStringOption(
        str: Option[String],
        err: ValidationKey = InvalidNumberString("")
      ): SystemValidation[Option[String]] =
      str match {
        case Some(s) =>
          validateNumberString(s, err)
            .fold(err => err.failure[Option[String]], _ => Some(s).successNel[String])
        case None => str.successNel[String]
      }

    def validateMinimum(number: Int, min: Int, err: ValidationKey): SystemValidation[Int] =
      if (number < min) err.failureNel[Int] else number.successNel[String]

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def validatePositiveNumber(number: Int, err: ValidationKey): SystemValidation[Int] =
      validateMinimum(number, 0, err)

    @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
    def validatePositiveNumber(number: BigDecimal, err: ValidationKey): SystemValidation[BigDecimal] =
      if (number < 0) err.failureNel[BigDecimal] else number.successNel[String]

    private val alphaRegex: Regex = "^[a-zA-Z ]+$".r

    @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
    def validateAlpha(value: String, err: ValidationKey = InvalidAlphaString): SystemValidation[String] =
      alphaRegex.findFirstIn(value).toSuccessNel(err.toString)

    private val alnumRegex: Regex = "^[a-zA-Z0-9 ]+$".r

    @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
    def validateAlnum(value: String, err: ValidationKey = InvalidAlnumString): SystemValidation[String] =
      alnumRegex.findFirstIn(value).toSuccessNel(err.toString)

    private val nameRegex: Regex = "^[a-zA-Z0-9\\.' ]+$".r

    @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
    def validateName(value: String, err: ValidationKey = InvalidName): SystemValidation[String] =
      nameRegex.findFirstIn(value).toSuccessNel(err.toString)

    private val emailRegex: Regex =
      "[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?".r

    def validateEmail(email: String): SystemValidation[String] =
      emailRegex.findFirstIn(email).toSuccessNel(InvalidEmail.toString)

    private val urlRegex: Regex =
      "^((https?|ftp)://|(www|ftp)\\.)[a-z0-9-]+(\\.[a-z0-9-]+)+([/?].*)?$".r

    def validateUrl(url: String): SystemValidation[String] =
      urlRegex.findFirstIn(url).toSuccessNel(InvalidUrl.toString)

    def validateOptionalUrl(url: Option[String]): SystemValidation[Option[String]] =
      url match {
        case Some(u) =>
          validateUrl(u).fold(err => err.failure[Option[String]], str => Some(str).successNel[String])
        case None => url.successNel[String]
      }

  }

  final case class FutureValidation[A](futval: Future[SystemValidation[A]]) {

    @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
    def map[B](f: A => B)(implicit executor: ExecutionContext): FutureValidation[B] = {
      val result = futval.map(_.fold(fail => Failure(fail), succ => Success(f(succ))))
      FutureValidation(result)
    }

    @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
    def flatMap[B](
        f: A => FutureValidation[B]
      )(
        implicit
        executor: ExecutionContext
      ): FutureValidation[B] = {
      val result = futval.flatMap(_.fold(fail => Future(Failure(fail)), succ => f(succ).futval))
      FutureValidation(result)
    }
  }

  object FutureValidation {

    @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter", "org.wartremover.warts.Overloading"))
    def apply[A](
        validation: => SystemValidation[A]
      )(
        implicit
        executor: ExecutionContext
      ): FutureValidation[A] =
      apply(Future(validation))
  }

}

// move package object here due to: https://issues.scala-lang.org/browse/SI-9922
package object biobank {

  /** Contains an error messsage when an invalid condition happens. */
  type SystemError = String

  /** Used by functions to return validated results. */
  type SystemValidation[A] = ValidationNel[String, A]
}
