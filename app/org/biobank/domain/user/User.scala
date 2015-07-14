package org.biobank.domain.user

import org.biobank.domain.{
  CommonValidations,
  ConcurrencySafeEntity,
  DomainValidation,
  DomainError,
  ValidationKey }
import org.joda.time.DateTime
import org.biobank.infrastructure.JsonUtils._

import play.api.libs.json._
import scalaz.Scalaz._

/** A user of the system.
  */
sealed trait User extends ConcurrencySafeEntity[UserId] {

  /** The user's full name. */
  val name: String

  /** The user's email. Must be unique to the system. */
  val email: String

  /** The user's password */
  val password: String

  /** The string used to salt the password. */
  val salt: String

  /** An optional URL to the user's avatar icon. */
  val avatarUrl: Option[String]

  /**
   * Authenticate a user.
   */
  def authenticate(email: String, password: String): DomainValidation[User] = {
    if (this.password == password) this.success
    else DomainError("authentication failure").failureNel
  }

  override def toString =
    s"""|${this.getClass.getSimpleName}: {
        |  id: $id,
        |  version: $version,
        |  timeAdded: $timeAdded,
        |  timeModified: $timeModified,
        |  name: $name,
        |  email: $email,
        |  password: $password,
        |  salt: $salt,
        |  avatarUrl: $avatarUrl,
        |}""".stripMargin
}

object User {

  implicit val userWrites = new Writes[User] {
    def writes(user: User) = {
      Json.obj(
        "id"           -> user.id,
        "version"      -> user.version,
        "timeAdded"    -> user.timeAdded,
        "timeModified" -> user.timeModified,
        "name"         -> user.name,
        "email"        -> user.email,
        "avatarUrl"    -> user.avatarUrl,
        "status"       -> user.getClass.getSimpleName
      )
    }
  }

  // users with duplicate emails are not allowed
  def compareByEmail(a: User, b: User) = (a.email compareToIgnoreCase b.email) < 0

  def compareByName(a: User, b: User) = {
    val nameCompare = a.name compareToIgnoreCase b.name
    if (nameCompare == 0) {
      compareByEmail(a, b)
    } else {
      nameCompare < 0
    }
  }

  def compareByStatus(a: User, b: User) = {
    val statusCompare = a.getClass.getSimpleName compare b.getClass.getSimpleName
    if (statusCompare == 0) {
      compareByName(a, b)
    } else {
      statusCompare < 0
    }
  }
}

trait UserValidations {
  val NameMinLength = 2

  case object PasswordRequired extends ValidationKey

  case object SaltRequired extends ValidationKey

  case object InvalidName extends ValidationKey

  case object InvalidEmail extends ValidationKey

  case object InvalidUrl extends ValidationKey

  val emailRegex = "[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?".r
  val urlRegex = "^((https?|ftp)://|(www|ftp)\\.)[a-z0-9-]+(\\.[a-z0-9-]+)+([/?].*)?$".r

  def validateEmail(email: String): DomainValidation[String] = {
    emailRegex.findFirstIn(email).fold { InvalidEmail.toString.failureNel[String] } { e => email.successNel }
  }

  def validateAvatarUrl(urlOption: Option[String]): DomainValidation[Option[String]] = {
    urlOption.fold {
      none[String].successNel[String]
    } { url  =>
      urlRegex.findFirstIn(url).fold {
        InvalidUrl.toString.failureNel[Option[String]]
      } { e =>
        some(url).successNel
      }
    }
  }
}


/** A user that just registered with the system. This user does not yet have full access
  * the system.
  */
case class RegisteredUser(id:           UserId,
                          version:      Long,
                          timeAdded:    DateTime,
                          timeModified: Option[DateTime],
                          name:         String,
                          email:        String,
                          password:     String,
                          salt:         String,
                          avatarUrl:    Option[String]) extends User with UserValidations {

  /* Activates a registered user. */
  def activate(): DomainValidation[ActiveUser] = {
    ActiveUser(id           = this.id,
               version      = this.version + 1,
               timeAdded    = this.timeAdded,
               timeModified = this.timeModified,
               name         = this.name,
               email        = this.email,
               password     = this.password,
               salt         = this.salt,
               avatarUrl    = this.avatarUrl).success
  }
}

/** Factory object. */
object RegisteredUser extends UserValidations {
  import CommonValidations._

  /** Creates a registered user. */
  def create(id:        UserId,
             version:   Long,
             name:      String,
             email:     String,
             password:  String,
             salt:      String,
             avatarUrl: Option[String]): DomainValidation[RegisteredUser] = {

    (validateId(id) |@|
      validateAndIncrementVersion(version) |@|
      validateString(name, NameMinLength, InvalidName) |@|
      validateEmail(email) |@|
      validateString(password, PasswordRequired) |@|
      validateString(salt, SaltRequired) |@|
      validateAvatarUrl(avatarUrl)) {
        RegisteredUser(_, _, DateTime.now, None, _, _, _, _, _)
      }
  }

}

/** A user that has access to the system. */
case class ActiveUser(id:           UserId,
                      version:      Long = -1,
                      timeAdded:    DateTime,
                      timeModified: Option[DateTime],
                      name:         String,
                      email:        String,
                      password:     String,
                      salt:         String,
                      avatarUrl:    Option[String])
    extends User
    with UserValidations {
  import CommonValidations._

  def withName(name: String): DomainValidation[ActiveUser] = {
    validateString(name, NameMinLength, InvalidName).fold(
      err => err.failure,
      n => copy(version = version + 1, name = n).success
    )
  }

  def withEmail(email: String): DomainValidation[ActiveUser] = {
    validateEmail(email).fold(
      err => err.failure,
      e => copy(version = version + 1, email = e).success
    )
  }

  def withPassword(password: String, salt: String): DomainValidation[ActiveUser] = {
    validateString(password, PasswordRequired).fold(
      err => err.failure,
      pwd => copy(version      = version + 1,
                  password     = pwd,
                  salt         = salt).success
    )
  }

  def withAvatarUrl(avatarUrl: Option[String]): DomainValidation[ActiveUser] = {
    validateAvatarUrl(avatarUrl).fold(
      err => err.failure,
      a => copy(version = version + 1, avatarUrl = a).success

    )
  }

  /** Locks an active user. */
  def lock(): DomainValidation[LockedUser] = {
    LockedUser(id           = this.id,
               version      = this.version + 1,
               timeAdded    = this.timeAdded,
               timeModified = this.timeModified,
               name         = this.name,
               email        = this.email,
               password     = this.password,
               salt         = this.salt,
               avatarUrl    = this.avatarUrl).success
  }
}

/** A user who no longer has access to the system. */
case class LockedUser(id:           UserId,
                      version:      Long,
                      timeAdded:    DateTime,
                      timeModified: Option[DateTime],
                      name:         String,
                      email:        String,
                      password:     String,
                      salt:         String,
                      avatarUrl:    Option[String])
    extends User {

  /** Unlocks a locked user. */
  def unlock(): DomainValidation[ActiveUser] = {
    ActiveUser(id           = this.id,
               version      = this.version + 1,
               timeAdded    = this.timeAdded,
               timeModified = this.timeModified,
               name         = this.name,
               email        = this.email,
               password     = this.password,
               salt         = this.salt,
               avatarUrl    = this.avatarUrl).success
  }

}

object UserHelper {

  def isUserRegistered(user: User): DomainValidation[RegisteredUser] = {
    user match {
      case registeredUser: RegisteredUser => registeredUser.success
      case _ => DomainError(s"the user is not registered").failureNel
    }
  }

  def isUserActive(user: User): DomainValidation[ActiveUser] = {
    user match {
      case activeUser: ActiveUser => activeUser.success
      case _ => DomainError(s"the user is not active").failureNel
    }
  }

  def isUserLocked(user: User): DomainValidation[LockedUser] = {
    user match {
      case lockedUser: LockedUser => lockedUser.success
      case _ => DomainError(s"the user is not active").failureNel
    }
  }

  def isUserNotLocked(user: User): DomainValidation[User] = {
    user match {
      case lockedUser: LockedUser => DomainError(s"the user is locked").failureNel
      case _ => user.success
    }
  }
}
