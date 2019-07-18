package org.biobank.utils.auth

import javax.inject.Inject

import com.mohiva.play.silhouette.api.LoginInfo
import com.mohiva.play.silhouette.api.services.IdentityService
import org.biobank.domain.users.UserId
import org.biobank.services.users.UsersService
import scala.concurrent.{ ExecutionContext, Future }

/**
 * Handles actions to users.
 */
trait UserService extends IdentityService[User]

/**
 * Handles actions to users.
 *
 * @param userDAO The user DAO implementation.
 */
 @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
class UserServiceImpl @Inject() (usersService: UsersService,
                                 implicit val executionContext: ExecutionContext)
    extends UserService {

  // def retrieve(id: UUID) = Future {
  //     usersService.getUser(UserId(id.toString)).toOption.map(User.apply)
  //   }

  def retrieve(loginInfo: LoginInfo): Future[Option[User]] = Future {
      usersService.getUser(UserId(loginInfo.providerKey)).toOption.map(User.apply)
    }

}
