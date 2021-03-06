package org.biobank.services

import com.google.inject.ImplementedBy
import javax.inject.{Inject, Singleton}
import org.biobank.domain.users.UserId
import play.api.Environment
import play.api.cache.SyncCacheApi
import scala.concurrent.duration._
import scala.util.Random
import scalaz.Scalaz._

/**
 * Manages tokens used to authenticate logged in users.
 *
 * Random string generation from here:
 *
 * - http://www.bindschaedler.com/2012/04/07/elegant-random-string-generation-in-scala/
 */
@ImplementedBy(classOf[AuthTokenImpl])
trait AuthToken {

  val env: Environment

  val cacheApi: SyncCacheApi

  def newToken(userId: UserId): String

  def removeToken(token: String): Unit

  def getUserId(token: String): ServiceValidation[UserId]

}

@Singleton
class AuthTokenImpl @Inject()(val env: Environment, val cacheApi: SyncCacheApi) extends AuthToken {
  import org.biobank.CommonValidations._

  val random: Random = new Random(new java.security.SecureRandom())

  val tokenExpirationTime: FiniteDuration =
    if (env.mode == play.api.Mode.Prod) 15.minutes
    else 60.minutes
  //else 5.seconds

  /**
   *  Generates a new token for userId with an expiration of tokenExpirationTime.
   *
   *  TODO: Should token be derived from salt? not sure if required if server only runs HTTPS.
   */
  def newToken(userId: UserId): String = {
    val token = randomAlphanumericString(64)
    cacheApi.set(token, userId, tokenExpirationTime)
    token
  }

  /**
   * Removes a token from the cache.
   */
  def removeToken(token: String): Unit = cacheApi.remove(token)

  /**
   *  If token is valid then the timeout is re-assigned on the cache.
   */
  def getUserId(token: String): ServiceValidation[UserId] = {
    val userId = cacheApi.get[UserId](token).toSuccessNel(InvalidToken.toString)
    userId foreach { cacheApi.set(token, _, tokenExpirationTime) }
    userId
  }

  // Generate a random string of length n from the given alphabet
  private def randomString(alphabet: String)(n: Int): String =
    LazyList.continually(random.nextInt(alphabet.size)).map(alphabet).take(n).mkString

  // Generate a random alphabnumeric string of length n
  private def randomAlphanumericString(n: Int) =
    randomString("abcdefghijklmnopqrstuvwxyz0123456789")(n)

}
