package org.biobank.domain

import org.biobank.validation.Validation._
import scala.concurrent.Future

/**
 * A read-only repository.
 */
trait CatsReadRepository[K, A] {

  def isEmpty: Boolean

  def getByKey(key: K): Option[A]

  def getValues: Iterable[A]

  def getKeys: Iterable[K]

  def exists(predicate: A => Boolean): Boolean

  def find(predicate: A => Boolean): Option[A]

}

/**
 * A read-only repository.
 */
trait CatsAsyncReadRepository[K, A] {

  def isEmpty: Future[Boolean]

  def getByKey(key: K): FutureValidationResult[A]

  def getValues: Future[Iterable[A]]

  def getKeys: Future[Iterable[K]]

  def exists(predicate: A => Boolean): Future[Boolean]

  def init(): Future[Unit]
}

/** A read/write repository.
 */
trait CatsReadWriteRepository[K, A] extends CatsReadRepository[K, A] {

  def nextIdentity(): K

  protected def nextIdentityAsString: String

  def init(): Unit

  def put(value: A): Unit

  def remove(value: A): Unit

  def removeAll(): Unit

}

trait CatsReadWriteRepositoryWithSlug[K, A] extends CatsReadWriteRepository[K, A] {

  /** if slug is already used, then create a new one with a count appended to it. */
  def uniqueSlug(slug: Slug): Slug

  def getBySlug(slug: Slug): Option[A]

  /** if slug is already used, then create a new one with a count appended to it. */
  def uniqueSlugFromStr(name: String): Slug

  protected def slugNotFound(slug: Slug): EntityCriteriaNotFound
}
