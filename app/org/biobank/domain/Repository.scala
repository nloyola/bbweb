package org.biobank.domain

import org.biobank.FutureValidation
import org.biobank.CommonValidations.EntityCriteriaNotFound
import scala.concurrent.Future

/**
 * A read-only repository.
 */
trait ReadRepository[K, A] {

  def isEmpty: Boolean

  def getByKey(key: K): DomainValidation[A]

  def getValues: Iterable[A]

  def getKeys: Iterable[K]

  def exists(predicate: A => Boolean): Boolean

}

/**
 * A read-only repository.
 */
trait AsyncReadRepository[K, A] {

  def isEmpty: Future[Boolean]

  def getByKey(key: K): FutureValidation[A]

  def getValues: Future[Iterable[A]]

  def getKeys: Future[Iterable[K]]

  def exists(predicate: A => Boolean): Future[Boolean]

  def init(): Future[Unit]
}

/** A read/write repository.
 */
trait ReadWriteRepository[K, A] extends ReadRepository[K, A] {

  def nextIdentity(): K

  protected def nextIdentityAsString: String

  def init(): Unit

  def put(value: A): Unit

  def remove(value: A): Unit

  def removeAll(): Unit

}

trait ReadWriteRepositoryWithSlug[K, A] extends ReadWriteRepository[K, A] {

  /** if slug is already used, then create a new one with a count appended to it. */
  def uniqueSlug(slug: Slug): Slug

  def getBySlug(slug: Slug): DomainValidation[A]

  /** if slug is already used, then create a new one with a count appended to it. */
  def uniqueSlugFromStr(name: String): Slug

  protected def slugNotFound(slug: Slug): EntityCriteriaNotFound
}
