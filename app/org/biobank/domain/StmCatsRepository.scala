package org.biobank.domain

import scala.concurrent.stm.Ref
import org.biobank.validation.Validation._

/**
 * A read-only wrapper around an STM Ref of a Map.
 */
abstract class StmCatsReadRepositoryImpl[K, A](keyGetter: A => K) extends CatsReadRepository[K, A] {

  protected val internalMap: Ref[Map[K, A]] = Ref(Map.empty[K, A])

  protected def getMap = internalMap.single.get

  def isEmpty: Boolean = getMap.isEmpty

  protected def notFound(id: K): IdNotFound

  def getByKey(key: K): Option[A] =
    internalMap.single.get.get(key)

  def getValues: Iterable[A] = getMap.values

  def getKeys: Iterable[K] = getMap.keys

  def exists(predicate: A => Boolean): Boolean = getValues.exists(predicate)

  def find(predicate: A => Boolean): Option[A] = getValues.find(predicate)
}

private object StmCatsReadWriteRepositoryImpl {

  val md = java.security.MessageDigest.getInstance("SHA-1")

}

/** A read/write wrapper around an STM Ref of a map.
 *
 * Used by processor actors.
 */
abstract class StmCatsReadWriteRepositoryImpl[K, A](keyGetter: A => K)
    extends StmCatsReadRepositoryImpl[K, A](keyGetter) with CatsReadWriteRepository[K, A] {

  def init(): Unit = removeAll

  protected def nextIdentityAsString: String =
    // ensure all IDs can be used in URLs
    Slug.slugify(
      play.api.libs.Codecs
        .sha1(StmReadWriteRepositoryImpl.md.digest(java.util.UUID.randomUUID.toString.getBytes))
    )

  def put(value: A): Unit =
    internalMap.single.transform(map => map + (keyGetter(value) -> value))

  def remove(value: A): Unit =
    internalMap.single.transform(map => map - keyGetter(value))

  def removeAll(): Unit =
    internalMap.single.transform(map => map.empty)

}

abstract class StmCatsReadWriteRepositoryImplWithSlug[K, A <: HasSlug](keyGetter: A => K)
    extends StmCatsReadWriteRepositoryImpl[K, A](keyGetter) {

  protected def slugNotFound(slug: Slug): EntityCriteriaNotFound

  def uniqueSlug(origSlug: Slug): Slug = {
    val slugRegex = s"^${origSlug}(-[0-9]+)?$$".r
    val count = internalMap.single.get.values.filter { v =>
      slugRegex.findFirstIn(v.slug.id) != None
    }.size
    if (count <= 0) origSlug
    else Slug(s"${origSlug.id}-$count")
  }

  def uniqueSlugFromStr(strSlug: String): Slug =
    uniqueSlug(Slug(strSlug))

  def getBySlug(slug: Slug): Option[A] =
    internalMap.single.get.values.find(_.slug == slug)

}
