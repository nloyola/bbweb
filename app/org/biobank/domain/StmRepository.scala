package org.biobank.domain

import com.github.ghik.silencer.silent
import org.biobank.CommonValidations.EntityCriteriaNotFound
import scala.concurrent.stm.Ref
import scalaz.Scalaz._

/**
 * A read-only wrapper around an STM Ref of a Map.
 */
@silent abstract class StmReadRepositoryImpl[K, A](keyGetter: (A) => K) extends ReadRepository[K, A] {
  import org.biobank.CommonValidations._

  protected val internalMap: Ref[Map[K, A]] = Ref(Map.empty[K, A])

  protected def getMap = internalMap.single.get

  def isEmpty: Boolean = getMap.isEmpty

  protected def notFound(id: K): IdNotFound

  def getByKey(key: K): DomainValidation[A] =
    internalMap.single.get.get(key).toSuccessNel(notFound(key).toString)

  def getValues: Iterable[A] = getMap.values

  def getKeys: Iterable[K] = getMap.keys

  def exists(predicate: A => Boolean): Boolean = getValues.exists(predicate)
}

private object StmReadWriteRepositoryImpl {

  val md = java.security.MessageDigest.getInstance("SHA-1")

}

/** A read/write wrapper around an STM Ref of a map.
 *
 * Used by processor actors.
 */
abstract private[domain] class StmReadWriteRepositoryImpl[K, A](keyGetter: (A) => K)
    extends StmReadRepositoryImpl[K, A](keyGetter) with ReadWriteRepository[K, A] {

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

abstract private[domain] class StmReadWriteRepositoryImplWithSlug[
    K,
    A <: ConcurrencySafeEntity[K] with HasSlug
  ](keyGetter: (A) => K)
    extends StmReadWriteRepositoryImpl[K, A](keyGetter) {

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

  def getBySlug(slug: Slug): DomainValidation[A] =
    internalMap.single.get.find(_._2.slug == slug).map(_._2).toSuccessNel(slugNotFound(slug).toString)

}
