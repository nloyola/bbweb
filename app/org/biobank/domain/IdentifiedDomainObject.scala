package org.biobank.domain

import play.api.libs.json._

/**
 * An object with a unique id.
 */
@SuppressWarnings(Array("org.wartremover.warts.ToString"))
trait IdentifiedDomainObject[T] {

  /** The unique ID for this object. */
  val id: T

  override def equals(that: Any): Boolean = that match {
    case that: IdentifiedDomainObject[_] => this.id == that.id
    case _ => false
  }

  override def hashCode: Int = this.id.hashCode + 41

  override def toString: String = id.toString
}

object IdentifiedDomainObject {

  // Do not want JSON to create a sub object, we just want it to be converted
  // to a single string
  implicit val identifiedValueObjectIdWrite: Writes[IdentifiedDomainObject[String]] =
    Writes { (id: IdentifiedDomainObject[String]) =>
      JsString(id.id)
    }

  implicit val maybeIdentifiedValueObjectIdWrite: Writes[Option[IdentifiedDomainObject[String]]] =
    Writes { (id: Option[IdentifiedDomainObject[String]]) =>
      id match {
        case Some(id) => JsString(id.id)
        case None     => JsNull
      }
    }

}
