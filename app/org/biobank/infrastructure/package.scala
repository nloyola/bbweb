package org.biobank

import scalaz._
import scalaz.Scalaz._

package infrastructure {

  sealed trait SortOrder
  case object AscendingOrder extends SortOrder
  case object DescendingOrder extends SortOrder

  object SortOrder {

    def fromString(order: String): ValidationNel[String, SortOrder] = {
      order match {
        case "asc" | "desc" =>
          val so: SortOrder = order match {
            case "asc"  => AscendingOrder
            case "desc" => DescendingOrder
          }
          so.successNel[String]
        case _ =>
          s"invalid order requested: $order".failureNel[SortOrder]
      }
    }

  }

  object Util {

    def toInt(s: String): Option[Int] =
      try {
        Some(s.toInt)
      } catch {
        case _: java.lang.NumberFormatException => None
      }
  }
}
