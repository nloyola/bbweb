package org.biobank.services

import cats.implicits._
import org.biobank.validation.Validation._

final case class PagedQuery(filter: FilterString, sort: SortString, page: Int, limit: Int) {

  def validPage(totalItems: Int): ServiceValidation[Int] = {
    import scalaz.Scalaz._

    if (((totalItems > 0) && ((page - 1) * limit >= totalItems)) ||
        ((totalItems == 0) && (page > 1))) {
      ServiceError(s"page exceeds limit: $page").failureNel[Int]
    } else {
      // if totalItems is zero, but page is 1 then it is valid
      page.successNel
    }
  }

  def validPageCats(totalItems: Int): ValidationResult[Int] = {
    if (((totalItems > 0) && ((page - 1) * limit >= totalItems)) ||
        ((totalItems == 0) && (page > 1))) {
      Error(s"page exceeds limit: $page").invalidNec
    } else {
      // if totalItems is zero, but page is 1 then it is valid
      page.validNec
    }
  }
}
