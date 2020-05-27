package org.biobank.controllers

import org.biobank.fixtures._
import play.api.test.Helpers._

/**
 * Common code for REST APIs that uses paged results.
 */
trait PagedResultsSharedSpec { this: ControllerFixture =>

  import org.scalatest.matchers.must.Matchers._

  /**
   * Common behaviour for paged queries.
   */
  def pagedQueryShouldFailSharedBehaviour(setupFunc: () => Url) = {

    it("fail with a negative page number") {
      val url  = setupFunc()
      val resp = makeAuthRequest(GET, url.addQueryString("page=-1", "limit=1"))
      resp.value must beBadRequestWithMessage("page is invalid")
    }

    it("fail with an invalid page number") {
      val url  = setupFunc()
      val resp = makeAuthRequest(GET, url.addQueryString("page=100000000"))
      resp.value must beBadRequestWithMessage("page exceeds limit")
    }

    it("fail with a negative page size") {
      val url  = setupFunc()
      val resp = makeAuthRequest(GET, url.addQueryString("limit=-1"))
      resp.value must beBadRequestWithMessage("page size is invalid")
    }

    it("fail with an invalid page size") {
      val url  = setupFunc()
      val resp = makeAuthRequest(GET, url.addQueryString("limit=1000000"))
      resp.value must beBadRequestWithMessage("page size exceeds maximum")
    }

    it("fail with an invalid sort field") {
      val url  = setupFunc()
      val resp = makeAuthRequest(GET, url.addQueryString("sort=xyz"))
      resp.value must beBadRequestWithMessage("invalid sort field")
    }
  }

}
