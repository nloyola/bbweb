package org.biobank.controllers

import org.biobank.fixtures.ControllerFixture
import play.api.libs.json._
import play.api.test._
import play.api.test.Helpers._

class ApplicationSpec extends ControllerFixture {

  protected val basePath = ""

  describe("Application") {

    it("send 404 on a bad request") {
      val result = route(app, FakeRequest(GET, "/xyz")).get
      status(result) mustEqual NOT_FOUND
    }

    it("return initial aggregate counts") {
      val reply = makeAuthRequest(GET, uri("dtos/counts")).value
      reply must beOkResponseWithJsonReply

      val jsonObj = (contentAsJson(reply) \ "data").as[JsObject]

      (jsonObj \ "studies").as[Int] mustBe (0)

      (jsonObj \ "centres").as[Int] mustBe (0)

      (jsonObj \ "users").as[Int] mustBe (1) // 1 for the default user
    }

    it("return correct aggregate counts") {
      studyRepository.put(factory.createDisabledStudy)
      centreRepository.put(factory.createDisabledCentre)
      userRepository.put(factory.createRegisteredUser)

      val reply = makeAuthRequest(GET, uri("dtos/counts")).value
      reply must beOkResponseWithJsonReply

      val jsonObj = (contentAsJson(reply) \ "data").as[JsObject]

      (jsonObj \ "studies").as[Int] mustBe (1)

      (jsonObj \ "centres").as[Int] mustBe (1)

      (jsonObj \ "users").as[Int] mustBe (2) // +1 for the default user
    }

  }
}
