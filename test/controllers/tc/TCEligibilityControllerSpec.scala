/*
 * Copyright 2019 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package controllers.tc

import com.github.fge.jackson.JsonLoader
import controllers.FakeCCEligibilityApplication
import eligibility.TCEligibility
import models.input.tc.TCEligibilityInput
import models.output.tc.TCEligibilityOutput
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import play.api.http.Status
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import service.AuditEvents

import scala.concurrent.Future

class TCEligibilityControllerSpec extends FakeCCEligibilityApplication with MockitoSugar {

  implicit val request = FakeRequest()

  "TCEligibilityController" should {

    "not return NOT_FOUND endpoint" in {
      val result = route(app, FakeRequest(POST, "/cc-eligibility/tax-credits/eligibility"))
      result.isDefined shouldBe true
      status(result.get) should not be NOT_FOUND
    }

    "Accept valid json and should return Json body" in {

      val controller = new TCEligibilityController {
        override val tcEligibility = mock[TCEligibility]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/eligibility_input_test.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.tcEligibility.eligibility(any[TCEligibilityInput]())).thenReturn(Future.successful(TCEligibilityOutput(taxYears = Nil)))
      val result = await(controller.eligible(request))
      status(result) shouldBe Status.OK
    }

    "Empty tax year should return Bad request" in {
      val controller = new TCEligibilityController {
        override val tcEligibility = mock[TCEligibility]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/empty_tax_year.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.tcEligibility.eligibility(any[TCEligibilityInput]())).thenReturn(Future.successful(TCEligibilityOutput(taxYears = Nil)))
      val result = await(controller.eligible(request))
      status(result) shouldBe Status.BAD_REQUEST
    }

    "Accept invalid json schema and should return Bad request" in {
      val controller = new TCEligibilityController{
        override val tcEligibility = mock[TCEligibility]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/invalid_tax_year.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.tcEligibility.eligibility(any[TCEligibilityInput]())).thenReturn(Future.successful(TCEligibilityOutput(taxYears = Nil)))
      val result = await(controller.eligible(request))
      status(result) shouldBe Status.BAD_REQUEST
    }

    "Accept invalid json with incorrect until date format json and return a Bad request" in {
      val controller = new TCEligibilityController {
        override val tcEligibility = mock[TCEligibility]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/incorrect_date_format.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.tcEligibility.eligibility(any[TCEligibilityInput]())).thenReturn(Future.successful(TCEligibilityOutput(taxYears = Nil)))
      val result = await(controller.eligible(request))
      status(result) shouldBe Status.BAD_REQUEST
    }

    "Accept invalid json if child id has negative value should return 400" in {
      val controller = new TCEligibilityController {
        override val tcEligibility = mock[TCEligibility]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/negative_child_id.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.tcEligibility.eligibility(any[TCEligibilityInput]())).thenReturn(Future.successful(TCEligibilityOutput(taxYears = Nil)))
      val result = await(controller.eligible(request))
      status(result) shouldBe 400
    }

    "Accept a valid json if there is negative childcare cost should return 400" in {
      val controller = new TCEligibilityController {
        override val tcEligibility = mock[TCEligibility]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/negative_childcare_cost.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.tcEligibility.eligibility(any[TCEligibilityInput]())).thenReturn(Future.successful(TCEligibilityOutput(taxYears = Nil)))
      val result = await(controller.eligible(request))
      status(result) shouldBe 400
    }

    "Accept a valid json if number of claimants more than 2 should return Bad request" in {
      val controller = new TCEligibilityController {
        override val tcEligibility = mock[TCEligibility]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/invalid_claimants_3.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.tcEligibility.eligibility(any[TCEligibilityInput]())).thenReturn(Future.successful(TCEligibilityOutput(taxYears = Nil)))
      val result = await(controller.eligible(request))
      status(result) shouldBe 400
    }

    "Accept a valid json if number of claimant/s less than 1 should return Bad request" in {
      val controller = new TCEligibilityController {
        override val tcEligibility = mock[TCEligibility]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/no_claimants.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.tcEligibility.eligibility(any[TCEligibilityInput]())).thenReturn(Future.successful(TCEligibilityOutput(taxYears = Nil)))
      val result = await(controller.eligible(request))
      status(result) shouldBe 400
    }

    "Accept a valid json if number of children more than 25 should return Bad request" in {
      val controller = new TCEligibilityController {
        override val tcEligibility = mock[TCEligibility]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/invalid_no_of_children.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.tcEligibility.eligibility(any[TCEligibilityInput]())).thenReturn(Future.successful(TCEligibilityOutput(taxYears = Nil)))
      val result = await(controller.eligible(request))
      status(result) shouldBe 400
    }

    "Return Internal Server Error with error message if an exception is thrown during eligibility" in {
      val controller = new TCEligibilityController {
        override val tcEligibility = mock[TCEligibility]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/eligibility_input_test.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.tcEligibility.eligibility(any[TCEligibilityInput]())).thenReturn(Future.failed(new Exception("Something bad happened in Eligibility")))
      val result = await(controller.eligible(request))
      val outputJSON = Json.parse(
        """
          |{
          |    "status": 500,
          |    "error": "Something bad happened in Eligibility"
          |}
        """.stripMargin)

      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
      jsonBodyOf(result) shouldBe outputJSON
    }
  }

}
