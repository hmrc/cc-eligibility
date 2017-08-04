/*
 * Copyright 2017 HM Revenue & Customs
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

package controllers

import com.github.fge.jackson.JsonLoader
import models.mappings.Household
import org.mockito.Matchers.{eq => mockEq, _}
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import play.api.http.Status
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import service.{AuditEvents, EligibilityService}
import spec.CCConfigSpec

import scala.concurrent.Future

class EligibilityControllerSpec extends CCConfigSpec with FakeCCEligibilityApplication with MockitoSugar {

  implicit val request = FakeRequest()

  "EligibilityController" should {
    val controller = new EligibilityController {
      override val eligibilityService = mock[EligibilityService]
      override val auditEvent = mock[AuditEvents]
    }

    "have reference to EligibilityService" in {
      EligibilityController.eligibilityService.isInstanceOf[EligibilityService] shouldBe true
    }

    "have reference to auditEvents" in {
      EligibilityController.auditEvent.isInstanceOf[AuditEvents] shouldBe true
    }

    "not return NOT_FOUND endpoint" in {
      val result = route(app, FakeRequest(POST, "/cc-eligibility/eligibility"))
      result.isDefined shouldBe true
      status(result.get) should not be NOT_FOUND
    }

    "Accept a valid json for household " in {
      val inputJson = Json.parse(JsonLoader.fromResource("/household/eligibility_input_household.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.eligibilityService.eligibility(any[Household]())).thenReturn(Future.successful("SchemesResult"))
      val result = await(controller.eligible(request))
      status(result) shouldBe Status.OK
    }

    "return a Bad request when incorrect date format in json" in {
      val inputJson = Json.parse(JsonLoader.fromResource("/household/incorrect_date_format.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.eligibilityService.eligibility(any[Household]())).thenReturn(Future.successful("SchemesResult"))
      val result = await(controller.eligible(request))
      status(result) shouldBe Status.BAD_REQUEST
    }

    "return a Bad request when no claimant present" in {
      val inputJson = Json.parse(JsonLoader.fromResource("/household/no_claimants.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.eligibilityService.eligibility(any[Household]())).thenReturn(Future.successful("SchemesResult"))
      val result = await(controller.eligible(request))
      status(result) shouldBe 400
    }


    "Return Internal Server Error with error message if an exception is thrown during eligibility" in {
      val inputJson = Json.parse(JsonLoader.fromResource("/household/eligibility_input_household.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)
      val JsonResult = inputJson.validate[Household]

      when(controller.eligibilityService.eligibility(mockEq(JsonResult.get))).
        thenReturn(Future.failed(new Exception("Something bad happened in Eligibility")))

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
