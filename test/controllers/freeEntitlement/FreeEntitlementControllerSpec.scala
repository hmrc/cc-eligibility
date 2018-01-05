/*
 * Copyright 2018 HM Revenue & Customs
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

package controllers.freeEntitlement

import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader
import eligibility.FreeEntitlementEligibility
import models.input.freeEntitlement.FreeEntitlementEligibilityInput
import models.output.freeEntitlement.{FifteenHoursEligibilityModel, ThirtyHoursEligibilityModel}
import org.mockito.Matchers.any
import org.mockito.Mockito.when
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.OneAppPerSuite
import play.api.http.Status.{NOT_FOUND, OK}
import play.api.libs.json.{JsValue, Json}
import play.api.test.FakeRequest
import play.api.test.Helpers.{POST, route, _}
import service.AuditEvents
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.Future

class FreeEntitlementControllerSpec extends UnitSpec with OneAppPerSuite with MockitoSugar {

  "fifteenHours" should {

    "not return NOT_FOUND endpoint" in {
      val result = route(app, FakeRequest(POST, "/cc-eligibility/fifteen-hours-entitlement/eligibility"))
      result.isDefined shouldBe true
      status(result.get) should not be NOT_FOUND
    }

    "return Bad Request if invalid data is sent" in {
      val testController = new FreeEntitlementController {
        override val freeHoursService = mock[FreeEntitlementEligibility]
        override val auditEvent: AuditEvents = mock[AuditEvents]
      }

      val inputJson = Json.obj()
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(
        testController.freeHoursService.fifteenHours(any())
      ).thenReturn(
        Future.successful(FifteenHoursEligibilityModel())
      )

      val result = await(testController.fifteenHours(request))
      status(result) shouldBe BAD_REQUEST
    }

    "accept valid request" in {
      val testController = new FreeEntitlementController {
        override val freeHoursService = mock[FreeEntitlementEligibility]
        override val auditEvent: AuditEvents = mock[AuditEvents]
      }

      val inputJson = Json.toJson(FreeEntitlementEligibilityInput("england", List()))
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(
        testController.freeHoursService.fifteenHours(any())
      ).thenReturn(
        Future.successful(FifteenHoursEligibilityModel())
      )

      val result = await(testController.fifteenHours(request))
      status(result) shouldBe OK
    }

    "return InternalServer error if exception is thrown" in {
      val testController = new FreeEntitlementController {
        override val freeHoursService = mock[FreeEntitlementEligibility]
        override val auditEvent: AuditEvents = mock[AuditEvents]
      }

      val inputJson = Json.toJson(FreeEntitlementEligibilityInput("england", List()))
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(
        testController.freeHoursService.fifteenHours(any())
      ).thenReturn(
        Future.failed(new RuntimeException)
      )

      val result = await(testController.fifteenHours(request))
      status(result) shouldBe INTERNAL_SERVER_ERROR
    }

  }

  "thirtyHours" should {

    "not return NOT_FOUND endpoint" in {
      val result = route(app, FakeRequest(POST, "/cc-eligibility/thirty-hours-entitlement/eligibility"))
      result.isDefined shouldBe true
      status(result.get) should not be NOT_FOUND
    }

    "return Bad Request if invalid data is sent" in {
      val testController = new FreeEntitlementController {
        override val freeHoursService = mock[FreeEntitlementEligibility]
        override val auditEvent: AuditEvents = mock[AuditEvents]
      }

      val inputJson: JsValue = Json.obj()
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(
        testController.freeHoursService.thirtyHours(any())(any(), any())
      ).thenReturn(
        Future.successful(ThirtyHoursEligibilityModel(true, true))
      )

      val result = await(testController.thirtyHours(request))
      status(result) shouldBe BAD_REQUEST
    }

    "accept valid request" in {
      val testController = new FreeEntitlementController {
        override val freeHoursService = mock[FreeEntitlementEligibility]
        override val auditEvent: AuditEvents = mock[AuditEvents]
      }

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_1.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(
        testController.freeHoursService.thirtyHours(any())(any(), any())
      ).thenReturn(
        Future.successful(ThirtyHoursEligibilityModel(true, true))
      )

      val result = await(testController.thirtyHours(request))
      status(result) shouldBe OK
    }

    "return InternalServer error if exception is thrown" in {
      val testController = new FreeEntitlementController {
        override val freeHoursService = mock[FreeEntitlementEligibility]
        override val auditEvent: AuditEvents = mock[AuditEvents]
      }

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_1.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(
        testController.freeHoursService.thirtyHours(any())(any(), any())
      ).thenReturn(
        Future.failed(new RuntimeException)
      )

      val result = await(testController.thirtyHours(request))
      status(result) shouldBe INTERNAL_SERVER_ERROR
    }

  }
}
