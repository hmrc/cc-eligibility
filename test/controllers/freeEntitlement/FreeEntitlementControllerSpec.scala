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

package controllers.freeEntitlement

import eligibility.FreeEntitlementService
import models.input.freeEntitlement.FreeEntitlementPayload
import models.output.freeEntitlement.FreeEntitlementPageModel
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.OneAppPerSuite
import play.api.test.FakeRequest
import play.api.http.Status.{NOT_FOUND, OK}
import play.api.test.Helpers.{POST, route}
import uk.gov.hmrc.play.test.UnitSpec
import play.api.test.Helpers._
import service.AuditEvents
import org.mockito.Matchers.any
import org.mockito.Mockito.when
import play.api.libs.json.Json

import scala.concurrent.Future

class FreeEntitlementControllerSpec extends UnitSpec with OneAppPerSuite with MockitoSugar {

  "FreeEntitlementController" should {

    "not return NOT_FOUND endpoint" in {
      val result = route(app, FakeRequest(POST, "/cc-eligibility/free-entitlement/eligibility"))
      result.isDefined shouldBe true
      status(result.get) should not be NOT_FOUND
    }

    "accept valid json request" in {

      val testController = new FreeEntitlementController {

        override val freeHoursService = mock[FreeEntitlementService]

        override val auditEvent: AuditEvents = mock[AuditEvents]
      }

      val inputJson = Json.toJson(FreeEntitlementPayload("england", List()))
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(testController.freeHoursService.eligibility(any())).thenReturn(Future.successful(FreeEntitlementPageModel()))

      val result = await(testController.eligible(request))
      status(result) shouldBe OK
    }

    "throw bad request exception given invalid json request" in {

      val testController = new FreeEntitlementController {

        override val freeHoursService = mock[FreeEntitlementService]

        override val auditEvent: AuditEvents = mock[AuditEvents]
      }

      val inputJson = Json.toJson("Payload")
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(testController.freeHoursService.eligibility(any())).thenReturn(Future.successful(FreeEntitlementPageModel()))

      val result = await(testController.eligible(request))
      status(result) shouldBe BAD_REQUEST
    }
  }
}
