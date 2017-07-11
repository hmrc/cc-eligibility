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

package scenarios

import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader
import controllers.FakeCCEligibilityApplication
import controllers.tc.TCEligibilityController
import eligibility.TCEligibility
import models.input.tc.TCEligibilityInput
import org.mockito.Matchers.{eq => mockEq}
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import play.api.http.Status
import play.api.libs.json.{JsValue, Json}
import play.api.test.FakeRequest
import service.AuditEvents
import spec.CCConfigSpec

import scala.concurrent.Future

class TCScenarioSpec extends CCConfigSpec with FakeCCEligibilityApplication with MockitoSugar {

  "Scenarios" should {

    "(Scenario 18)(1 Claimant, Basic, 30 hour, childcare, family, one child in higher education) determine what elements the scenario receives" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController {
        override val tcEligibility = mock[TCEligibility]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/multi_tax_year_multi_periods.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[TCEligibilityInput]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.tcEligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible(FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_18.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

  }

}
