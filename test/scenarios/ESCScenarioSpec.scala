/*
 * Copyright 2016 HM Revenue & Customs
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
import controllers.esc.ESCEligibilityController
import eligibility.ESCEligibility
import helper.JsonRequestHelper._
import models.input.esc.Request
import org.mockito.Matchers.{eq => mockEq}
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import play.api.Logger
import play.api.http.Status
import play.api.libs.json.{JsValue, Json}
import play.api.test.FakeRequest
import service.AuditEvents
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.Future

class ESCScenarioSpec extends UnitSpec with FakeCCEligibilityApplication with MockitoSugar {

  val mockESCEligibilityController = new ESCEligibilityController with ESCEligibility {
    override val eligibility: ESCEligibilityService = mock[ESCEligibilityService]
    override val auditEvent = mock[AuditEvents]
  }

  "ESC Scenarios" should {

    "(Scenario 1) determine the periods within tax years" in {
      val controller = mockESCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/esc/scenario_1.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = ESCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(executeAction(controller.eligible, FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json"), inputJson.toString))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/esc/scenario_1.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 2) determine the periods within tax years" in {
      val controller = mockESCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/esc/scenario_2.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = ESCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(executeAction(controller.eligible, FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json"), inputJson.toString))

            val outputResource: JsonNode = JsonLoader.fromResource("/json/output/esc/scenario_2.json")
            val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 3) determine the periods within tax years" in {
      val controller = mockESCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/esc/scenario_3.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = ESCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(executeAction(controller.eligible, FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json"), inputJson.toString))

            val outputResource: JsonNode = JsonLoader.fromResource("/json/output/esc/scenario_3.json")
            val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 4) determine the periods within tax years" in {
      val controller = mockESCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/esc/scenario_4.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = ESCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(executeAction(controller.eligible, FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json"), inputJson.toString))

            val outputResource: JsonNode = JsonLoader.fromResource("/json/output/esc/scenario_4.json")
            val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 5) determine the periods within tax years" in {
      val controller = mockESCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/esc/scenario_5.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = ESCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(executeAction(controller.eligible, FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json"), inputJson.toString))

            val outputResource: JsonNode = JsonLoader.fromResource("/json/output/esc/scenario_5.json")
            val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 6) determine the periods within tax years" in {
      val controller = mockESCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/esc/scenario_6.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = ESCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(executeAction(controller.eligible, FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json"), inputJson.toString))

            val outputResource: JsonNode = JsonLoader.fromResource("/json/output/esc/scenario_6.json")
            val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 7) determine the periods within tax years" in {
      val controller = mockESCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/esc/scenario_7.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = ESCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(executeAction(controller.eligible, FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json"), inputJson.toString))

            val outputResource: JsonNode = JsonLoader.fromResource("/json/output/esc/scenario_7.json")
            val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 8) determine the periods within tax years" in {
      val controller = mockESCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/esc/scenario_8.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = ESCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(executeAction(controller.eligible, FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json"), inputJson.toString))

            val outputResource: JsonNode = JsonLoader.fromResource("/json/output/esc/scenario_8.json")
            val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 9) determine the periods within tax years" in {
      val controller = mockESCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/esc/scenario_9.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = ESCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(executeAction(controller.eligible, FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json"), inputJson.toString))

            val outputResource: JsonNode = JsonLoader.fromResource("/json/output/esc/scenario_9.json")
            val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 10) determine the periods within tax years" in {
      val controller = mockESCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/esc/scenario_10.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = ESCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(executeAction(controller.eligible, FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json"), inputJson.toString))

            val outputResource: JsonNode = JsonLoader.fromResource("/json/output/esc/scenario_10.json")
            val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 11) determine the periods within tax years" in {
      val controller = mockESCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/esc/scenario_11.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = ESCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(executeAction(controller.eligible, FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json"), inputJson.toString))

            val outputResource: JsonNode = JsonLoader.fromResource("/json/output/esc/scenario_11.json")
            val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 12) determine the periods within tax years" in {
      val controller = mockESCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/esc/scenario_12.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = ESCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(executeAction(controller.eligible, FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json"), inputJson.toString))

            val outputResource: JsonNode = JsonLoader.fromResource("/json/output/esc/scenario_12.json")
            val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 13) determine the periods within tax years" in {
      val controller = mockESCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/esc/scenario_13.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = ESCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(executeAction(controller.eligible, FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json"), inputJson.toString))

            val outputResource: JsonNode = JsonLoader.fromResource("/json/output/esc/scenario_13.json")
            val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 14) determine the periods within tax years" in {
      val controller = mockESCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/esc/scenario_14.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = ESCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(executeAction(controller.eligible, FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json"), inputJson.toString))

            val outputResource: JsonNode = JsonLoader.fromResource("/json/output/esc/scenario_14.json")
            val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 15) determine the periods within tax years" in {
      val controller = mockESCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/esc/scenario_15.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = ESCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(executeAction(controller.eligible, FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json"), inputJson.toString))

            val outputResource: JsonNode = JsonLoader.fromResource("/json/output/esc/scenario_15.json")
            val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 16) determine the periods within tax years" in {
      val controller = mockESCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/esc/scenario_16.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]
      val eligible = ESCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(executeAction(controller.eligible, FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json"), inputJson.toString))

            val outputResource: JsonNode = JsonLoader.fromResource("/json/output/esc/scenario_16.json")
            val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 17) determine the periods within tax years" in {
      val controller = mockESCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/esc/scenario_17.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = ESCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(executeAction(controller.eligible, FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json"), inputJson.toString))

            val outputResource: JsonNode = JsonLoader.fromResource("/json/output/esc/scenario_17.json")
            val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 18) determine the periods within tax years" in {
      val controller = mockESCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/esc/scenario_18.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = ESCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(executeAction(controller.eligible, FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json"), inputJson.toString))

            val outputResource: JsonNode = JsonLoader.fromResource("/json/output/esc/scenario_18.json")
            val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 19) determine the periods within tax years" in {
      val controller = mockESCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/esc/scenario_19.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = ESCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(executeAction(controller.eligible, FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json"), inputJson.toString))

            val outputResource: JsonNode = JsonLoader.fromResource("/json/output/esc/scenario_19.json")
            val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 20) determine the periods within tax years" in {
      val controller = mockESCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/esc/scenario_20.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = ESCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(executeAction(controller.eligible, FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json"), inputJson.toString))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/esc/scenario_20.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }


  }
}
