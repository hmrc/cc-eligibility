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
import models.input.tc.{Request, TCEligibilityInput}
import org.mockito.Matchers.{eq => mockEq}
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import play.api.http.Status
import play.api.libs.json.{JsValue, Json}
import play.api.test.FakeRequest
import service.AuditEvents
import spec.CCSpecConfig

import scala.concurrent.Future

/**
* Created by adamconder on 11/09/15.
*/
class TCScenarioSpec extends CCSpecConfig with FakeCCEligibilityApplication with MockitoSugar {

  "Scenarios" should {

    "(Scenario 1)(1 claimants, 1 children)(No elements) determine what elements the scenario receives" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController {
        override val tcEligibility = mock[TCEligibility]
        override val auditEvent = mock[AuditEvents]
      }

      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_1.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[TCEligibilityInput]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.tcEligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_1.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 2) (1 claimant working, 1 qualifying Child)" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val tcEligibility = mock[TCEligibility]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_2.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[TCEligibilityInput]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.tcEligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_2.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 3) (1 claimant, no children) determine what elements the scenario receives" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val tcEligibility = mock[TCEligibility]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_3.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[TCEligibilityInput]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.tcEligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_3.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 4) (1 claimant working disabled, severely disabled without income, 1 qualifying Child non disabled)" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val tcEligibility = mock[TCEligibility]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_12.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[TCEligibilityInput]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.tcEligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))
      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_12.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 5)(Basic element, 30 hours, 1 claimant, disabled and severely disabled)(Single Tax Year) determine what elements the scenario receives" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val tcEligibility = mock[TCEligibility]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_5.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[TCEligibilityInput]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.tcEligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_5.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 6) (1 claimant working disabled, 1 qualifying Child disabled)" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val tcEligibility = mock[TCEligibility]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_6.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_6.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 7)(2 claimants, 0 children)(No elements) determine what elements the scenario receives" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val tcEligibility = mock[TCEligibility]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_7.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_7.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 8) (2 claimants one working, one child turns 15 years old before claim date) determine what elements the scenario receives" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility : TCEligibilityService = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_8.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_8.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 9) (2 claimants one working, one child 3 years old) determine what elements the scenario receives" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility : TCEligibilityService = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_9.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_9.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 10) (1 claimant working, 1 child in education) determine what elements the scenario receives" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility : TCEligibilityService = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_10.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_10.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 11)(Basic Element, 30 hours, 1 claimant)(Single Tax Year) determine what elements the scenario receives" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility : TCEligibilityService = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_11.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_11.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 12) (1 claimant working disabled, severely disabled without income, 1 qualifying Child)" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility : TCEligibilityService = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_12.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_12.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 14)(2 claimants, 2 children)(Basic, 30 hours, childcare, second adult, family elements) determine what elements the scenario receives" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility : TCEligibilityService = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_14.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_14.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 15) (1 claimant working disabled, 2 qualifying Child - 1 turning 16 and disabled)" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility : TCEligibilityService = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_15.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_15.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 16)(Basic element, child, childcare)(Split periods)(Multiple tax years) determine what elements the scenario receives" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility : TCEligibilityService = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_16.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_16.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 17) (1 claimant working, 1 child in education) determine what elements the scenario receives" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility : TCEligibilityService = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_17.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_17.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 18)(1 Claimant, Basic, 30 hour, childcare, family, one child in higher education) determine what elements the scenario receives" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility : TCEligibilityService = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_18.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_18.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 20)(multiple tax year, 1 claimant, 2 children in education) determine what elements the scenario receives" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility : TCEligibilityService = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_20.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_20.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 21)(Basic element, child, childcare)(Split periods)(Multiple tax years) determine what elements the scenario receives" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility : TCEligibilityService = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_21.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_21.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 22)(multiple tax year, 1 claimant, 1 child not in education) determine what elements the scenario receives" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility : TCEligibilityService = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_22.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_22.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 23)(multiple tax year, 1 claimant, 1 child future dob) determine what elements the scenario receives" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility : TCEligibilityService = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_23.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_23.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 24) (one claimant, one child turning 15, one child yet to be born) determine what elements the scenario receives" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility : TCEligibilityService = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_24.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_24.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 26)(single tax year, 2 claimants, 2 childrens, one future dob) determine what elements the scenario receives" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility : TCEligibilityService = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_26.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_26.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 27)(No basic element) determine what elements the scenario receives" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility : TCEligibilityService = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_27.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_27.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 28)(2 claimants, 2 children)(family, 30 hours element) determine what elements the scenario receives" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility : TCEligibilityService = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_28.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)

      val request = inputJson.validate[Request]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_28.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 29)(1 claimants, 2 children)(Basic, 30 hours, childcare, second adult, family elements)(2 TY, 4 periods)(claimants hours change during second tax year) determine what elements the scenario receives" in {
      // mock out the eligibility service
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility : TCEligibilityService = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }
      // load input resource
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_29.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      // get a real result from eligibility
      val eligible = TCEligibility.eligibility.eligibility(request.get)

      // fake request to the controller through the API endpoint
      when(controller.eligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      // load output resource
      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tc/scenario_29.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      // check results
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

  }

}
