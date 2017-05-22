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
import controllers.tfc.TFCEligibilityController
import eligibility.TFCEligibility
import models.input.tfc.Request
import org.mockito.Matchers.{any, eq => mockEq}
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import play.api.http.Status
import play.api.libs.json.{JsValue, Json}
import play.api.test.FakeRequest
import service.AuditEvents
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.Future

class TFCScenarioSpec extends UnitSpec with FakeCCEligibilityApplication with MockitoSugar {

  val mockTFCEligibilityController = new TFCEligibilityController with TFCEligibility {
    override val eligibility: TFCEligibilityService = mock[TFCEligibilityService]
    override val auditEvent = mock[AuditEvents]
  }

  "TFC Scenarios" should {
    implicit val req = FakeRequest()
    implicit val hc = new HeaderCarrier()

    "(Scenario 1) single parent with failed eligibility and qualifying child" in {
      val controller = mockTFCEligibilityController
      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_1.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]
      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_1.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 2) single parent live or works in UK but fails eligibility and qualifying child" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_2.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_2.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 3) single parent live or works in UK has over 16 hrs work but fails eligibility and qualifying child" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_3.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_3.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 4) single parent live or works in UK and earns below 100000 but fails eligibility and qualifying child" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_4.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_4.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 5) single parent earns below 100000 works over 16 hrs with qualifying disabled child" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_5.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_5.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 6) single parent passes eligibility and qualifying child" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_6.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_6.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 7) single parent passes eligibility but child doesn't qualify" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_7.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_7.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 8) single parent passes eligibility(lives in UK, works more than 16 hrs and income below 100000) and qualifying child" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_8.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_8.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 9) single parent passes eligibility(lives in UK, works more than 16 hrs and income below 100000) and qualifying disabled child" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_9.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_9.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 10) single parent passes eligibility(lives in UK, works more than 16 hrs and income below 100000) and qualifying child and one to be born" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_10.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_10.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }
    
    "(Scenario 11)(Single Parent) Qualifying claimant and household with a child yet to be born" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_11.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_11.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 12)(Couple) Claimant and partner not qualifying, one qualifying child" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_12.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_12.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 13)(Couple) Claimant qualifying, partner not qualifying, one qualifying child" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_13.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_13.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 14)(Couple) Claimant and partner both qualifying, partner claiming other schemes, one qualifying child" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_14.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_14.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 15)(Couple) Claimant and partner both qualifying, one qualifying child" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_15.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_15.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 16)(Couple) Claimant and partner both qualifying, one qualifying child, one yet to be born" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_16.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_16.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 17)(Couple) Claimant one does not qualify and the partner qualifies, one qualifying child, one yet to be born" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_17.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_17.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 18)(Couple) Claimant does qualify and the partner does not qualify, one qualifying child, one yet to be born, one disabled child not qualifying" in {

      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_18.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_18.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK

      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 19)(Couple) Claimant and the partner both qualify, one qualifying child, one yet to be born, one disabled child not qualifying" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_19.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_19.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 20)(Couple) Claimant and the partner both do not qualify, one qualifying child, one yet to be born, one disabled child not qualifying" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_20.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_20.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK

      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 21)(Single Parent)(4 Periods) Claimant qualifies in all periods, child drops out in third period" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_21.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_21.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK

      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 22)(Couple)(4 Periods) Both Claimant and Partner qualify in all periods, one qualifying child in all periods, one born in the third period" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_22.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_22.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK

      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 23)(Single Parent)(4 Periods) Claimant qualifies in all periods, one disabled child drops out in third period, one born in third period" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_23.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_23.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK

      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 24)(Couple)(4 Periods) Claimant does not qualify partner qualifies but claiming other schemes, child qualifies throughout" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_24.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_24.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK

      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 25)(Couple)(4 Periods) Claimant and partner both qualify throughout, child drops out in fourth period" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_25.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_25.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK

      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 26)(Single Parent)(4 Periods) Claimant qualifies throughout, two children drop out in third period" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_26.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_26.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK

      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 27)(Couple)(4 Periods) Claimant and partner qualify throughout, two disabled children qualify throughout" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_27.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_27.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK

      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 28)(Couple)(4 Periods) Claimant and partner qualify throughout, one child drops out in third, one child born in fourth period" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_28.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_28.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK

      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 29)(Couple)(4 Periods) Claimant and partner qualify throughout, one disabled child drops out in second period, one child born in third period" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_29.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_29.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK

      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 30)(Couple)(4 Periods) Claimant and partner qualify throughout, two children drop out in second period, one child born in fourth period" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_30.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_30.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK

      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 31) - 2 claimants - 3 children - 4 periods" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_31.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_31.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 32) - 2 claimants - 2 children - 4 periods" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_32.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_32.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 33) - 2 claimants - 1 claimant claiming other scheme - 3 children - 4 periods" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_33.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_33.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 34) - 2 claimants - 2 children - 8 periods" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_34.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_34.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 35) - 2 claimants - 2 children - 6 periods" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_35.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_35.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 36) - 2 claimants- both claimants claiming other scheme - 2 children - 6 periods" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_36.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_36.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 37) - 1 claimant - 3 children - 6 periods" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_37.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_37.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 38) - 2 claimants - 3 children - 2 children droput in 2 different periods - 8 periods" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_38.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_38.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 39) - 2 claimants - 4 children - 8 periods" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_39.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_39.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 40) - 2 claimants - 3 children - 8 periods" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_40.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_40.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)
      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 41) single parent qualifying eligibility and qualifying child" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_41.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_41.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 42) single parent not qualifying eligibility and qualifying child" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_42.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_42.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "(Scenario 43)(Couple) Claimant and partner qualifying, one qualifying child" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_43.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_43.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }
    "(Scenario 43)(Couple) Claimant and partner not qualifying, one qualifying child" in {
      val controller = mockTFCEligibilityController

      val inputResource: JsonNode = JsonLoader.fromResource("/json/input/tfc/scenario_44.json")
      val inputJson: JsValue = Json.parse(inputResource.toString)
      val request = inputJson.validate[Request]

      val eligible = TFCEligibility.eligibility.eligibility(request.get)

      when(controller.eligibility.eligibility(mockEq(request.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligible))
      val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

      val outputResource: JsonNode = JsonLoader.fromResource("/json/output/tfc/scenario_44.json")
      val outputJson: JsValue = Json.parse(outputResource.toString)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

  }
}
