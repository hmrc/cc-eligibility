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
import controllers.esc.ESCEligibilityController
import eligibility.ESCEligibility
import models.input.esc.ESCEligibilityInput
import org.mockito.Matchers.{eq => mockEq, _}
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import play.api.http.Status
import play.api.libs.json.{JsValue, Json}
import play.api.test.FakeRequest
import service.AuditEvents
import spec.CCSpecConfig
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table
import scala.concurrent.Future

class ESCScenarioSpec extends CCSpecConfig with FakeCCEligibilityApplication with MockitoSugar {

  "ESCScenarioSpec" should {
    val testData = Table(
      ("scenario", "number"),
      ("1", 1),
      ("2", 2),
      ("3", 3),
      ("4", 4),
      ("5", 5),
      ("6", 6),
      ("7", 7),
      ("8", 8),
      ("9", 9),
      ("10", 10),
      ("11", 11),
      ("12", 12),
      ("13", 13),
      ("14", 14),
      ("15", 15),
      ("16", 16),
      ("17", 17),
      ("18", 18),
      ("19", 19),
      ("20", 20)
    )

    forAll(testData) { case (scenario, number) =>

      s"(Scenario $scenario) determine the periods within tax years" in {
        val controller = new ESCEligibilityController {
          override val escEligibility = mock[ESCEligibility]
          override val auditEvent = mock[AuditEvents]
        }
        val inputResource: JsonNode = JsonLoader.fromResource(s"/json/input/esc/scenario_${number}.json")
        val inputJson: JsValue = Json.parse(inputResource.toString)
        val request = inputJson.validate[ESCEligibilityInput]

        val eligible = ESCEligibility.eligibility(request.get)

        when(controller.escEligibility.eligibility(mockEq(request.get))).thenReturn(Future.successful(eligible))
        val result = await(controller.eligible (FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)))

        val outputResource: JsonNode = JsonLoader.fromResource(s"/json/output/esc/scenario_${number}.json")
        val outputJson: JsValue = Json.parse(outputResource.toString)

        status(result) shouldBe Status.OK
        jsonBodyOf(result) shouldBe outputJson
      }
    }

  }

}
