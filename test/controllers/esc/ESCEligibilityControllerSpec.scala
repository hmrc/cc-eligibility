/*
 * Copyright 2020 HM Revenue & Customs
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

package controllers.esc

import com.github.fge.jackson.JsonLoader
import controllers.FakeCCEligibilityApplication
import eligibility.ESCEligibility
import models.input.esc.ESCEligibilityInput
import models.output.esc.ESCEligibilityOutput
import org.joda.time.LocalDate
import org.mockito.ArgumentMatchers.{eq => mockEq, _}
import org.mockito.Mockito._
import play.api.http.Status
import play.api.libs.json.Json
import play.api.test.FakeRequest
import service.AuditEvents

import scala.concurrent.Future

class ESCEligibilityControllerSpec extends FakeCCEligibilityApplication {

  val mockESC: ESCEligibility = mock[ESCEligibility]

  "ESCEligibilityController" should {
    val controller = new ESCEligibilityController (
      mockESC,
      mock[AuditEvents],
      mockCC
    )

    "Accept valid json should return Json body" in {
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/esc/eligibility_input_test.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(mockESC.eligibility(any[ESCEligibilityInput]())).thenReturn(Future.successful(ESCEligibilityOutput(Nil)))
      val result = await(controller.eligible(request))
      status(result) shouldBe Status.OK
    }

    "Accept invalid json schema (tax year), should return Bad request" in {
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/esc/invalid_tax_year.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(mockESC.eligibility(any[ESCEligibilityInput]())).thenReturn(Future.successful(ESCEligibilityOutput(Nil)))

      val result = await(controller.eligible(request))
      status(result) shouldBe Status.BAD_REQUEST
    }

    "Accept invalid json with incorrect until date format json should return a Bad request" in {
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/esc/incorrect_date_format.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(mockESC.eligibility(any[ESCEligibilityInput]())).thenReturn(Future.successful(ESCEligibilityOutput(Nil)))

      val result = await(controller.eligible(request))
      status(result) shouldBe Status.BAD_REQUEST
    }

    "Accept invalid json if child id has negative value should return 400" in {
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/esc/negative_child_id.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(mockESC.eligibility(any[ESCEligibilityInput]())).thenReturn(Future.successful(ESCEligibilityOutput(Nil)))

      val result = await(controller.eligible(request))
      status(result) shouldBe 400
    }

    "Accept a valid json if number of claimant/s less than 1 should return Bad request" in {
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/esc/no_claimants.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(mockESC.eligibility(any[ESCEligibilityInput]())).thenReturn(Future.successful(ESCEligibilityOutput(Nil)))

      val result = await(controller.eligible(request))
      status(result) shouldBe 400
    }

    "Accept a valid json if number of children more than 25 should return Bad request" in {
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/esc/invalid_no_of_children.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(mockESC.eligibility(any[ESCEligibilityInput]())).thenReturn(Future.successful(ESCEligibilityOutput(Nil)))

      val result = await(controller.eligible(request))
      status(result) shouldBe 400
    }

    "Accept valid json scenario and return a valid response (ESC start date provided)" in {
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/esc/eligibility_input_test.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)
      val JsonResult = inputJson.validate[ESCEligibilityInput]
      val eligibilityResult = app.injector.instanceOf[ESCEligibility].eligibility(JsonResult.get)

      when(mockESC.eligibility(mockEq(JsonResult.get))).thenReturn(Future.successful(eligibilityResult))

      val result = await(controller.eligible(request))

      val outputJson = Json.parse(
        s"""
        {
          "taxYears": [
              {
                  "from": "2016-08-27",
                  "until": "2017-04-06",
                  "periods": [
                      {
                          "from": "2016-08-27",
                          "until": "2017-04-06",
                          "claimants": [
                              {
                                  "qualifying": false,
                                  "isPartner": false,
                                  "eligibleMonthsInPeriod": 0,
                                  "vouchers": false,
                                  "escStartDate":"${LocalDate.now().toString()}"
                              }
                          ],
                          "children":[
                              {
                                  "qualifying":true,
                                  "childCareCost":100,
                                  "childCareCostPeriod":"Week"
                              }
                          ]
                      }
                  ]
              }
          ],
          "eligibility":false,
          "parentEligibility":false,
          "partnerEligibility":false
        }
        """.stripMargin)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "Return Internal Server Error with error message if an exception is thrown during eligibility" in {
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/esc/eligibility_input_test.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)
      val JsonResult = inputJson.validate[ESCEligibilityInput]

      when(mockESC.eligibility(mockEq(JsonResult.get))).thenReturn(Future.failed(new Exception("Something bad happened in Eligibility")))

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
