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

package controllers.esc

import com.github.fge.jackson.JsonLoader
import controllers.FakeCCEligibilityApplication
import eligibility.ESCEligibility
import models.input.esc.ESCEligibilityInput
import models.output.OutputAPIModel.Eligibility
import org.mockito.Matchers.{eq => mockEq, _}
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import play.api.http.Status
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import service.AuditEvents
import spec.CCSpecConfig
import scala.concurrent.Future

class ESCEligibilityControllerSpec extends CCSpecConfig with FakeCCEligibilityApplication with MockitoSugar {

  implicit val request = FakeRequest()

  "ESCEligibilityController" should {

    "not return NOT_FOUND endpoint" in {
      val result = route(app, FakeRequest(POST, "/cc-eligibility/employer-supported-childcare/eligibility"))
      result.isDefined shouldBe true
      status(result.get) should not be NOT_FOUND
    }

    "Accept valid json should return Json body" in {
      val controller = new ESCEligibilityController {
        override val escEligibility = mock[ESCEligibility]
        override val auditEvent = mock[AuditEvents]
      }
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/esc/eligibility_input_test.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.escEligibility.eligibility(any[ESCEligibilityInput]())).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe Status.OK
    }

    "Accept invalid json schema (tax year), should return Bad request" in {
      val controller = new ESCEligibilityController {
        override val escEligibility = mock[ESCEligibility]
        override val auditEvent = mock[AuditEvents]
      }
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/esc/invalid_tax_year.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.escEligibility.eligibility(any[ESCEligibilityInput]())).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe Status.BAD_REQUEST
    }

    "Accept invalid json with incorrect until date format json should return a Bad request" in {
      val controller = new ESCEligibilityController with ESCEligibility {
        override val escEligibility = mock[ESCEligibility]
        override val auditEvent = mock[AuditEvents]
      }
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/esc/incorrect_date_format.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.escEligibility.eligibility(any[ESCEligibilityInput]())).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe Status.BAD_REQUEST
    }

    "Accept invalid json if child id has negative value should return 400" in {
      val controller = new ESCEligibilityController with ESCEligibility {
        override val escEligibility = mock[ESCEligibility]
        override val auditEvent = mock[AuditEvents]
      }
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/esc/negative_child_id.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.escEligibility.eligibility(any[ESCEligibilityInput]())).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe 400
    }

    "Accept a valid json if number of claimant/s less than 1 should return Bad request" in {
      val controller = new ESCEligibilityController with ESCEligibility {
        override val escEligibility = mock[ESCEligibility]
        override val auditEvent = mock[AuditEvents]
      }
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/esc/no_claimants.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.escEligibility.eligibility(any[ESCEligibilityInput]())).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe 400
    }

    "Accept a valid json if number of children more than 25 should return Bad request" in {
      val controller = new ESCEligibilityController with ESCEligibility {
        override val escEligibility = mock[ESCEligibility]
        override val auditEvent = mock[AuditEvents]
      }
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/esc/invalid_no_of_children.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.escEligibility.eligibility(any[ESCEligibilityInput]())).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe 400
    }

    "Accept valid json scenario and return a valid response (ESC start date provided)" in {
      val controller = new ESCEligibilityController with ESCEligibility {
        override val escEligibility = mock[ESCEligibility]
        override val auditEvent = mock[AuditEvents]
      }
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/esc/eligibility_input_test.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)
      val JsonResult = inputJson.validate[ESCEligibilityInput]
      val eligibilityResult = ESCEligibility.eligibility(JsonResult.get)

      when(controller.escEligibility.eligibility(mockEq(JsonResult.get))).thenReturn(Future.successful(eligibilityResult))
      val result = await(controller.eligible(request))

      val outputJson = Json.parse(
        s"""
        {
            "eligibility": {
                "tc": null,
                "tfc": null,
                "esc": {
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
                                            "vouchers": false
                                        }
                                    ],
                                    "children":[
                                        {
                                            "qualifying":true,
                                            "childCareCost":100,
                                            "childCareCostPeriod":"Month"
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
            }
        }
        """.stripMargin)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "Return Internal Server Error with error message if an exception is thrown during eligibility" in {
      val controller = new ESCEligibilityController with ESCEligibility {
        override val escEligibility = mock[ESCEligibility]
        override val auditEvent = mock[AuditEvents]
      }
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/esc/eligibility_input_test.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)
      val JsonResult = inputJson.validate[ESCEligibilityInput]

      when(controller.escEligibility.eligibility(mockEq(JsonResult.get))).thenReturn(Future.failed(new Exception("Something bad happened in Eligibility")))
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
