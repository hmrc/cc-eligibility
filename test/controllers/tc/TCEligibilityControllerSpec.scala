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

package controllers.tc

import com.github.fge.jackson.JsonLoader
import controllers.FakeCCEligibilityApplication
import eligibility.TCEligibility
import models.input.tc.Request
import models.output.OutputAPIModel.Eligibility
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
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

class TCEligibilityControllerSpec extends CCSpecConfig with FakeCCEligibilityApplication with MockitoSugar {

  implicit val request = FakeRequest()

  "TCEligibilityController" should {

    "not return NOT_FOUND endpoint" in {
      val result = route(app, FakeRequest(POST, "/cc-eligibility/tax-credits/eligibility"))
      result.isDefined shouldBe true
      status(result.get) should not be NOT_FOUND
    }

    "Accept valid json and should return Json body" in {

      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/scenario_1.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.eligibility.eligibility(any[Request]())).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe Status.OK
    }

    "Empty tax year should return Bad request" in {
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/empty_tax_year.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.eligibility.eligibility(any[Request]())).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe Status.BAD_REQUEST
    }

    "Accept invalid json schema and should return Bad request" in {
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/invalid_tax_year.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.eligibility.eligibility(any[Request]())).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe Status.BAD_REQUEST
    }

    "Accept invalid json with incorrect until date format json and return a Bad request" in {
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/incorrect_date_format.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.eligibility.eligibility(any[Request]())).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe Status.BAD_REQUEST
    }

    "Accept invalid json if total income is less than 0 and return Bad request" in {
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/negative_income.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.eligibility.eligibility(any[Request]())).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe Status.BAD_REQUEST
    }

    "Accept invalid json if total previous income is less than 0 and return Bad request" in {
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/negative_previous_income.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.eligibility.eligibility(any[Request]())).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe Status.BAD_REQUEST
    }

    "Accept invalid json if child id has negative value should return 400" in {
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/negative_child_id.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.eligibility.eligibility(any[Request]())).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe 400
    }

    "Accept a valid json if there is negative childcare cost should return 400" in {
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/negative_childcare_cost.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.eligibility.eligibility(any[Request]())).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe 400
    }

    "Accept a valid json if number of claimants more than 2 should return Bad request" in {
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/invalid_claimants_3.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.eligibility.eligibility(any[Request]())).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe 400
    }

    "Accept a valid json if number of claimant/s less than 1 should return Bad request" in {
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/no_claimants.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.eligibility.eligibility(any[Request]())).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe 400
    }

    "Accept a valid json if number of children more than 25 should return Bad request" in {
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/invalid_no_of_children.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.eligibility.eligibility(any[Request]())).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe 400
    }

    "Accept json for scenario 1 and return a valid response" in {
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/scenario_1.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)
      val JsonResult = inputJson.validate[Request]
      val eligibilityResult = TCEligibility.eligibility.eligibility(JsonResult.get)

      when(controller.eligibility.eligibility(mockEq(JsonResult.get))).thenReturn(Future.successful(eligibilityResult))
      val result = await(controller.eligible(request))

      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStartDate = LocalDate.parse("2016-08-27", formatter)
      val periodEndDate = LocalDate.parse("2017-04-06", formatter)
      val outputJson = Json.parse(
        s"""
          {
            "eligibility": {
              "tc": {
                "eligible": false,
                "taxYears": [
                  {
                    "from": "${periodStartDate.toString("yyyy-MM-dd")}",
                    "until": "${periodEndDate.toString("yyyy-MM-dd")}",
                    "houseHoldIncome": 0.00,

                    "periods": [
                      {
                        "from": "${periodStartDate.toString("yyyy-MM-dd")}",
                        "until": "${periodEndDate.toString("yyyy-MM-dd")}",
                        "householdElements": {
                          "basic": false,
                          "hours30": false,
                          "childcare": false,
                          "loneParent": false,
                          "secondParent": false,
                          "family": false,
                          "wtc": true,
                          "ctc": false
                        },
                        "claimants": [
                          {
                            "qualifying": true,
                            "isPartner": false,
                            "claimantDisability": {
                              "disability": false,
                              "severeDisability": false
                            }
                          }
                        ],
                        "children": [
                          {
                            "id": 0,                            "childcareCost": 3000.00,
                            "childcareCostPeriod": "Month",
                            "qualifying": false,
                            "childElements": {
                              "child": false,
                              "youngAdult": false,
                              "disability": false,
                              "severeDisability": false,
                              "childcare": false
                            }
                          }
                        ]
                      }
                    ]
                  }
                ]
              },
              "tfc": null,
              "esc": null
            }
          }
        """.stripMargin)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "Return Internal Server Error with error message if an exception is thrown during eligibility" in {
      val controller = new TCEligibilityController with TCEligibility {
        override val eligibility = mock[TCEligibilityService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tc/scenario_1.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)
      val JsonResult = inputJson.validate[Request]

      when(controller.eligibility.eligibility(mockEq(JsonResult.get))).thenReturn(Future.failed(new Exception("Something bad happened in Eligibility")))
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
