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

package controllers.tfc

import com.github.fge.jackson.JsonLoader
import controllers.FakeCCEligibilityApplication
import eligibility.TFCEligibility
import models.input.tfc.Request
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
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Future

class TFCEligibilityControllerSpec extends CCSpecConfig with FakeCCEligibilityApplication with MockitoSugar {

  val mockTFCEligibilityController = new TFCEligibilityController with TFCEligibility {
    override val eligibility = mock[TFCEligibilityService]
    override val auditEvent = mock[AuditEvents]
  }

  implicit val request = FakeRequest()
  implicit val hc = HeaderCarrier

  "TFCEligibilityController" should {

    "not return NOT_FOUND endpoint" in {
      val result = route(app, FakeRequest(POST, "/cc-eligibility/tax-free-childcare/eligibility"))
      result.isDefined shouldBe true
      status(result.get) should not be NOT_FOUND
    }

    "accept valid json should return Json body" in {
      val controller = mockTFCEligibilityController
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tfc/eligibility_input_test.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.eligibility.eligibility(any[Request]())(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe Status.OK
    }

    "accept valid json scenario and return a valid response (TFC start date provided)" in {
      val controller = mockTFCEligibilityController
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tfc/eligibility_input_test.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)
      val JsonResult = inputJson.validate[Request]
      val eligibilityResult = TFCEligibility.eligibility.eligibility(JsonResult.get)(any[play.api.mvc.Request[_]], any[HeaderCarrier])

      when(controller.eligibility.eligibility(mockEq(JsonResult.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligibilityResult))
      val result = await(controller.eligible(request))

      val outputJson = Json.parse(
        s"""
        {
            "eligibility": {
                "tc": null,
                "esc": null,
                "tfc": {
                  "from": "2016-08-27",
                  "until": "2017-05-27",
                  "householdEligibility": true,
                   "periods": [
                    {
                      "from" : "2016-08-27",
                      "until" : "2016-11-27",
                      "periodEligibility" : true,
                      "claimants" : [
                       {
                        "qualifying" : true,
                        "isPartner" : false,
                        "failures" : []
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "name" : "Child 1",
                        "qualifying" : true,
                        "from" : "2016-08-27",
                        "until" : "2016-11-27",
                        "failures" : []
                       }
                      ]
                    },
                    {
                      "from" : "2016-11-27",
                      "until" : "2017-02-27",
                      "periodEligibility" : true,
                      "claimants" : [
                       {
                        "qualifying" : true,
                        "isPartner" : false,
                        "failures" : []
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "name" : "Child 1",
                        "qualifying" : true,
                        "from" : "2016-11-27",
                        "until" : "2017-02-27",
                        "failures" : []
                       }
                      ]
                    },
                    {
                      "from" : "2017-02-27",
                      "until" : "2017-05-27",
                      "periodEligibility" : true,
                      "claimants" : [
                       {
                        "qualifying" : true,
                        "isPartner" : false,
                        "failures" : []
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "name" : "Child 1",
                        "qualifying" : true,
                        "from" : "2017-02-27",
                        "until" : "2017-05-27",
                        "failures" : []
                       }
                      ]
                    }
                   ]
                }
            }
        }
        """.stripMargin)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "return Internal Server Error with error message if an exception is thrown during eligibility" in {
      val controller = mockTFCEligibilityController
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tfc/eligibility_input_test.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)
      val JsonResult = inputJson.validate[Request]

      when(controller.eligibility.eligibility(mockEq(JsonResult.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.failed(new Exception("Something bad happened in Eligibility")))
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

    "accept invalid json if child name exceeding 25 characters should return 400" in {
      val controller = mockTFCEligibilityController
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tfc/invalid_child_name.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.eligibility.eligibility(any[Request]())(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe 400
    }

    "accept invalid json with incorrect from date format should return a bad request" in {
      val controller = mockTFCEligibilityController
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tfc/incorrect_from_date_format.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.eligibility.eligibility(any[Request]())(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe Status.BAD_REQUEST
    }

    "accept a valid json if number of claimant/s less than 1 should return 400" in {
      val controller = mockTFCEligibilityController
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tfc/no_claimants.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.eligibility.eligibility(any[Request]())(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe 400
    }

    "accept invalid json if child id has negative value should return 400" in {
      val controller = mockTFCEligibilityController
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tfc/negative_child_id.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.eligibility.eligibility(any[Request]())(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe 400
    }

    "accept a valid json if number of children more than 25 should return bad request" in {
      val controller = mockTFCEligibilityController
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tfc/invalid_no_of_children.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.eligibility.eligibility(any[Request]())(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(Eligibility()))
      val result = await(controller.eligible(request))
      status(result) shouldBe BAD_REQUEST
    }

    "accept valid json scenario when claimant selected carer's allowance and return a valid response (TFC start date provided)" in {
      val controller = mockTFCEligibilityController
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tfc/carers_allowance_parent.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)
      val JsonResult = inputJson.validate[Request]
      val eligibilityResult = TFCEligibility.eligibility.eligibility(JsonResult.get)(any[play.api.mvc.Request[_]], any[HeaderCarrier])

      when(controller.eligibility.eligibility(mockEq(JsonResult.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligibilityResult))
      val result = await(controller.eligible(request))

      val outputJson = Json.parse(
        s"""
        {
            "eligibility": {
                "tc": null,
                "esc": null,
                "tfc": {
                  "from": "2016-08-27",
                  "until": "2017-05-27",
                  "householdEligibility": true,
                   "periods": [
                    {
                      "from" : "2016-08-27",
                      "until" : "2016-11-27",
                      "periodEligibility" : true,
                      "claimants" : [
                       {
                        "qualifying" : true,
                        "isPartner" : false,
                        "failures" : []
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true,
                        "failures" : []
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "name" : "Child 1",
                        "qualifying" : true,
                        "from" : "2016-08-27",
                        "until" : "2016-11-27",
                        "failures" : []
                       }
                      ]
                    },
                    {
                      "from" : "2016-11-27",
                      "until" : "2017-02-27",
                      "periodEligibility" : true,
                      "claimants" : [
                       {
                        "qualifying" : true,
                        "isPartner" : false,
                        "failures" : []
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true,
                        "failures" : []
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "name" : "Child 1",
                        "qualifying" : true,
                        "from" : "2016-11-27",
                        "until" : "2017-02-27",
                        "failures" : []
                       }
                      ]
                    },
                    {
                      "from" : "2017-02-27",
                      "until" : "2017-05-27",
                      "periodEligibility" : true,
                      "claimants" : [
                       {
                        "qualifying" : true,
                        "isPartner" : false,
                        "failures" : []
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true,
                        "failures" : []
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "name" : "Child 1",
                        "qualifying" : true,
                        "from" : "2017-02-27",
                        "until" : "2017-05-27",
                        "failures" : []
                       }
                      ]
                    }
                   ]
                }
            }
        }
        """.stripMargin)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "accept valid json scenario when partner selected carer's allowance and return a valid response (TFC start date provided)" in {
      val controller = mockTFCEligibilityController
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tfc/carers_allowance_partner.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)
      val JsonResult = inputJson.validate[Request]
      val eligibilityResult = TFCEligibility.eligibility.eligibility(JsonResult.get)(any[play.api.mvc.Request[_]], any[HeaderCarrier])

      when(controller.eligibility.eligibility(mockEq(JsonResult.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligibilityResult))
      val result = await(controller.eligible(request))

      val outputJson = Json.parse(
        s"""
        {
            "eligibility": {
                "tc": null,
                "esc": null,
                "tfc": {
                  "from": "2016-08-27",
                  "until": "2017-05-27",
                  "householdEligibility": true,
                   "periods": [
                    {
                      "from" : "2016-08-27",
                      "until" : "2016-11-27",
                      "periodEligibility" : true,
                      "claimants" : [
                       {
                        "qualifying" : true,
                        "isPartner" : false,
                        "failures" : []
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true,
                        "failures" : []
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "name" : "Child 1",
                        "qualifying" : true,
                        "from" : "2016-08-27",
                        "until" : "2016-11-27",
                        "failures" : []
                       }
                      ]
                    },
                    {
                      "from" : "2016-11-27",
                      "until" : "2017-02-27",
                      "periodEligibility" : true,
                      "claimants" : [
                       {
                        "qualifying" : true,
                        "isPartner" : false,
                        "failures" : []
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true,
                        "failures" : []
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "name" : "Child 1",
                        "qualifying" : true,
                        "from" : "2016-11-27",
                        "until" : "2017-02-27",
                        "failures" : []
                       }
                      ]
                    },
                    {
                      "from" : "2017-02-27",
                      "until" : "2017-05-27",
                      "periodEligibility" : true,
                      "claimants" : [
                       {
                        "qualifying" : true,
                        "isPartner" : false,
                        "failures" : []
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true,
                        "failures" : []
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "name" : "Child 1",
                        "qualifying" : true,
                        "from" : "2017-02-27",
                        "until" : "2017-05-27",
                        "failures" : []
                       }
                      ]
                    }
                   ]
                }
            }
        }
        """.stripMargin)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "accept valid json scenario when parent and partner selected carer's allowance with parent and partner do not qualify and return a valid response  (TFC start date provided)" in {
      val controller = mockTFCEligibilityController
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tfc/carers_allowance_both.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)
      val JsonResult = inputJson.validate[Request]
      val eligibilityResult = TFCEligibility.eligibility.eligibility(JsonResult.get)(any[play.api.mvc.Request[_]], any[HeaderCarrier])

      when(controller.eligibility.eligibility(mockEq(JsonResult.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligibilityResult))
      val result = await(controller.eligible(request))

      val outputJson = Json.parse(
        s"""
        {
            "eligibility": {
                "tc": null,
                "esc": null,
                "tfc": {
                  "from": "2016-08-27",
                  "until": "2017-05-27",
                  "householdEligibility": false,
                   "periods": [
                    {
                      "from" : "2016-08-27",
                      "until" : "2016-11-27",
                      "periodEligibility" : true,
                      "claimants" : [
                       {
                        "qualifying" : true,
                        "isPartner" : false,
                        "failures" : []
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true,
                        "failures" : []
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "name" : "Child 1",
                        "qualifying" : true,
                        "from" : "2016-08-27",
                        "until" : "2016-11-27",
                        "failures" : []
                       }
                      ]
                    },
                    {
                      "from" : "2016-11-27",
                      "until" : "2017-02-27",
                      "periodEligibility" : true,
                      "claimants" : [
                       {
                        "qualifying" : true,
                        "isPartner" : false,
                        "failures" : []
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true,
                        "failures" : []
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "name" : "Child 1",
                        "qualifying" : true,
                        "from" : "2016-11-27",
                        "until" : "2017-02-27",
                        "failures" : []
                       }
                      ]
                    },
                    {
                      "from" : "2017-02-27",
                      "until" : "2017-05-27",
                      "periodEligibility" : true,
                      "claimants" : [
                       {
                        "qualifying" : true,
                        "isPartner" : false,
                        "failures" : []
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true,
                        "failures" : []
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "name" : "Child 1",
                        "qualifying" : true,
                        "from" : "2017-02-27",
                        "until" : "2017-05-27",
                        "failures" : []
                       }
                      ]
                    }
                   ]
                }
            }
        }
        """.stripMargin)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }

    "accept valid json scenario when parent and partner selected carer's allowance with partner hours and return a valid response (TFC start date provided)" in {
      val controller = mockTFCEligibilityController
      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tfc/carers_allowance_hours_both.json").toString)
      val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)
      val JsonResult = inputJson.validate[Request]
      val eligibilityResult = TFCEligibility.eligibility.eligibility(JsonResult.get)(any[play.api.mvc.Request[_]], any[HeaderCarrier])

      when(controller.eligibility.eligibility(mockEq(JsonResult.get))(any[play.api.mvc.Request[_]], any[HeaderCarrier])).thenReturn(Future.successful(eligibilityResult))
      val result = await(controller.eligible(request))

      val outputJson = Json.parse(
        s"""
        {
            "eligibility": {
                "tc": null,
                "esc": null,
                "tfc": {
                  "from": "2016-08-27",
                  "until": "2017-05-27",
                  "householdEligibility": true,
                   "periods": [
                    {
                      "from" : "2016-08-27",
                      "until" : "2016-11-27",
                      "periodEligibility" : true,
                      "claimants" : [
                       {
                        "qualifying" : true,
                        "isPartner" : false,
                        "failures" : []
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true,
                        "failures" : []
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "name" : "Child 1",
                        "qualifying" : true,
                        "from" : "2016-08-27",
                        "until" : "2016-11-27",
                        "failures" : []
                       }
                      ]
                    },
                    {
                      "from" : "2016-11-27",
                      "until" : "2017-02-27",
                      "periodEligibility" : true,
                      "claimants" : [
                       {
                        "qualifying" : true,
                        "isPartner" : false,
                        "failures" : []
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true,
                        "failures" : []
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "name" : "Child 1",
                        "qualifying" : true,
                        "from" : "2016-11-27",
                        "until" : "2017-02-27",
                        "failures" : []
                       }
                      ]
                    },
                    {
                      "from" : "2017-02-27",
                      "until" : "2017-05-27",
                      "periodEligibility" : true,
                      "claimants" : [
                       {
                        "qualifying" : true,
                        "isPartner" : false,
                        "failures" : []
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true,
                        "failures" : []
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "name" : "Child 1",
                        "qualifying" : true,
                        "from" : "2017-02-27",
                        "until" : "2017-05-27",
                        "failures" : []
                       }
                      ]
                    }
                   ]
                }
            }
        }
        """.stripMargin)

      status(result) shouldBe Status.OK
      jsonBodyOf(result) shouldBe outputJson
    }
  }


}
