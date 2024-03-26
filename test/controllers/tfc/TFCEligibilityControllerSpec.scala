/*
 * Copyright 2023 HM Revenue & Customs
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
import models.input.tfc.TFCEligibilityInput
import models.output.tfc.TFCEligibilityOutput
import java.time.LocalDate
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfter
import play.api.http.Status
import play.api.libs.json.{JsValue, Json}
import play.api.mvc._
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import service.AuditEvents
import uk.gov.hmrc.http.HeaderCarrier
import utils.CCConfigSpec

import scala.concurrent.{ExecutionContext, Future}

class TFCEligibilityControllerSpec extends CCConfigSpec
  with FakeCCEligibilityApplication
  with BeforeAndAfter {

  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  val validTFCEligibilityInputJson: JsValue = Json.parse(JsonLoader.fromResource("/json/input/tfc/eligibility_input_test.json").toString)
  val validTFCEligibilityOutput: TFCEligibilityOutput = TFCEligibilityOutput(LocalDate.now(), LocalDate.now, false, false, Nil)
  val SUT = new TFCEligibilityController(mock[TFCEligibility], mock[AuditEvents], mockCC)

  private def withCalltoPOSTInvalidPayload(payload: String)(handler: Future[Result] => Any) = {
    handler(SUT.eligible.apply(registerRequestWithPayload(Json.parse(payload))))
  }

  private def withCallToPOST(payload: JsValue)(handler: Future[Result] => Any) = {
    handler(SUT.eligible.apply(registerRequestWithPayload(payload)))
  }
  private def registerRequestWithPayload(payload: JsValue): Request[JsValue] = FakeRequest(
    "POST",
    "",
    FakeHeaders(),
    payload
  ).withHeaders(CONTENT_TYPE -> "application/json")

  before{
    when(SUT.tfcEligibility.eligibility(any[TFCEligibilityInput]())(any[HeaderCarrier])).
      thenReturn(Future.successful(validTFCEligibilityOutput))
  }

  "TFCEligibilityController" must {

    "not return NOT_FOUND endpoint" in {
      val result = route(app, FakeRequest(POST, "/cc-eligibility/tax-free-childcare/eligibility"))
      result.isDefined shouldBe true
      status(result.get) should not be NOT_FOUND
    }

    "accept valid json should return Json body" in {

      withCallToPOST(validTFCEligibilityInputJson){ result =>
        result.flatMap{r =>
          jsonBodyOf(r) shouldBe Json.toJson(validTFCEligibilityOutput)
        }
      }
    }

    "return Internal Server Error with error message if an exception is thrown during eligibility" in {
      when(SUT.tfcEligibility.eligibility(any[TFCEligibilityInput]())(any[HeaderCarrier])).
        thenReturn(Future.failed(new Exception("Something bad happened in Eligibility")))

      val outputJSON = Json.parse(
        """
          |{
          |    "status": 500,
          |    "error": "Something bad happened in Eligibility"
          |}
        """.stripMargin)
      withCallToPOST(validTFCEligibilityInputJson){ result =>

        status(result) shouldBe Status.INTERNAL_SERVER_ERROR
        result.flatMap{r => jsonBodyOf(r) shouldBe outputJSON}
      }
    }

    "accept invalid json with incorrect from date format should return a bad request" in {

      val inputJson = JsonLoader.fromResource("/json/input/tfc/incorrect_from_date_format.json").toString

      withCalltoPOSTInvalidPayload(inputJson){ result =>
        status(result) shouldBe Status.BAD_REQUEST
      }
    }

    "accept a valid json if number of claimant/s less than 1 should return 400" in {

      val inputJson = JsonLoader.fromResource("/json/input/tfc/incorrect_from_date_format.json").toString

      withCalltoPOSTInvalidPayload(inputJson){ result =>
        status(result) shouldBe 400
      }
    }

    "accept invalid json if child id has negative value should return 400" in {

      val inputJson = JsonLoader.fromResource("/json/input/tfc/negative_child_id.json").toString
      withCalltoPOSTInvalidPayload(inputJson){ result =>
        status(result) shouldBe 400
      }
    }

    "accept a valid json if number of children more than 25 should return bad request" in {

      val inputJson = JsonLoader.fromResource("/json/input/tfc/invalid_no_of_children.json").toString
      withCalltoPOSTInvalidPayload(inputJson){ result =>
        status(result) shouldBe BAD_REQUEST
      }
    }

    "accept valid json scenario when claimant selected carer's allowance and return a valid response (TFC start date provided)" in {

      val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tfc/carers_allowance_parent.json").toString)

      val outputJson = Json.parse(
        s"""
        {
                  "from": "2016-08-27",
                  "until": "2017-05-27",
                  "householdEligibility": true,
                  "tfcRollout":true,
                   "periods": [
                    {
                      "from" : "2016-08-27",
                      "until" : "2016-11-27",
                      "periodEligibility" : true,
                      "claimants" : [
                       {
                        "qualifying" : true,
                        "isPartner" : false
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "qualifying" : true,
                        "from" : "2016-08-27",
                        "until" : "2016-11-27",
                        "tfcRollout":true
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
                        "isPartner" : false
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "qualifying" : true,
                        "from" : "2016-11-27",
                        "until" : "2017-02-27",
                        "tfcRollout":true
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
                        "isPartner" : false
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "qualifying" : true,
                        "from" : "2017-02-27",
                        "until" : "2017-05-27",
                        "tfcRollout":true
                       }
                      ]
                    }
                   ]
        }
        """.stripMargin)

      withCallToPOST(inputJson){ res =>
        res.flatMap{ result =>
          status(result) shouldBe Status.OK
          jsonBodyOf(result) shouldBe outputJson
        }
      }
    }
  }

  "accept valid json scenario when partner selected carer's allowance and return a valid response (TFC start date provided)" in {

    val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tfc/carers_allowance_partner.json").toString)

    val outputJson = Json.parse(
      s"""
        {
                   "from": "2016-08-27",
                  "until": "2017-05-27",
                  "householdEligibility": true,
                  "tfcRollout":true,
                   "periods": [
                    {
                      "from" : "2016-08-27",
                      "until" : "2016-11-27",
                      "periodEligibility" : true,
                      "claimants" : [
                       {
                        "qualifying" : true,
                        "isPartner" : false
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "qualifying" : true,
                        "from" : "2016-08-27",
                        "until" : "2016-11-27",
                        "tfcRollout":true
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
                        "isPartner" : false
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "qualifying" : true,
                        "from" : "2016-11-27",
                        "until" : "2017-02-27",
                        "tfcRollout":true
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
                        "isPartner" : false
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "qualifying" : true,
                        "from" : "2017-02-27",
                        "until" : "2017-05-27",
                        "tfcRollout":true
                       }
                      ]
                    }
                   ]
        }
        """.stripMargin)

    withCallToPOST(inputJson){  res =>
      res.flatMap{ result =>
        status(result) shouldBe Status.OK
        jsonBodyOf(result) shouldBe outputJson
      }
    }
  }

  "accept valid json scenario when parent and partner selected carer's allowance with parent and partner do not qualify and return a valid response  (TFC start date provided)" in {

    val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tfc/carers_allowance_both.json").toString)

    val outputJson = Json.parse(
      s"""
        {
                  "from": "2016-08-27",
                  "until": "2017-05-27",
                  "householdEligibility": false,
                  "tfcRollout":true,
                   "periods": [
                    {
                      "from" : "2016-08-27",
                      "until" : "2016-11-27",
                      "periodEligibility" : true,
                      "claimants" : [
                       {
                        "qualifying" : true,
                        "isPartner" : false
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "qualifying" : true,
                        "from" : "2016-08-27",
                        "until" : "2016-11-27",
                        "tfcRollout":true
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
                        "isPartner" : false
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "qualifying" : true,
                        "from" : "2016-11-27",
                        "until" : "2017-02-27",
                        "tfcRollout":true
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
                          "isPartner" : false
                        },
                        {
                          "qualifying" : true,
                          "isPartner" : true
                        }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "qualifying" : true,
                        "from" : "2017-02-27",
                        "until" : "2017-05-27",
                        "tfcRollout":true
                       }
                      ]
                    }
                   ]
        }
        """.stripMargin)

    withCallToPOST(inputJson){  res =>
      res.flatMap{ result =>
        status(result) shouldBe Status.OK
        jsonBodyOf(result) shouldBe outputJson
      }
    }
  }

  "accept valid json scenario when parent and partner selected carer's allowance with partner hours and return a valid response (TFC start date provided)" in {

    val inputJson = Json.parse(JsonLoader.fromResource("/json/input/tfc/carers_allowance_hours_both.json").toString)

    val outputJson = Json.parse(
      s"""
        {
                  "from": "2016-08-27",
                  "until": "2017-05-27",
                  "householdEligibility": true,
                  "tfcRollout":true,
                   "periods": [
                    {
                      "from" : "2016-08-27",
                      "until" : "2016-11-27",
                      "periodEligibility" : true,
                      "claimants" : [
                       {
                        "qualifying" : true,
                        "isPartner" : false
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "qualifying" : true,
                        "from" : "2016-08-27",
                        "until" : "2016-11-27",
                        "tfcRollout":true
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
                        "isPartner" : false
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "qualifying" : true,
                        "from" : "2016-11-27",
                        "until" : "2017-02-27",
                        "tfcRollout":true
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
                        "isPartner" : false
                       },
                       {
                        "qualifying" : true,
                        "isPartner" : true
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "qualifying" : true,
                        "from" : "2017-02-27",
                        "until" : "2017-05-27",
                        "tfcRollout":true
                       }
                      ]
                    }
                   ]
        }
        """.stripMargin)

    withCallToPOST(inputJson){  res =>
      res.flatMap{ result =>
        status(result) shouldBe Status.OK
        jsonBodyOf(result) shouldBe outputJson
      }
    }
  }
}
