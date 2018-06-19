/*
 * Copyright 2018 HM Revenue & Customs
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

package utils

import controllers.FakeCCEligibilityApplication
import models.output.esc.{ESCEligibilityOutput, ESCPeriod}
import models.output.tc._
import models.output.tfc._
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.data.validation.ValidationError
import play.api.libs.json._

class JSONFactorySpec extends FakeCCEligibilityApplication {

  "JSONFactory" should {

    "Return a valid output JSON when error sequence and status are passed" in {
      val status = 400
      val JSONPath = JsPath \ "tc"
      val validationError1 = ValidationError("Very Bad Thing Happened", 42)
      val validationError2 = ValidationError("Not So Bad Thing Happened", "Error")
      val errorTuple: (play.api.libs.json.JsPath, Seq[play.api.data.validation.ValidationError]) = (JSONPath, Seq(validationError1, validationError2))

      val outputJSON = Json.parse(
        """
          |{
          |"status": 400,
          |"errors":
          |[
          |   {
          |     "path" : "/tc",
          |     "validationErrors" :
          |     [
          |       {
          |        "message": "Very Bad Thing Happened",
          |        "args": [42]
          |       },
          |       {
          |        "message": "Not So Bad Thing Happened",
          |        "args": ["Error"]
          |       }
          |     ]
          |   }
          | ]
          | }
        """.stripMargin)

      val result = utils.JSONFactory.generateErrorJSON(status, Left(Seq(errorTuple)))
      result shouldBe outputJSON
    }

    "Return a valid output JSON if error sequence is missing" in {
      val status = 500
      val outputJSON = Json.parse(
        """
          |{
          |"status": 500,
          |"errors": ["Error while generating JSON response"]
          | }
        """.stripMargin)

      val result = utils.JSONFactory.generateErrorJSON(status, Left(Nil))
      result shouldBe outputJSON
    }

    "Return a valid output JSON when exception and status are passed" in {
      val status = 500
      val exception = new Exception("Very Bad Thing Happened")

      val outputJSON = Json.parse(
        """
          |{
          |"status": 500,
          |"error": "Very Bad Thing Happened"
          | }
        """.stripMargin)

      val result  = utils.JSONFactory.generateErrorJSON(status, Right(exception))
      result shouldBe outputJSON
    }

    "Return a valid response with eligibility result" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStartDate = LocalDate.parse("2014-09-01", formatter)
      val periodEndDate = LocalDate.parse("2015-04-05", formatter)

      val outputClaimant = models.output.tc.TCOutputClaimant(qualifying = true, isPartner = false, claimantDisability = TCDisability(disability = false, severeDisability = false))
      val outputChild = models.output.tc.TCOutputChild(childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = TCChildElements(child = true, youngAdult = false, disability = false, severeDisability = false, childcare = true))
      val outputHhElements = models.output.tc.TCHouseHoldElements(basic = true, hours30 = false, childcare = true, loneParent = true, secondParent = false, family = true, wtc = true, ctc = true)
      val outputPeriod = models.output.tc.TCPeriod(from = periodStartDate, until = periodEndDate, householdElements = outputHhElements, claimants = List(outputClaimant), children = List(outputChild))
      val outputTaxYear = models.output.tc.TCTaxYear(from = periodStartDate, until = periodEndDate, periods = List(outputPeriod))
      val tcEligibilityModel = TCEligibilityOutput(eligible = true, taxYears = List(outputTaxYear))

      val outputJson = Json.parse(
        s"""
          {
                "eligible": true,
                "taxYears": [
                  {
                    "from": "${periodStartDate.toString("yyyy-MM-dd")}",
                    "until": "${periodEndDate.toString("yyyy-MM-dd")}",
                    "currentHouseholdIncome": {},
                    "previousHouseholdIncome": {},
                    "periods": [
                      {
                        "from": "${periodStartDate.toString("yyyy-MM-dd")}",
                        "until": "${periodEndDate.toString("yyyy-MM-dd")}",
                        "householdElements": {
                          "basic": true,
                          "hours30": false,
                          "childcare": true,
                          "loneParent": true,
                          "secondParent": false,
                          "family": true,
                          "wtc": true,
                          "ctc": true
                        },
                        "claimants": [
                          {
                            "qualifying": true,
                            "isPartner": false,
                            "claimantDisability": {
                              "disability": false,
                              "severeDisability": false
                            },
                            "doesNotTaper": false
                          }
                        ],
                        "children": [
                          {
                            "childcareCost": 200.0,
                            "childcareCostPeriod": "Month",
                            "qualifying": true,
                            "childElements": {
                              "child": true,
                              "youngAdult": false,
                              "disability": false,
                              "severeDisability": false,
                              "childcare": true
                            }
                          }
                        ]
                      }
                    ]
                  }
                ],
                "wtc": false,
                "ctc": false
              }
        """.stripMargin)

      val result = Json.toJson(tcEligibilityModel)
      result shouldBe outputJson
    }

    "Return a valid TFC response with eligibility result" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-08-27", formatter)
      val from = LocalDate.parse("2015-06-30", formatter)

      val outputClaimant = models.output.tfc.TFCOutputClaimant(qualifying = true, isPartner = false)
      val outputStartPeriod1 = LocalDate.parse("2015-06-30", formatter)
      val outputUntilPeriod1 = LocalDate.parse("2015-09-30", formatter)
      val outputStartPeriod2 = LocalDate.parse("2015-09-30", formatter)
      val outputUntilPeriod2 = LocalDate.parse("2015-12-30", formatter)
      val outputPeriodChild1 = models.output.tfc.TFCOutputChild(id = 0, qualifying = true, from = Some(outputStartPeriod1), until = Some(outputUntilPeriod1), tfcRollout = false)
      val outputPeriodChild2 = models.output.tfc.TFCOutputChild(id = 0, childcareCostPeriod=Periods.Weekly, qualifying = true, from = Some(outputStartPeriod2), until = Some(outputUntilPeriod2), tfcRollout = false)

      val tfcPeriods = List(
        TFCPeriod(from = outputStartPeriod1,
          until = outputUntilPeriod1,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(outputPeriodChild1)),
        TFCPeriod(from = outputStartPeriod2,
          until = outputUntilPeriod2,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(outputPeriodChild2))
      )
      val tfcEligibilityModel = TFCEligibilityOutput(from = from, until = tfcPeriods.last.until, householdEligibility = true, periods = tfcPeriods, tfcRollout = false)

      val outputJson = Json.parse(
        s"""
        {
          "from": "2015-06-30",
          "until": "2015-12-30",
          "householdEligibility": true,
           "tfcRollout":false,
           "periods": [
            {
              "from" : "2015-06-30",
              "until" : "2015-09-30",
              "periodEligibility" : true,
              "claimants" : [
               {
                "qualifying" : true,
                "isPartner" : false
               }
              ],
              "children" : [
               {
                "id":0,
                "qualifying" : true,
                "from" : "2015-06-30",
                "until" : "2015-09-30",
                "tfcRollout":false,
                "childcareCost":0,
                "childcareCostPeriod": "Month",
                "disability": {
                  "disabled":false,
                  "severelyDisabled":false
                  }
               }
              ]
            },
            {
              "from" : "2015-09-30",
              "until" : "2015-12-30",
              "periodEligibility" : true,
              "claimants" : [
               {
                "qualifying" : true,
                "isPartner" : false
               }
              ],
              "children" : [
               {
                "id":0,
                "qualifying" : true,
                "from" : "2015-09-30",
                "until" : "2015-12-30",
                "tfcRollout":false,
                "childcareCost":0,
                "childcareCostPeriod": "Week",
                "disability": {
                  "disabled":false,
                  "severelyDisabled":false
                  }
               }
              ]
            }
           ]
        }
      """.stripMargin)

      val result = Json.toJson(tfcEligibilityModel)
      result shouldBe outputJson
    }

    "Return a valid ESC response with eligibility result" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("2015-07-01", formatter)
      val dateOfBirth2 = LocalDate.parse("2002-06-27", formatter)
      val periodStart = LocalDate.parse("2015-06-20", formatter)
      val periodEnd = LocalDate.parse("2016-04-06", formatter)

      val outputClaimant1 = models.output.esc.ESCClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 11,
        vouchers = true
      )

      val escPeriods =  List(
        ESCPeriod(
          from = periodStart,
          until = periodEnd,
          claimants = List(
            outputClaimant1
          ),
          children = List(
            models.output.esc.ESCChild(
              qualifying = false,
              childCareCost = 100,
              childCareCostPeriod = Periods.Monthly
            )
          )
        )
      )

      val outputTaxYear = models.output.esc.ESCTaxYear(from = periodStart, until = periodEnd, periods = escPeriods)
      val escEligibilityModel = ESCEligibilityOutput(taxYears = List(outputTaxYear))

      val outputJson = Json.parse(
        s"""
        {
            "taxYears":[
            {
               "from":"2015-06-20",
               "until":"2016-04-06",
               "periods":[
                  {
                     "from":"2015-06-20",
                     "until":"2016-04-06",
                     "claimants":[
                        {
                           "qualifying":true,
                           "isPartner":false,
                           "eligibleMonthsInPeriod":11,
                           "vouchers":true,
                           "escStartDate":"${LocalDate.now().toString()}"
                        }
                     ],
                     "children": [
                        {
                          "qualifying": false,
                          "childCareCost": 100,
                          "childCareCostPeriod": "Month"
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

      val result = Json.toJson(escEligibilityModel)
      result shouldBe outputJson
    }
  }
}
