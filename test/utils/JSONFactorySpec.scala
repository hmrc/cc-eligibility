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

package utils

import controllers.FakeCCEligibilityApplication
import models.output.OutputAPIModel._
import models.output.esc.{ESCEligibilityModel, ESCPeriod}
import models.output.tc._
import models.output.tfc._
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.data.validation.ValidationError
import play.api.libs.json._
import spec.CCSpecConfig

class JSONFactorySpec extends CCSpecConfig with FakeCCEligibilityApplication {

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

      val outputClaimant = models.output.tc.OutputClaimant(qualifying = true, isPartner = false, claimantDisability = ClaimantDisability(disability = false, severeDisability = false), failures = List())
      val outputChild = models.output.tc.OutputChild(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = false, severeDisability = false, childcare = true), failures = List())
      val outputHhElements = models.output.tc.HouseholdElements(basic = true, hours30 = false, childcare = true, loneParent = true, secondParent = false, family = true, wtc = true, ctc = true)
      val outputPeriod = models.output.tc.TCPeriod(from = periodStartDate, until = periodEndDate, householdElements = outputHhElements, claimants = List(outputClaimant), children = List(outputChild))
      val outputTaxYear = models.output.tc.TaxYear(from = periodStartDate, until = periodEndDate, houseHoldIncome = BigDecimal(0.00), periods = List(outputPeriod))
      val tcEligibilityModel = TCEligibilityModel(eligible = true, taxYears = List(outputTaxYear))

      val eligibility = Eligibility(tc = Some(tcEligibilityModel))

      val outputJson = Json.parse(
        s"""
          {
            "eligibility": {
              "tc": {
                "eligible": true,
                "taxYears": [
                  {
                    "from": "${periodStartDate.toString("yyyy-MM-dd")}",
                    "until": "${periodEndDate.toString("yyyy-MM-dd")}",
                    "houseHoldIncome": 0.0,
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
                            "failures": []
                          }
                        ],
                        "children": [
                          {
                            "id": 0,
                            "name": "Child 1",
                            "childcareCost": 200.0,
                            "childcareCostPeriod": "Month",
                            "qualifying": true,
                            "childElements": {
                              "child": true,
                              "youngAdult": false,
                              "disability": false,
                              "severeDisability": false,
                              "childcare": true
                            },
                            "failures": []
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

      val result = utils.JSONFactory.generateResultJson(eligibility)
      result shouldBe outputJson
    }

    "Return a valid TFC response with eligibility result" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-08-27", formatter)
      val from = LocalDate.parse("2015-06-30", formatter)

      val outputClaimant = models.output.tfc.OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val outputStartPeriod1 = LocalDate.parse("2015-06-30", formatter)
      val outputUntilPeriod1 = LocalDate.parse("2015-09-30", formatter)
      val outputStartPeriod2 = LocalDate.parse("2015-09-30", formatter)
      val outputUntilPeriod2 = LocalDate.parse("2015-12-30", formatter)
      val outputPeriodChild1 = models.output.tfc.OutputChild(id = 0, name = Some("Child 1"), qualifying = true, from = outputStartPeriod1, until = outputUntilPeriod1, failures = List())
      val outputPeriodChild2 = models.output.tfc.OutputChild(id = 0, name = Some("Child 1"), qualifying = true, from = outputStartPeriod2, until = outputUntilPeriod2, failures = List())

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
      val tfcEligibilityModel = TFCEligibilityModel(from = from, until = tfcPeriods.last.until, householdEligibility = true, periods = tfcPeriods)
      val eligibilityOutputModel = Eligibility(tfc = Some(tfcEligibilityModel))

      val outputJson = Json.parse(
        s"""
        {
            "eligibility": {
                "tc": null,
                "tfc": {
                  "from": "2015-06-30",
                  "until": "2015-12-30",
                  "householdEligibility": true,
                   "periods": [
                    {
                      "from" : "2015-06-30",
                      "until" : "2015-09-30",
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
                        "from" : "2015-06-30",
                        "until" : "2015-09-30",
                        "failures" : []
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
                        "isPartner" : false,
                        "failures" : []
                       }
                      ],
                      "children" : [
                       {
                        "id" : 0,
                        "name" : "Child 1",
                        "qualifying" : true,
                        "from" : "2015-09-30",
                        "until" : "2015-12-30",
                        "failures" : []
                       }
                      ]
                    }
                   ]
                },
            "esc": null
            }
        }
        """.stripMargin)

      val result = utils.JSONFactory.generateResultJson(eligibilityOutputModel)
      result shouldBe outputJson
    }

    "Return a valid ESC response with eligibility result" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("2015-07-01", formatter)
      val dateOfBirth2 = LocalDate.parse("2002-06-27", formatter)
      val periodStart = LocalDate.parse("2015-06-20", formatter)
      val periodEnd = LocalDate.parse("2016-04-06", formatter)

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        name = Some("Child 1"),
        qualifying = false,
        failures = List()
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 0,
        name = Some("Child 2"),
        qualifying = true,
        failures = List()
      )

      val outputClaimant1 = models.output.esc.OutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 11,
        elements = models.output.esc.ClaimantElements(
          vouchers = true
        ),
        failures = List()
      )

      val escPeriods =  List(
        ESCPeriod(
          from = periodStart,
          until = periodEnd,
          claimants = List(
            outputClaimant1
          ),
          children = List(
            outputChild1,
            outputChild2
          )
        )
      )

      val outputTaxYear = models.output.esc.TaxYear(from = periodStart, until = periodEnd, periods = escPeriods)

      val escEligibilityModel = ESCEligibilityModel(taxYears = List(outputTaxYear))
      val eligibilityOutputModel = Eligibility(esc = Some(escEligibilityModel))


      val outputJson = Json.parse(
        s"""
        {
         "eligibility":{
            "tc":null,
            "tfc":null,
            "esc":{
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
                                 "elements":{
                                    "vouchers":true
                                 },
                                 "failures":[

                                 ]
                              }
                           ],
                           "children":[
                              {
                                 "id":0,
                                 "name":"Child 1",
                                 "qualifying":false,
                                 "failures":[

                                 ]
                              },
                              {
                                 "id":0,
                                 "name":"Child 2",
                                 "qualifying":true,
                                 "failures":[

                                 ]
                              }
                           ]
                        }
                     ]
                  }
               ]
            }
         }
      }
        """.stripMargin)

      val result = utils.JSONFactory.generateResultJson(eligibilityOutputModel)
      result shouldBe outputJson
    }
  }
}
