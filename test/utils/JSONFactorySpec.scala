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

package utils

import controllers.FakeCCEligibilityApplication
import models.output.esc.{ESCEligibilityOutput, ESCPeriod}
import models.output.tfc._
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import play.api.libs.json._

class JSONFactorySpec extends FakeCCEligibilityApplication {

  "JSONFactory" must {


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


    "Return a valid TFC response with eligibility result" in {
      val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
      val from = LocalDate.parse("2015-06-30", formatter)

      val outputClaimant = models.output.tfc.TFCOutputClaimant(qualifying = true, isPartner = false)
      val outputStartPeriod1 = LocalDate.parse("2015-06-30", formatter)
      val outputUntilPeriod1 = LocalDate.parse("2015-09-30", formatter)
      val outputStartPeriod2 = LocalDate.parse("2015-09-30", formatter)
      val outputUntilPeriod2 = LocalDate.parse("2015-12-30", formatter)
      val outputPeriodChild1 = models.output.tfc.TFCOutputChild(id = 0, qualifying = true, from = Some(outputStartPeriod1), until = Some(outputUntilPeriod1))
      val outputPeriodChild2 = models.output.tfc.TFCOutputChild(id = 0, childcareCostPeriod=Periods.Weekly, qualifying = true, from = Some(outputStartPeriod2), until = Some(outputUntilPeriod2))

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
      val tfcEligibilityModel = TFCEligibilityOutput(from = from, until = tfcPeriods.last.until, householdEligibility = true, periods = tfcPeriods)

      val outputJson = Json.parse(
        s"""
        {
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
                "isPartner" : false
               }
              ],
              "children" : [
               {
                "id":0,
                "qualifying" : true,
                "from" : "2015-06-30",
                "until" : "2015-09-30",
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
      val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse("2015-06-20", formatter)
      val periodEnd = LocalDate.parse("2016-04-06", formatter)

      val outputClaimant1 = models.output.esc.ESCClaimant(
        qualifying = true,
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
              childCareCost = 100,
              childCareCostPeriod = Periods.Monthly
            )
          )
        )
      )

      val outputTaxYear = models.output.esc.ESCTaxYear(from = periodStart, until = periodEnd, periods = escPeriods)
      val escEligibilityModel =   ESCEligibilityOutput(taxYears = List(outputTaxYear))

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
                           "escStartDate":"${LocalDate.now().toString}"
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
