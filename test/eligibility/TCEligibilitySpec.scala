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

package eligibility

import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader
import controllers.FakeCCEligibilityApplication
import models.input.tc._

import models.output.tc.{ChildElements, ClaimantDisability, TCEligibilityModel}
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.scalatest.mock.MockitoSugar
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}
import spec.CCSpecConfig
import utils.Periods
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table
import scala.concurrent.Future

class TCEligibilitySpec extends CCSpecConfig with FakeCCEligibilityApplication with org.scalatest.PrivateMethodTester with MockitoSugar {

  "TCEligibility" should {

    "return a Future[TCEligibilityModel] result" in {
      val service = TCEligibility
      val result = service.eligibility(TCEligibilityInput(taxYears = List()))
      result.isInstanceOf[Future[TCEligibilityModel]] shouldBe true
    }

    "(no change) determine start dates of periods in the tax year" in {
      val dateOfBirth = LocalDate.parse("2008-08-27", formatter)
      val child = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = TaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child))

      val decoratedDetermineStartDatesOfPeriodsInTaxYear = PrivateMethod[List[LocalDate]]('determineStartDatesOfPeriodsInTaxYear)
      val result = TCEligibility invokePrivate decoratedDetermineStartDatesOfPeriodsInTaxYear(taxYear)

      result.length shouldBe 1
    }

    "(one change, one period) determine start dates of periods in the tax year" in {
      val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
      val child = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = TaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child))

      val decoratedDetermineStartDatesOfPeriodsInTaxYear = PrivateMethod[List[LocalDate]]('determineStartDatesOfPeriodsInTaxYear)
      val result = TCEligibility invokePrivate decoratedDetermineStartDatesOfPeriodsInTaxYear(taxYear)

      result.length shouldBe 2
    }

    "(one change, two periods)(child being born in tax year) determine start dates of periods in tax year" in {
      val dateOfBirth = LocalDate.parse("2016-08-27", formatter)
      val child = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)

      val startTaxYear = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = TaxYear(from = startTaxYear, until = endTaxYear, claimants = List(), children = List(child))

      val decoratedDetermineStartDatesOfPeriodsInTaxYear = PrivateMethod[List[LocalDate]]('determineStartDatesOfPeriodsInTaxYear)
      val result = TCEligibility invokePrivate decoratedDetermineStartDatesOfPeriodsInTaxYear(taxYear)

      result.length shouldBe 2
    }

    "(two changes, 3 periods - same day) determine start dates of periods in the tax year" in {
      val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
      val dateOfBirth20 = LocalDate.parse("1996-08-27", formatter)
      val child = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val childTurns19 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth20, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = TaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child, childTurns19))

      val decoratedDetermineStartDatesOfPeriodsInTaxYear = PrivateMethod[List[LocalDate]]('determineStartDatesOfPeriodsInTaxYear)
      val result = TCEligibility invokePrivate decoratedDetermineStartDatesOfPeriodsInTaxYear(taxYear)

      result.length shouldBe 3
    }

    "(two changes, 2 periods) determine start dates of periods in the tax year" in {
      val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
      val dateOfBirth20age = LocalDate.parse("1996-09-01", formatter)
      val child = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val dateOfBirth20 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth20age, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = TaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child, dateOfBirth20))

      val decoratedDetermineStartDatesOfPeriodsInTaxYear = PrivateMethod[List[LocalDate]]('determineStartDatesOfPeriodsInTaxYear)
      val result = TCEligibility invokePrivate decoratedDetermineStartDatesOfPeriodsInTaxYear(taxYear)

      result.length shouldBe 2
    }

    "(three changes, 2 periods) determine start dates of periods in the tax year" in {
      val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
      val dateOfBirth20 = LocalDate.parse("1996-08-27", formatter)
      val child = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child1Turns20 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth20, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child2Turns20 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth20, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = TaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child, child1Turns20, child2Turns20))

      val decoratedDetermineStartDatesOfPeriodsInTaxYear = PrivateMethod[List[LocalDate]]('determineStartDatesOfPeriodsInTaxYear)
      val result = TCEligibility invokePrivate decoratedDetermineStartDatesOfPeriodsInTaxYear(taxYear)

      result.length shouldBe 3
    }

    "(two changes, 3 periods) determine start dates of periods in the tax year" in {
      val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
      val dateOfBirth20 = LocalDate.parse("1996-10-27", formatter)
      val child = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child1Turns20 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth20, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = TaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child, child1Turns20))

      val decoratedDetermineStartDatesOfPeriodsInTaxYear = PrivateMethod[List[LocalDate]]('determineStartDatesOfPeriodsInTaxYear)
      val result = TCEligibility invokePrivate decoratedDetermineStartDatesOfPeriodsInTaxYear(taxYear)

      result.length shouldBe 3
    }

    "(three changes, 4 periods) determine start dates of periods in the tax year" in {
      val dateOfBirth20Child1 = LocalDate.parse("1996-08-27", formatter)
      val dateOfBirth20Child2 = LocalDate.parse("1996-10-27", formatter)
      val dateOfBirth15Child3 = LocalDate.parse("2001-07-12", formatter)

      val child = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth20Child1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child1Turns20 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth20Child2, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child2Turns15 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth15Child3, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val today = LocalDate.parse("2016-07-05", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = TaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child, child1Turns20, child2Turns15))

      val decoratedDetermineStartDatesOfPeriodsInTaxYear = PrivateMethod[List[LocalDate]]('determineStartDatesOfPeriodsInTaxYear)
      val result = TCEligibility invokePrivate decoratedDetermineStartDatesOfPeriodsInTaxYear(taxYear)

      result.length shouldBe 4
    }

    "(one change, one period, child turning 15) determine start dates of periods in the tax year" in {
      val dateOfBirth = LocalDate.parse("2001-08-27", formatter)
      val child = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = TaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child))

      val decoratedDetermineStartDatesOfPeriodsInTaxYear = PrivateMethod[List[LocalDate]]('determineStartDatesOfPeriodsInTaxYear)
      val result = TCEligibility invokePrivate decoratedDetermineStartDatesOfPeriodsInTaxYear(taxYear)

      result.length shouldBe 2
    }

    "(single tax year - 1 period) return a tax year with entitlement periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_2.json")
      val json: JsValue = Json.parse(resource.toString)
      val txYrStartDate = LocalDate.parse("2016-08-27", formatter)
      val txYrEndDate = LocalDate.parse("2017-04-06", formatter)

      val result = json.validate[TCEligibilityInput]
      result match {
        case JsSuccess(x, _) =>
          x shouldBe a[TCEligibilityInput]

          val response = TCEligibility.eligibility(x)
          response.isInstanceOf[Future[TCEligibilityModel]] shouldBe true
          response.taxYears.length shouldBe 1
          response.taxYears.head.periods.length shouldBe 1

          response.taxYears.head.from shouldBe txYrStartDate
          response.taxYears.head.until shouldBe txYrEndDate
          response.taxYears.head.periods.head.from shouldBe txYrStartDate
          response.taxYears.head.periods.head.until shouldBe txYrEndDate

        case JsError(e) =>
          throw new RuntimeException(e.toList.toString())
      }
    }

    "(single tax year - 2 periods) return a tax year with entitlement periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_13.json")
      val json: JsValue = Json.parse(resource.toString)

      val txYrStartDate = LocalDate.parse("2016-08-10", formatter)
      val txYrEndDate = LocalDate.parse("2017-04-06", formatter)
      val period1StartDate = LocalDate.parse("2016-08-10", formatter)
      val period1EndDate = LocalDate.parse("2016-09-01", formatter)
      val period2StartDate = LocalDate.parse("2016-09-01", formatter)
      val period2EndDate = LocalDate.parse("2017-04-06", formatter)

      val result = json.validate[TCEligibilityInput]
      result match {
        case JsSuccess(x, _) =>
          x shouldBe a[TCEligibilityInput]

          val response = TCEligibility.eligibility(x)
          response.isInstanceOf[Future[TCEligibilityModel]] shouldBe true
          response.taxYears.length shouldBe 1
          response.taxYears.head.periods.length shouldBe 2

          //taxYear start/end dates
          response.taxYears.head.from shouldBe txYrStartDate
          response.taxYears.head.until shouldBe txYrEndDate

          //period start/end dates
          response.taxYears.head.periods.head.from shouldBe period1StartDate
          response.taxYears.head.periods.head.until shouldBe period1EndDate

          response.taxYears.head.periods.tail.head.from shouldBe period2StartDate
          response.taxYears.head.periods.tail.head.until shouldBe period2EndDate

        case JsError(e) =>
          throw new RuntimeException(e.toList.toString())
      }
    }

    "(multiple tax year - 1 period each) return a tax year with entitlement periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_19.json")
      val json: JsValue = Json.parse(resource.toString)

      val txYrStartDate = LocalDate.parse("2016-07-27", formatter)
      val txYrEndDate = LocalDate.parse("2017-04-06", formatter)
      val txYr2StartDate = LocalDate.parse("2017-04-06", formatter)
      val txYr2EndDate = LocalDate.parse("2017-07-28", formatter)

      val period1StartDate = LocalDate.parse("2016-07-27", formatter)
      val period1EndDate = LocalDate.parse("2017-04-06", formatter)

      val period2StartDate = LocalDate.parse("2017-04-06", formatter)
      val period2EndDate = LocalDate.parse("2017-07-28", formatter)

      val result = json.validate[TCEligibilityInput]
      result match {
        case JsSuccess(x, _) =>
          x shouldBe a[TCEligibilityInput]

          val response = TCEligibility.eligibility(x)

          response.isInstanceOf[Future[TCEligibilityModel]] shouldBe true
          response.taxYears.length shouldBe 2
          response.taxYears.head.periods.length shouldBe 1
          response.taxYears.tail.head.periods.length shouldBe 1

          //taxYear start/end dates
          response.taxYears.head.from shouldBe txYrStartDate
          response.taxYears.head.until shouldBe txYrEndDate

          response.taxYears.tail.head.from shouldBe txYr2StartDate
          response.taxYears.tail.head.until shouldBe txYr2EndDate

          //1st Tax Year period start/end dates
          response.taxYears.head.periods.head.from shouldBe period1StartDate
          response.taxYears.head.periods.head.until shouldBe period1EndDate

          //2nd Tax Year period start/end dates
          response.taxYears.tail.head.periods.head.from shouldBe period2StartDate
          response.taxYears.tail.head.periods.head.until shouldBe period2EndDate

        case JsError(e) =>
          throw new RuntimeException(e.toList.toString())
      }
    }

    "(multiple tax year - (1st TY 2 periods) (2nd TY 1 period)) return a tax year with entitlement periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_15.json")
      val json: JsValue = Json.parse(resource.toString)

      val txYrStartDate = LocalDate.parse("2016-05-27", formatter)
      val txYrEndDate = LocalDate.parse("2017-04-06", formatter)
      val txYr2StartDate = LocalDate.parse("2017-04-06", formatter)
      val txYr2EndDate = LocalDate.parse("2018-04-06", formatter)

      val period1StartDate = LocalDate.parse("2016-05-27", formatter)
      val period1EndDate = LocalDate.parse("2016-09-01", formatter)

      val period2StartDate = LocalDate.parse("2016-09-01", formatter)
      val period2EndDate = LocalDate.parse("2017-04-06", formatter)

      val TxYr2period1StartDate = LocalDate.parse("2017-04-06", formatter)
      val TxYr2period1EndDate = LocalDate.parse("2018-04-06", formatter)

      val result = json.validate[TCEligibilityInput]
      result match {
        case JsSuccess(x, _) =>
          x shouldBe a[TCEligibilityInput]

          val response = TCEligibility.eligibility(x)

          response.isInstanceOf[Future[TCEligibilityModel]] shouldBe true
          response.taxYears.length shouldBe 2
          response.taxYears.head.periods.length shouldBe 2
          response.taxYears.tail.head.periods.length shouldBe 1

          //taxYear start/end dates
          response.taxYears.head.from shouldBe txYrStartDate
          response.taxYears.head.until shouldBe txYrEndDate

          response.taxYears.tail.head.from shouldBe txYr2StartDate
          response.taxYears.tail.head.until shouldBe txYr2EndDate

          //1st Tax Year period start/end dates
          response.taxYears.head.periods.head.from shouldBe period1StartDate
          response.taxYears.head.periods.head.until shouldBe period1EndDate

          response.taxYears.head.periods.tail.head.from shouldBe period2StartDate
          response.taxYears.head.periods.tail.head.until shouldBe period2EndDate

          //2nd Tax Year period start/end dates
          response.taxYears.tail.head.periods.head.from shouldBe TxYr2period1StartDate
          response.taxYears.tail.head.periods.head.until shouldBe TxYr2period1EndDate

        case JsError(e) =>
          throw new RuntimeException(e.toList.toString())
      }
    }

    "(multiple tax year - (1st TY 2 periods) (2nd TY 3 period) (higher education) return a tax year with entitlement periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_18.json")
      val json: JsValue = Json.parse(resource.toString)

      val txYrStartDate = LocalDate.parse("2016-10-27", formatter)
      val txYrEndDate = LocalDate.parse("2017-04-06", formatter)
      val txYr2StartDate = LocalDate.parse("2017-04-06", formatter)
      val txYr2EndDate = LocalDate.parse("2018-04-06", formatter)

      val period1StartDate = LocalDate.parse("2016-10-27", formatter)
      val period1EndDate = LocalDate.parse("2017-04-06", formatter)

      val TxYr2period1StartDate = LocalDate.parse("2017-04-06", formatter)
      val TxYr2period1EndDate = LocalDate.parse("2017-08-27", formatter)

      val TxYr2period2StartDate = LocalDate.parse("2017-08-27", formatter)
      val TxYr2period2EndDate = LocalDate.parse("2017-09-01", formatter)

      val TxYr2period3StartDate = LocalDate.parse("2017-09-01", formatter)
      val TxYr2period3EndDate = LocalDate.parse("2018-04-06", formatter)

      val result = json.validate[TCEligibilityInput]
      result match {
        case JsSuccess(x, _) =>
          x shouldBe a[TCEligibilityInput]

          val response = TCEligibility.eligibility(x)

          response.isInstanceOf[Future[TCEligibilityModel]] shouldBe true
          response.taxYears.length shouldBe 2
          //child turns 15 in the current tax year and 16 in the next tax year
          response.taxYears.head.periods.length shouldBe 1
          response.taxYears.tail.head.periods.length shouldBe 3

          //taxYear start/end dates
          response.taxYears.head.from shouldBe txYrStartDate
          response.taxYears.head.until shouldBe txYrEndDate

          response.taxYears.tail.head.from shouldBe txYr2StartDate
          response.taxYears.tail.head.until shouldBe txYr2EndDate

          //1st Tax Year period start/end dates
          response.taxYears.head.periods.head.from shouldBe period1StartDate
          response.taxYears.head.periods.head.until shouldBe period1EndDate

          //2nd Tax Year period start/end dates
          response.taxYears.tail.head.periods.head.from shouldBe TxYr2period1StartDate
          response.taxYears.tail.head.periods.head.until shouldBe TxYr2period1EndDate

          response.taxYears.tail.head.periods.tail.head.from shouldBe TxYr2period2StartDate
          response.taxYears.tail.head.periods.tail.head.until shouldBe TxYr2period2EndDate

          response.taxYears.tail.head.periods.tail.tail.head.from shouldBe TxYr2period3StartDate
          response.taxYears.tail.head.periods.tail.tail.head.until shouldBe TxYr2period3EndDate

        case JsError(e) =>
          throw new RuntimeException(e.toList.toString())
      }
    }

    "(multiple tax year - (1st TY 2 periods) (2nd TY 2 period)) return a tax year with entitlement periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_20.json")
      val json: JsValue = Json.parse(resource.toString)

      val txYrStartDate = LocalDate.parse("2016-07-27", formatter)
      val txYrEndDate = LocalDate.parse("2017-04-06", formatter)
      val txYr2StartDate = LocalDate.parse("2017-04-06", formatter)
      val txYr2EndDate = LocalDate.parse("2018-04-06", formatter)

      val period1StartDate = LocalDate.parse("2016-07-27", formatter)
      val period1EndDate = LocalDate.parse("2016-09-01", formatter)

      val period2StartDate = LocalDate.parse("2016-09-01", formatter)
      val period2EndDate = LocalDate.parse("2017-04-06", formatter)

      val TxYr2period1StartDate = LocalDate.parse("2017-04-06", formatter)
      val TxYr2period1EndDate = LocalDate.parse("2017-09-01", formatter)

      val TxYr2period2StartDate = LocalDate.parse("2017-09-01", formatter)
      val TxYr2period2EndDate = LocalDate.parse("2018-04-06", formatter)

      val result = json.validate[TCEligibilityInput]
      result match {
        case JsSuccess(x, _) =>
          x shouldBe a[TCEligibilityInput]

          val response = TCEligibility.eligibility(x)

          response.isInstanceOf[Future[TCEligibilityModel]] shouldBe true
          response.taxYears.length shouldBe 2
          response.taxYears.head.periods.length shouldBe 2
          response.taxYears.tail.head.periods.length shouldBe 2

          //taxYear start/end dates
          response.taxYears.head.from shouldBe txYrStartDate
          response.taxYears.head.until shouldBe txYrEndDate

          response.taxYears.tail.head.from shouldBe txYr2StartDate
          response.taxYears.tail.head.until shouldBe txYr2EndDate

          //1st Tax Year period start/end dates
          response.taxYears.head.periods.head.from shouldBe period1StartDate
          response.taxYears.head.periods.head.until shouldBe period1EndDate

          response.taxYears.head.periods.tail.head.from shouldBe period2StartDate
          response.taxYears.head.periods.tail.head.until shouldBe period2EndDate

          //2nd Tax Year period start/end dates
          response.taxYears.tail.head.periods.head.from shouldBe TxYr2period1StartDate
          response.taxYears.tail.head.periods.head.until shouldBe TxYr2period1EndDate

          response.taxYears.tail.head.periods.tail.head.from shouldBe TxYr2period2StartDate
          response.taxYears.tail.head.periods.tail.head.until shouldBe TxYr2period2EndDate

        case JsError(e) =>
          throw new RuntimeException(e.toList.toString())
      }
    }

    "(multiple tax year - (1st TY 2 periods split on 15th birthday) (2nd TY 2 period)) return a tax year with entitlement periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_21.json")
      val json: JsValue = Json.parse(resource.toString)

      val txYrStartDate = LocalDate.parse("2016-07-27", formatter)
      val txYrEndDate = LocalDate.parse("2017-04-06", formatter)
      val txYr2StartDate = LocalDate.parse("2017-04-06", formatter)
      val txYr2EndDate = LocalDate.parse("2018-04-06", formatter)

      val TxYr1period1StartDate = LocalDate.parse("2016-07-27", formatter)
      val TxYr1period1EndDate = LocalDate.parse("2016-09-01", formatter)

      val TxYr1period2StartDate = LocalDate.parse("2016-09-01", formatter)
      val TxYr1period2EndDate = LocalDate.parse("2017-04-06", formatter)

      val TxYr2period1StartDate = LocalDate.parse("2017-04-06", formatter)
      val TxYr2period1EndDate = LocalDate.parse("2017-09-01", formatter)

      val TxYr2period2StartDate = LocalDate.parse("2017-09-01", formatter)
      val TxYr2period2EndDate = LocalDate.parse("2018-04-06", formatter)

      val result = json.validate[TCEligibilityInput]
      result match {
        case JsSuccess(x, _) =>
          x shouldBe a[TCEligibilityInput]

          val response = TCEligibility.eligibility(x)

          response.isInstanceOf[Future[TCEligibilityModel]] shouldBe true
          response.taxYears.length shouldBe 2
          response.taxYears.head.periods.length shouldBe 2
          response.taxYears.tail.head.periods.length shouldBe 2

          //taxYear start/end dates
          response.taxYears.head.from shouldBe txYrStartDate
          response.taxYears.head.until shouldBe txYrEndDate

          response.taxYears.tail.head.from shouldBe txYr2StartDate
          response.taxYears.tail.head.until shouldBe txYr2EndDate

          //1st Tax Year period 1 start/end dates
          response.taxYears.head.periods.head.from shouldBe TxYr1period1StartDate
          response.taxYears.head.periods.head.until shouldBe TxYr1period1EndDate

          //1st Tax Year period 2 start/end dates
          response.taxYears.head.periods.tail.head.from shouldBe TxYr1period2StartDate
          response.taxYears.head.periods.tail.head.until shouldBe TxYr1period2EndDate

          //2nd Tax Year period start/end dates
          response.taxYears.tail.head.periods.head.from shouldBe TxYr2period1StartDate
          response.taxYears.tail.head.periods.head.until shouldBe TxYr2period1EndDate

          response.taxYears.tail.head.periods.tail.head.from shouldBe TxYr2period2StartDate
          response.taxYears.tail.head.periods.tail.head.until shouldBe TxYr2period2EndDate

        case JsError(e) =>
          throw new RuntimeException(e.toList.toString())
      }
    }

    "(multiple tax year - (1st TY 1 period) (2nd TY 2 period)) return a tax year with entitlement periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_22.json")
      val json: JsValue = Json.parse(resource.toString)

      val txYrStartDate = LocalDate.parse("2016-09-10", formatter)
      val txYrEndDate = LocalDate.parse("2017-04-06", formatter)
      val txYr2StartDate = LocalDate.parse("2017-04-06", formatter)
      val txYr2EndDate = LocalDate.parse("2018-04-06", formatter)

      val TxYr1period1StartDate = LocalDate.parse("2016-09-10", formatter)
      val TxYr1period1EndDate = LocalDate.parse("2017-04-06", formatter)

      val TxYr2period1StartDate = LocalDate.parse("2017-04-06", formatter)
      val TxYr2period1EndDate = LocalDate.parse("2017-09-01", formatter)

      val TxYr2period2StartDate = LocalDate.parse("2017-09-01", formatter)
      val TxYr2period2EndDate = LocalDate.parse("2018-04-06", formatter)

      val result = json.validate[TCEligibilityInput]
      result match {
        case JsSuccess(x, _) =>
          x shouldBe a[TCEligibilityInput]

          val response = TCEligibility.eligibility(x)

          response.isInstanceOf[Future[TCEligibilityModel]] shouldBe true
          response.taxYears.length shouldBe 2
          response.taxYears.head.periods.length shouldBe 1
          response.taxYears.tail.head.periods.length shouldBe 2

          //taxYear start/end dates
          response.taxYears.head.from shouldBe txYrStartDate
          response.taxYears.head.until shouldBe txYrEndDate

          response.taxYears.tail.head.from shouldBe txYr2StartDate
          response.taxYears.tail.head.until shouldBe txYr2EndDate

          //1st Tax Year period 1 start/end dates
          response.taxYears.head.periods.head.from shouldBe TxYr1period1StartDate
          response.taxYears.head.periods.head.until shouldBe TxYr1period1EndDate

          //2nd Tax Year period start/end dates
          response.taxYears.tail.head.periods.head.from shouldBe TxYr2period1StartDate
          response.taxYears.tail.head.periods.head.until shouldBe TxYr2period1EndDate

          response.taxYears.tail.head.periods.tail.head.from shouldBe TxYr2period2StartDate
          response.taxYears.tail.head.periods.tail.head.until shouldBe TxYr2period2EndDate

        case JsError(e) =>
          throw new RuntimeException(e.toList.toString())
      }
    }

    "(multiple tax years - (1st TY 1 periods) (2nd TY 2 period - split on a child being born)) return a tax year with entitlement periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_23.json")
      val json: JsValue = Json.parse(resource.toString)

      val txYrStartDate = LocalDate.parse("2016-09-10", formatter)
      val txYrEndDate = LocalDate.parse("2017-04-06", formatter)
      val txYr2StartDate = LocalDate.parse("2017-04-06", formatter)
      val txYr2EndDate = LocalDate.parse("2018-04-06", formatter)

      val txyr1period1StartDate = LocalDate.parse("2016-09-10", formatter)
      val txyr1period1EndDate = LocalDate.parse("2016-09-12", formatter)

      val txyr1period2StartDate = LocalDate.parse("2016-09-12", formatter)
      val txyr1period2EndDate = LocalDate.parse("2017-04-06", formatter)

      val TxYr2period1StartDate = LocalDate.parse("2017-04-06", formatter)
      val TxYr2period1EndDate = LocalDate.parse("2018-04-06", formatter)

      val result = json.validate[TCEligibilityInput]
      result match {
        case JsSuccess(x, _) =>
          x shouldBe a[TCEligibilityInput]

          val response = TCEligibility.eligibility(x)

          response.isInstanceOf[Future[TCEligibilityModel]] shouldBe true
          response.taxYears.length shouldBe 2
          response.taxYears.head.periods.length shouldBe 2
          response.taxYears.tail.head.periods.length shouldBe 1

          //taxYear start/end dates
          response.taxYears.head.from shouldBe txYrStartDate
          response.taxYears.head.until shouldBe txYrEndDate

          response.taxYears.tail.head.from shouldBe txYr2StartDate
          response.taxYears.tail.head.until shouldBe txYr2EndDate

          //1st Tax Year period start/end dates
          response.taxYears.head.periods.head.from shouldBe txyr1period1StartDate
          response.taxYears.head.periods.head.until shouldBe txyr1period1EndDate

          response.taxYears.head.periods.tail.head.from shouldBe txyr1period2StartDate
          response.taxYears.head.periods.tail.head.until shouldBe txyr1period2EndDate

          //2nd Tax Year period start/end dates
          response.taxYears.tail.head.periods.head.from shouldBe TxYr2period1StartDate
          response.taxYears.tail.head.periods.head.until shouldBe TxYr2period1EndDate

        case JsError(e) =>
          throw new RuntimeException(e.toList.toString())
      }
    }

    "(multiple tax year - (1st TY 1 period) (2nd TY 3 periods)(child being born in second tax year)) return a tax year with entitlement periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_24.json")
      val json: JsValue = Json.parse(resource.toString)

      val txYrStartDate = LocalDate.parse("2016-09-10", formatter)
      val txYrEndDate = LocalDate.parse("2017-04-06", formatter)
      val txYr2StartDate = LocalDate.parse("2017-04-06", formatter)
      val txYr2EndDate = LocalDate.parse("2018-04-06", formatter)

      val TxYr1period1StartDate = LocalDate.parse("2016-09-10", formatter)
      val TxYr1period1EndDate = LocalDate.parse("2017-04-06", formatter)

      val TxYr2period1StartDate = LocalDate.parse("2017-04-06", formatter)
      val TxYr2period1EndDate = LocalDate.parse("2017-05-10", formatter)

      val TxYr2period2StartDate = LocalDate.parse("2017-05-10", formatter)
      val TxYr2period2EndDate = LocalDate.parse("2017-09-01", formatter)

      val TxYr2period3StartDate = LocalDate.parse("2017-09-01", formatter)
      val TxYr2period3EndDate = LocalDate.parse("2018-04-06", formatter)

      val result = json.validate[TCEligibilityInput]
      result match {
        case JsSuccess(x, _) =>
          x shouldBe a[TCEligibilityInput]

          val response = TCEligibility.eligibility(x)

          response.isInstanceOf[Future[TCEligibilityModel]] shouldBe true
          response.taxYears.length shouldBe 2
          response.taxYears.head.periods.length shouldBe 1
          response.taxYears.tail.head.periods.length shouldBe 3

          //taxYear start/end dates
          response.taxYears.head.from shouldBe txYrStartDate
          response.taxYears.head.until shouldBe txYrEndDate

          response.taxYears.tail.head.from shouldBe txYr2StartDate
          response.taxYears.tail.head.until shouldBe txYr2EndDate

          //1st Tax Year period 1 start/end dates
          response.taxYears.head.periods.head.from shouldBe TxYr1period1StartDate
          response.taxYears.head.periods.head.until shouldBe TxYr1period1EndDate

          //2nd Tax Year period start/end dates
          response.taxYears.tail.head.periods.head.from shouldBe TxYr2period1StartDate
          response.taxYears.tail.head.periods.head.until shouldBe TxYr2period1EndDate

          response.taxYears.tail.head.periods.tail.head.from shouldBe TxYr2period2StartDate
          response.taxYears.tail.head.periods.tail.head.until shouldBe TxYr2period2EndDate

          response.taxYears.tail.head.periods.tail.tail.head.from shouldBe TxYr2period3StartDate
          response.taxYears.tail.head.periods.tail.tail.head.until shouldBe TxYr2period3EndDate

        case JsError(e) =>
          throw new RuntimeException(e.toList.toString())
      }
    }

    "populate the child's elements model for a period (3 children: 1st < 16, 2nd is 15, 3rd is 19)" in {
      val dateOfBirth1 = LocalDate.parse("2004-08-31", formatter)
      val dateOfBirth2 = LocalDate.parse("2001-08-31", formatter)
      val dateOfBirth3 = LocalDate.parse("1997-08-31", formatter)
      val periodStartDate = LocalDate.parse("2016-08-31", formatter)
      val educationStartDate = LocalDate.parse("2014-09-05", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child2 = Child(id = 1, childcareCost = BigDecimal(99.21), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth2, disability = Disability(disabled = true, severelyDisabled = false), education = None)
      val child3 = Child(id = 2, childcareCost = BigDecimal(0.01), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth3, disability = Disability(disabled = true, severelyDisabled = true), education = Some(Education(inEducation = true, startDate = educationStartDate)))

      val result = TCEligibility.determineChildrenEligibilityForPeriod(List(child1, child2, child3), periodStartDate)

      val outputChild1 = models.output.tc.OutputChild(childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = false, severeDisability = false, childcare = true))
      val outputChild2 = models.output.tc.OutputChild(childcareCost = BigDecimal(99.21), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = true, severeDisability = false, childcare = true))
      val outputChild3 = models.output.tc.OutputChild(childcareCost = BigDecimal(0.01), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = false, youngAdult = true, disability = true, severeDisability = true))

      result shouldBe List(outputChild3, outputChild2, outputChild1)
    }

    "populate the child's elements model for a period (3 children: 1st < 16, 2nd is 15, 3rd is 19)(1st September)" in {
      val dateOfBirth1 = LocalDate.parse("2004-08-31", formatter)
      val dateOfBirth2 = LocalDate.parse("2001-08-31", formatter)
      val dateOfBirth3 = LocalDate.parse("1997-08-31", formatter)
      val periodStartDate = LocalDate.parse("2016-09-01", formatter)
      val educationStartDate = LocalDate.parse("2014-09-05", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child2 = Child(id = 1, childcareCost = BigDecimal(99.21), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth2, disability = Disability(disabled = true, severelyDisabled = false), education = None)
      val child3 = Child(id = 2, childcareCost = BigDecimal(0.01), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth3, disability = Disability(disabled = true, severelyDisabled = true), education = Some(Education(inEducation = true, startDate = educationStartDate)))

      val result = TCEligibility.determineChildrenEligibilityForPeriod(List(child1, child2, child3), periodStartDate)

      val outputChild1 = models.output.tc.OutputChild(childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = false, severeDisability = false, childcare = true))
      val outputChild2 = models.output.tc.OutputChild(childcareCost = BigDecimal(99.21), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = true, severeDisability = false, childcare = true))
      val outputChild3 = models.output.tc.OutputChild(childcareCost = BigDecimal(0.01), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = false, youngAdult = true, disability = true, severeDisability = true))

      result shouldBe List(outputChild3, outputChild2, outputChild1)
    }

    "populate the child's elements model for a period (3 children: 1st < 16, 2nd is 16, 3rd is 20)" in {
      val dateOfBirth1 = LocalDate.parse("2004-08-31", formatter)
      val dateOfBirth2 = LocalDate.parse("2001-08-31", formatter)
      val dateOfBirth3 = LocalDate.parse("1997-08-31", formatter)
      val periodStartDate = LocalDate.parse("2017-08-31", formatter)
      val educationStartDate = LocalDate.parse("2014-09-05", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child2 = Child(id = 1, childcareCost = BigDecimal(99.21), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth2, disability = Disability(disabled = true, severelyDisabled = false), education = None)
      val child3 = Child(id = 2, childcareCost = BigDecimal(0.01), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth3, disability = Disability(disabled = true, severelyDisabled = true), education = Some(Education(inEducation = true, startDate = educationStartDate)))

      val result = TCEligibility.determineChildrenEligibilityForPeriod(List(child1, child2, child3), periodStartDate)

      val outputChild1 = models.output.tc.OutputChild(childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = false, severeDisability = false, childcare = true))
      val outputChild2 = models.output.tc.OutputChild(childcareCost = BigDecimal(99.21), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = true, severeDisability = false, childcare = true))
      val outputChild3 = models.output.tc.OutputChild(childcareCost = BigDecimal(0.01), childcareCostPeriod = Periods.Monthly, qualifying = false, childElements = ChildElements(child = false, youngAdult = false, disability = false, severeDisability = false))

      result shouldBe List(outputChild3, outputChild2, outputChild1)
    }

    "get child's element for all childen born before 6.4.2017" in {
      val dateOfBirth1 = LocalDate.parse("2008-02-01", formatter)
      val dateOfBirth2 = LocalDate.parse("2011-06-15", formatter)
      val dateOfBirth3 = LocalDate.parse("2013-12-26", formatter)
      val dateOfBirth4 = LocalDate.parse("2017-04-06", formatter)


      val periodStartDate = LocalDate.parse("2020-04-06", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child2 = Child(id = 1, childcareCost = BigDecimal(99.21), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth2, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child3 = Child(id = 2, childcareCost = BigDecimal(0.01), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth3, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child4 = Child(id = 3, childcareCost = BigDecimal(0.01), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth4, disability = Disability(disabled = false, severelyDisabled = false), education = None)

      val result = TCEligibility.determineChildrenEligibilityForPeriod(List(child1, child2, child3, child4), periodStartDate)

      val outputChild1 = models.output.tc.OutputChild(childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = false, severeDisability = false, childcare = true))
      val outputChild2 = models.output.tc.OutputChild(childcareCost = BigDecimal(99.21), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = false, severeDisability = false, childcare = true))
      val outputChild3 = models.output.tc.OutputChild(childcareCost = BigDecimal(0.01), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = false, severeDisability = false, childcare = true))
      val outputChild4 = models.output.tc.OutputChild(childcareCost = BigDecimal(0.01), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = false, youngAdult = false, disability = false, severeDisability = false, childcare = true))

      result shouldBe List(outputChild1, outputChild2, outputChild3, outputChild4)
    }

    "restrict child's element for 2 childen born after 6.4.2017" in {
      val dateOfBirth1 = LocalDate.parse("2017-05-01", formatter)
      val dateOfBirth2 = LocalDate.parse("2018-07-05", formatter)
      val dateOfBirth3 = LocalDate.parse("2019-06-08", formatter)

      val periodStartDate = LocalDate.parse("2020-04-06", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child2 = Child(id = 1, childcareCost = BigDecimal(99.21), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth2, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child3 = Child(id = 2, childcareCost = BigDecimal(0.01), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth3, disability = Disability(disabled = false, severelyDisabled = false), education = None)

      val result = TCEligibility.determineChildrenEligibilityForPeriod(List(child1, child2, child3), periodStartDate)

      val outputChild1 = models.output.tc.OutputChild(childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = false, severeDisability = false, childcare = true))
      val outputChild2 = models.output.tc.OutputChild(childcareCost = BigDecimal(99.21), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = false, severeDisability = false, childcare = true))
      val outputChild3 = models.output.tc.OutputChild(childcareCost = BigDecimal(0.01), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = false, youngAdult = false, disability = false, severeDisability = false, childcare = true))

      result shouldBe List(outputChild1, outputChild2, outputChild3)
    }

    "child's element is paid for 2 childen" in {

      val dateOfBirth1 = LocalDate.parse("2013-02-19", formatter)
      val dateOfBirth2 = LocalDate.parse("2017-07-05", formatter)

      val periodStartDate = LocalDate.parse("2018-04-06", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child2 = Child(id = 1, childcareCost = BigDecimal(99.21), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth2, disability = Disability(disabled = false, severelyDisabled = false), education = None)

      val result = TCEligibility.determineChildrenEligibilityForPeriod(List(child1, child2), periodStartDate)

      val outputChild1 = models.output.tc.OutputChild(childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = false, severeDisability = false, childcare = true))
      val outputChild2 = models.output.tc.OutputChild(childcareCost = BigDecimal(99.21), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = false, severeDisability = false, childcare = true))

      result shouldBe List(outputChild1, outputChild2)
    }

    "multiple births are treated as one if there is no more of one child before that" in {

      val dateOfBirth1 = LocalDate.parse("2017-05-01", formatter)
      val dateOfBirth2 = LocalDate.parse("2018-10-10", formatter)
      val dateOfBirth3 = LocalDate.parse("2018-10-10", formatter)
      val dateOfBirth4 = LocalDate.parse("2019-04-06", formatter)

      val periodStartDate = LocalDate.parse("2020-04-06", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child2 = Child(id = 1, childcareCost = BigDecimal(99.21), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth2, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child3 = Child(id = 2, childcareCost = BigDecimal(0.01), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth3, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child4 = Child(id = 3, childcareCost = BigDecimal(0.01), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth4, disability = Disability(disabled = false, severelyDisabled = false), education = None)

      val result = TCEligibility.determineChildrenEligibilityForPeriod(List(child1, child2, child3, child4), periodStartDate)

      val outputChild1 = models.output.tc.OutputChild(childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = false, severeDisability = false, childcare = true))
      val outputChild2 = models.output.tc.OutputChild(childcareCost = BigDecimal(99.21), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = false, severeDisability = false, childcare = true))
      val outputChild3 = models.output.tc.OutputChild(childcareCost = BigDecimal(0.01), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = false, severeDisability = false, childcare = true))
      val outputChild4 = models.output.tc.OutputChild(childcareCost = BigDecimal(0.01), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = false, youngAdult = false, disability = false, severeDisability = false, childcare = true))

      result shouldBe List(outputChild1, outputChild2, outputChild3, outputChild4)
    }

    "multiple births are treated as separate if there are no children born before that" in {

      val dateOfBirth1 = LocalDate.parse("2018-10-10", formatter)
      val dateOfBirth2 = LocalDate.parse("2018-10-10", formatter)
      val dateOfBirth3 = LocalDate.parse("2019-04-06", formatter)

      val periodStartDate = LocalDate.parse("2020-04-06", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child2 = Child(id = 1, childcareCost = BigDecimal(99.21), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth2, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child3 = Child(id = 2, childcareCost = BigDecimal(0.01), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth3, disability = Disability(disabled = false, severelyDisabled = false), education = None)

      val result = TCEligibility.determineChildrenEligibilityForPeriod(List(child1, child2, child3), periodStartDate)

      val outputChild1 = models.output.tc.OutputChild(childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = false, severeDisability = false, childcare = true))
      val outputChild2 = models.output.tc.OutputChild(childcareCost = BigDecimal(99.21), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = false, severeDisability = false, childcare = true))
      val outputChild3 = models.output.tc.OutputChild(childcareCost = BigDecimal(0.01), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = false, youngAdult = false, disability = false, severeDisability = false, childcare = true))

      result shouldBe List(outputChild1, outputChild2, outputChild3)
    }

    "multiple births don't receive child element if there are two children born before that" in {

      val dateOfBirth1 = LocalDate.parse("2017-05-01", formatter)
      val dateOfBirth2 = LocalDate.parse("2018-10-10", formatter)
      val dateOfBirth3 = LocalDate.parse("2019-04-06", formatter)
      val dateOfBirth4 = LocalDate.parse("2019-04-06", formatter)

      val periodStartDate = LocalDate.parse("2020-04-06", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child2 = Child(id = 1, childcareCost = BigDecimal(99.21), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth2, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child3 = Child(id = 2, childcareCost = BigDecimal(0.01), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth3, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child4 = Child(id = 3, childcareCost = BigDecimal(0.01), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth4, disability = Disability(disabled = false, severelyDisabled = false), education = None)

      val result = TCEligibility.determineChildrenEligibilityForPeriod(List(child1, child2, child3, child4), periodStartDate)

      val outputChild1 = models.output.tc.OutputChild(childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = false, severeDisability = false, childcare = true))
      val outputChild2 = models.output.tc.OutputChild(childcareCost = BigDecimal(99.21), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = false, severeDisability = false, childcare = true))
      val outputChild3 = models.output.tc.OutputChild(childcareCost = BigDecimal(0.01), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = false, youngAdult = false, disability = false, severeDisability = false, childcare = true))
      val outputChild4 = models.output.tc.OutputChild(childcareCost = BigDecimal(0.01), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = false, youngAdult = false, disability = false, severeDisability = false, childcare = true))

      result shouldBe List(outputChild1, outputChild2, outputChild3, outputChild4)
    }

    "populate the claimant's elements model for a period (1 claimant, non-disabled, qualifying)" in {
      val periodStartDate = LocalDate.parse("2017-08-31", formatter)

      val claimant = Claimant(isPartner = false, hours = 16, disability = Disability(disabled = false, severelyDisabled = false), carersAllowance = false)
      val ty = TaxYear(from = periodStartDate, until = periodStartDate, claimants = List(claimant), children = List())

      val outputClaimant = models.output.tc.OutputClaimant(qualifying = true, isPartner = false, claimantDisability = ClaimantDisability(disability = false, severeDisability = false))

      val result = TCEligibility.determineClaimantsEligibilityForPeriod(ty)
      result shouldBe List(outputClaimant)
    }

    "populate the claimant's elements model for a period (1 claimant, disabled, working > 16h, qualifying)" in {
      val periodStartDate = LocalDate.parse("2017-08-31", formatter)

      val claimant = Claimant(isPartner = false, hours = 17, disability = Disability(disabled = true, severelyDisabled = false), carersAllowance = false)
      val ty = TaxYear(from = periodStartDate, until = periodStartDate, claimants = List(claimant), children = List())

      val outputClaimant = models.output.tc.OutputClaimant(qualifying = true, isPartner = false, claimantDisability = ClaimantDisability(disability = true, severeDisability = false))

      val result = TCEligibility.determineClaimantsEligibilityForPeriod(ty)
      result shouldBe List(outputClaimant)
    }

    "populate the claimant's elements model for a period (1 claimant, disabled, working < 16h, qualifying)" in {
      val periodStartDate = LocalDate.parse("2017-08-31", formatter)

      val claimant = Claimant(isPartner = false, hours = 14, disability = Disability(disabled = true, severelyDisabled = false), carersAllowance = false)

      val ty = TaxYear(from = periodStartDate, until = periodStartDate, claimants = List(claimant), children = List())
      val outputClaimant = models.output.tc.OutputClaimant(qualifying = true, isPartner = false, claimantDisability = ClaimantDisability(disability = false, severeDisability = false))

      val result = TCEligibility.determineClaimantsEligibilityForPeriod(ty)
      result shouldBe List(outputClaimant)
    }

    "populate the claimant's elements model for a period (1 claimant, severely disabled, working < 16h, qualifying)" in {
      val periodStartDate = LocalDate.parse("2017-08-31", formatter)

      val claimant = Claimant(isPartner = false, hours = 14, disability = Disability(disabled = true, severelyDisabled = true), carersAllowance = false)

      val ty = TaxYear(from = periodStartDate, until = periodStartDate, claimants = List(claimant), children = List())
      val outputClaimant = models.output.tc.OutputClaimant(qualifying = true, isPartner = false, claimantDisability = ClaimantDisability(disability = false, severeDisability = false))

      val result = TCEligibility.determineClaimantsEligibilityForPeriod(ty)
      result shouldBe List(outputClaimant)
    }

    "populate the claimant's elements model for a period (1 claimant, severely disabled, working > 16h, qualifying)" in {
      val periodStartDate = LocalDate.parse("2017-08-31", formatter)

      val claimant = Claimant(isPartner = false, hours = 16, disability = Disability(disabled = true, severelyDisabled = true), carersAllowance = false)

      val ty = TaxYear(from = periodStartDate, until = periodStartDate, claimants = List(claimant), children = List())
      val outputClaimant = models.output.tc.OutputClaimant(qualifying = true, isPartner = false, claimantDisability = ClaimantDisability(disability = true, severeDisability = true))

      val result = TCEligibility.determineClaimantsEligibilityForPeriod(ty)
      result shouldBe List(outputClaimant)
    }

    "populate the claimant's elements model for a period (couple, 1 non-disabled, 2nd disabled, qualifying)" in {
      val periodStartDate = LocalDate.parse("2017-08-31", formatter)

      val claimant = Claimant(isPartner = false, hours = 14, disability = Disability(disabled = false, severelyDisabled = false), carersAllowance = false)
      val claimant1 = Claimant(isPartner = true, hours = 20, disability = Disability(disabled = true, severelyDisabled = false), carersAllowance = false)

      val ty = TaxYear(from = periodStartDate, until = periodStartDate, claimants = List(claimant, claimant1), children = List())

      val outputClaimant = models.output.tc.OutputClaimant(qualifying = true, isPartner = false, claimantDisability = ClaimantDisability(disability = false, severeDisability = false))
      val outputClaimant1 = models.output.tc.OutputClaimant(qualifying = true, isPartner = true, claimantDisability = ClaimantDisability(disability = true, severeDisability = false))

      val result = TCEligibility.determineClaimantsEligibilityForPeriod(ty)
      result shouldBe List(outputClaimant, outputClaimant1)
    }

    "populate the claimant's elements model for a period (couple, 1 non-disabled > 16h, 2nd severely disabled <16h, qualifying)" in {
      val periodStartDate = LocalDate.parse("2017-08-31", formatter)

      val claimant = Claimant(isPartner = false, hours = 16, disability = Disability(disabled = false, severelyDisabled = false), carersAllowance = false)
      val claimant1 = Claimant(isPartner = true, hours = 0, disability = Disability(disabled = true, severelyDisabled = true), carersAllowance = false)

      val ty = TaxYear(from = periodStartDate, until = periodStartDate, claimants = List(claimant, claimant1), children = List())

      val outputClaimant = models.output.tc.OutputClaimant(qualifying = true, isPartner = false, claimantDisability = ClaimantDisability(disability = false, severeDisability = false))
      val outputClaimant1 = models.output.tc.OutputClaimant(qualifying = true, isPartner = true, claimantDisability = ClaimantDisability(disability = false, severeDisability = true))

      val result = TCEligibility.determineClaimantsEligibilityForPeriod(ty)
      result shouldBe List(outputClaimant, outputClaimant1)
    }

    "populate the claimant's elements model for a period (couple, 1 severely disabled, 2nd non-disabled, qualifying)" in {
      val periodStartDate = LocalDate.parse("2017-08-31", formatter)

      val claimant = Claimant(isPartner = false, hours = 16, disability = Disability(disabled = false, severelyDisabled = false), carersAllowance = false)
      val claimant1 = Claimant(isPartner = true, hours = 10, disability = Disability(disabled = true, severelyDisabled = true), carersAllowance = false)

      val ty = TaxYear(from = periodStartDate, until = periodStartDate, claimants = List(claimant, claimant1), children = List())

      val outputClaimant = models.output.tc.OutputClaimant(qualifying = true, isPartner = false, claimantDisability = ClaimantDisability(disability = false, severeDisability = false))
      val outputClaimant1 = models.output.tc.OutputClaimant(qualifying = true, isPartner = true, claimantDisability = ClaimantDisability(disability = false, severeDisability = true))

      val result = TCEligibility.determineClaimantsEligibilityForPeriod(ty)
      result shouldBe List(outputClaimant, outputClaimant1)
    }

    "populate the claimant's elements model for a period (couple, both severely disabled, one working < 16, other > 16, qualifying)" in {
      val periodStartDate = LocalDate.parse("2017-08-31", formatter)

      val claimant = Claimant(isPartner = false, hours = 2, disability = Disability(disabled = true, severelyDisabled = true), carersAllowance = false)
      val claimant1 = Claimant(isPartner = true, hours = 16, disability = Disability(disabled = true, severelyDisabled = true), carersAllowance = false)

      val ty = TaxYear(from = periodStartDate, until = periodStartDate, claimants = List(claimant, claimant1), children = List())

      val outputClaimant = models.output.tc.OutputClaimant(qualifying = true, isPartner = false, claimantDisability = ClaimantDisability(disability = false, severeDisability = true))
      val outputClaimant1 = models.output.tc.OutputClaimant(qualifying = true, isPartner = true, claimantDisability = ClaimantDisability(disability = true, severeDisability = true))

      val result = TCEligibility.determineClaimantsEligibilityForPeriod(ty)
      result shouldBe List(outputClaimant, outputClaimant1)
    }

    "populate the household elements model for a period (single claimant, 1 qualifying child, working 30h, non-disabled)" in {
      val dateOfBirth1 = LocalDate.parse("2004-08-31", formatter)
      val periodStartDate = LocalDate.parse("2016-09-01", formatter)
      val periodEndDate = LocalDate.parse("2016-12-01", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val claimant1 = Claimant(isPartner = false, hours = 30, disability = Disability(disabled = false, severelyDisabled = false), carersAllowance = false)
      val ty = models.input.tc.TaxYear(from = periodStartDate, until = periodEndDate, claimants = List(claimant1), children = List(child1))

      val outputHousehold = models.output.tc.HouseholdElements(basic = true, hours30 = true, childcare = true, loneParent = true, secondParent = false, family = true, wtc = true, ctc = true)

      val result = TCEligibility.determineHouseholdEligibilityForPeriod(ty, periodStartDate)
      result shouldBe outputHousehold
    }

    "populate the household elements model for a period (single claimant, 1 qualifying child, working 16h, disabled)" in {
      val dateOfBirth1 = LocalDate.parse("2004-08-31", formatter)
      val periodStartDate = LocalDate.parse("2016-09-01", formatter)
      val periodEndDate = LocalDate.parse("2016-12-01", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val claimant1 = Claimant(isPartner = false, hours = 16, disability = Disability(disabled = true, severelyDisabled = false), carersAllowance = false)
      val ty = models.input.tc.TaxYear(from = periodStartDate, until = periodEndDate, claimants = List(claimant1), children = List(child1))

      val outputHousehold = models.output.tc.HouseholdElements(basic = true, hours30 = false, childcare = true, loneParent = true, secondParent = false, family = true, wtc = true, ctc = true)

      val result = TCEligibility.determineHouseholdEligibilityForPeriod(ty, periodStartDate)
      result shouldBe outputHousehold
    }

    "populate the household elements model for a period (single claimant, 1 qualifying child, working 1h, severely disabled)" in {
      val dateOfBirth1 = LocalDate.parse("2004-08-31", formatter)
      val periodStartDate = LocalDate.parse("2016-09-01", formatter)
      val periodEndDate = LocalDate.parse("2016-12-01", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val claimant1 = Claimant(isPartner = false, hours = 1, disability = Disability(disabled = true, severelyDisabled = true), carersAllowance = false)
      val ty = models.input.tc.TaxYear(from = periodStartDate, until = periodEndDate, claimants = List(claimant1), children = List(child1))

      val outputHousehold = models.output.tc.HouseholdElements(basic = false, hours30 = false, childcare = false, loneParent = true, secondParent = false, family = true, wtc = false, ctc = true)

      val result = TCEligibility.determineHouseholdEligibilityForPeriod(ty, periodStartDate)
      result shouldBe outputHousehold
    }

    "populate the household elements model for a period (single claimant, 1 non qualifying child (> 16), working 1h, severely disabled)" in {
      val dateOfBirth1 = LocalDate.parse("1993-08-31", formatter)
      val periodStartDate = LocalDate.parse("2016-09-01", formatter)
      val periodEndDate = LocalDate.parse("2016-12-01", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val claimant1 = Claimant(isPartner = false, hours = 1, disability = Disability(disabled = true, severelyDisabled = true), carersAllowance = false)
      val ty = models.input.tc.TaxYear(from = periodStartDate, until = periodEndDate, claimants = List(claimant1), children = List(child1))

      val outputHousehold = models.output.tc.HouseholdElements(basic = false, hours30 = false, childcare = false, loneParent = false, secondParent = false, family = false, wtc = false, ctc = false)

      val result = TCEligibility.determineHouseholdEligibilityForPeriod(ty, periodStartDate)
      result shouldBe outputHousehold
    }

    "populate the household elements model for a period (single claimant, 1 qualifying young adult (< 19), working 16h, non disabled)" in {
      val dateOfBirth1 = LocalDate.parse("1998-08-31", formatter)
      val periodStartDate = LocalDate.parse("2016-09-01", formatter)
      val periodEndDate = LocalDate.parse("2016-12-01", formatter)
      val educationStartDate = LocalDate.parse("2011-09-05", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
      val claimant1 = Claimant(isPartner = false, hours = 16, disability = Disability(disabled = false, severelyDisabled = false), carersAllowance = false)
      val ty = models.input.tc.TaxYear(from = periodStartDate, until = periodEndDate, claimants = List(claimant1), children = List(child1))

      val outputHousehold = models.output.tc.HouseholdElements(basic = true, hours30 = false, childcare = false, loneParent = true, secondParent = false, family = true, wtc = true, ctc = true)

      val result = TCEligibility.determineHouseholdEligibilityForPeriod(ty, periodStartDate)
      result shouldBe outputHousehold
    }

    "populate the household elements model for a period (single claimant, 1 non qualifying child (> 16), working 16h, severely disabled)" in {
      val dateOfBirth1 = LocalDate.parse("1999-08-31", formatter)
      val periodStartDate = LocalDate.parse("2016-09-01", formatter)
      val periodEndDate = LocalDate.parse("2016-12-01", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val claimant1 = Claimant(isPartner = false, hours = 16, disability = Disability(disabled = true, severelyDisabled = true), carersAllowance = false)
      val ty = models.input.tc.TaxYear(from = periodStartDate, until = periodEndDate, claimants = List(claimant1), children = List(child1))

      val outputHousehold = models.output.tc.HouseholdElements(basic = false, hours30 = false, childcare = false, loneParent = false, secondParent = false, family = false, wtc = true, ctc = false)

      val result = TCEligibility.determineHouseholdEligibilityForPeriod(ty, periodStartDate)
      result shouldBe outputHousehold
    }

    "populate the household elements model for a period (single claimant, 1 non qualifying child (> 15), 1 qualifying child (< 15), working 16h, severely disabled)" in {
      val dateOfBirth1 = LocalDate.parse("1999-08-31", formatter)
      val dateOfBirth2 = LocalDate.parse("2010-08-31", formatter)
      val periodStartDate = LocalDate.parse("2016-09-01", formatter)
      val periodEndDate = LocalDate.parse("2016-12-01", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val child2 = Child(id = 1, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth2, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val claimant1 = Claimant(isPartner = false, hours = 16, disability = Disability(disabled = true, severelyDisabled = true), carersAllowance = false)
      val ty = models.input.tc.TaxYear(from = periodStartDate, until = periodEndDate, claimants = List(claimant1), children = List(child1, child2))

      val outputHousehold = models.output.tc.HouseholdElements(basic = true, hours30 = false, childcare = true, loneParent = true, secondParent = false, family = true, wtc = true, ctc = true)

      val result = TCEligibility.determineHouseholdEligibilityForPeriod(ty, periodStartDate)
      result shouldBe outputHousehold
    }

    "populate the household elements model for a period (couple, 1st claimant > 16h, non disabled, 2nd claimant 8h, non disabled, 1 qualifying child)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("2003-08-31", formatter)
      val periodStartDate = LocalDate.parse("2016-09-01", formatter)
      val periodEndDate = LocalDate.parse("2016-12-01", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val claimant1 = Claimant(isPartner = false, hours = 16, disability = Disability(disabled = false, severelyDisabled = false), carersAllowance = false)
      val claimant2 = Claimant(isPartner = true, hours = 8, disability = Disability(disabled = false, severelyDisabled = false), carersAllowance = false)
      val ty = models.input.tc.TaxYear(from = periodStartDate, until = periodEndDate, claimants = List(claimant1, claimant2), children = List(child1))

      val outputHousehold = models.output.tc.HouseholdElements(basic = true, hours30 = false, childcare = false, loneParent = false, secondParent = true, family = true, wtc = true, ctc = true)

      val result = TCEligibility.determineHouseholdEligibilityForPeriod(ty, periodStartDate)
      result shouldBe outputHousehold
    }

    "populate the household elements model for a period (couple, 1st claimant > 16h, non disabled, 2nd claimant 0h, non disabled, 1 qualifying child)" in {
      val dateOfBirth1 = LocalDate.parse("2003-08-31", formatter)
      val periodStartDate = LocalDate.parse("2016-09-01", formatter)
      val periodEndDate = LocalDate.parse("2016-12-01", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val claimant1 = Claimant(isPartner = false, hours = 16, disability = Disability(disabled = false, severelyDisabled = false), carersAllowance = false)
      val claimant2 = Claimant(isPartner = true, hours = 0, disability = Disability(disabled = false, severelyDisabled = false), carersAllowance = false)
      val ty = models.input.tc.TaxYear(from = periodStartDate, until = periodEndDate, claimants = List(claimant1, claimant2), children = List(child1))

      val outputHousehold = models.output.tc.HouseholdElements(basic = false, hours30 = false, childcare = false, loneParent = false, secondParent = false, family = true, wtc = false, ctc = true)

      val result = TCEligibility.determineHouseholdEligibilityForPeriod(ty, periodStartDate)
      result shouldBe outputHousehold
    }

    "populate the household elements model for a period (couple, 1st claimant > 16h, non disabled, 2nd claimant 16h, non disabled, 1 qualifying child)" in {
      val dateOfBirth1 = LocalDate.parse("2003-08-31", formatter)
      val periodStartDate = LocalDate.parse("2016-09-01", formatter)
      val periodEndDate = LocalDate.parse("2016-12-01", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val claimant1 = Claimant(isPartner = false, hours = 16, disability = Disability(disabled = false, severelyDisabled = false), carersAllowance = false)
      val claimant2 = Claimant(isPartner = true, hours = 16, disability = Disability(disabled = false, severelyDisabled = false), carersAllowance = false)
      val ty = models.input.tc.TaxYear(from = periodStartDate, until = periodEndDate, claimants = List(claimant1, claimant2), children = List(child1))

      val outputHousehold = models.output.tc.HouseholdElements(basic = true, hours30 = true, childcare = true, loneParent = false, secondParent = true, family = true, wtc = true, ctc = true)

      val result = TCEligibility.determineHouseholdEligibilityForPeriod(ty, periodStartDate)
      result shouldBe outputHousehold
    }

    "populate the household elements model for a period (couple, 1st claimant > 16h, non disabled, 2nd claimant 0h severely disabled, 1 qualifying child)" in {
      val dateOfBirth1 = LocalDate.parse("2003-08-31", formatter)
      val periodStartDate = LocalDate.parse("2016-09-01", formatter)
      val periodEndDate = LocalDate.parse("2016-12-01", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val claimant1 = Claimant(isPartner = false, hours = 16, disability = Disability(disabled = false, severelyDisabled = false), carersAllowance = false)
      val claimant2 = Claimant(isPartner = true, hours = 0, disability = Disability(disabled = true, severelyDisabled = true), carersAllowance = false)
      val ty = models.input.tc.TaxYear(from = periodStartDate, until = periodEndDate, claimants = List(claimant1, claimant2), children = List(child1))

      val outputHousehold = models.output.tc.HouseholdElements(basic = true, hours30 = false, childcare = true, loneParent = false, secondParent = true, family = true, wtc = true, ctc = true)

      val result = TCEligibility.determineHouseholdEligibilityForPeriod(ty, periodStartDate)
      result shouldBe outputHousehold
    }

    "populate the household elements model for a period (couple, 1st claimant > 16h, non disabled, 2nd claimant 0h disabled, 1 qualifying child)" in {
      val dateOfBirth1 = LocalDate.parse("2003-08-31", formatter)
      val periodStartDate = LocalDate.parse("2016-09-01", formatter)
      val periodEndDate = LocalDate.parse("2016-12-01", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val claimant1 = Claimant(isPartner = false, hours = 16, disability = Disability(disabled = false, severelyDisabled = false), carersAllowance = false)
      val claimant2 = Claimant(isPartner = true, hours = 0, disability = Disability(disabled = true, severelyDisabled = true), carersAllowance = false)
      val ty = models.input.tc.TaxYear(from = periodStartDate, until = periodEndDate, claimants = List(claimant1, claimant2), children = List(child1))

      val outputHousehold = models.output.tc.HouseholdElements(basic = true, hours30 = false, childcare = true, loneParent = false, secondParent = true, family = true, wtc = true, ctc = true)

      val result = TCEligibility.determineHouseholdEligibilityForPeriod(ty, periodStartDate)
      result shouldBe outputHousehold
    }

    "populate the household elements model for a period (couple, 1st claimant 0h, non disabled, 2nd claimant 0h severely disabled, 1 qualifying child)" in {
      val dateOfBirth1 = LocalDate.parse("2003-08-31", formatter)
      val periodStartDate = LocalDate.parse("2016-09-01", formatter)
      val periodEndDate = LocalDate.parse("2016-12-01", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val claimant1 = Claimant(isPartner = false, hours = 0, disability = Disability(disabled = false, severelyDisabled = false), carersAllowance = false)
      val claimant2 = Claimant(isPartner = true, hours = 0, disability = Disability(disabled = true, severelyDisabled = true), carersAllowance = false)
      val ty = models.input.tc.TaxYear(from = periodStartDate, until = periodEndDate, claimants = List(claimant1, claimant2), children = List(child1))

      val outputHousehold = models.output.tc.HouseholdElements(basic = false, hours30 = false, childcare = false, loneParent = false, secondParent = false, family = true, wtc = false, ctc = true)

      val result = TCEligibility.determineHouseholdEligibilityForPeriod(ty, periodStartDate)
      result shouldBe outputHousehold
    }

    def buildTaxYear(wtcCTCEligibility: List[(Boolean, Boolean)]) = {
      val periodStartDate = LocalDate.parse("2016-09-01", formatter)
      val periodEndDate = LocalDate.parse("2016-12-01", formatter)

      models.output.tc.TaxYear(
        from = periodStartDate,
        until = periodEndDate,
        periods = for((wtcEligibility, ctcEligibility) <- wtcCTCEligibility) yield models.output.tc.TCPeriod(
          from = periodStartDate,
          until = periodEndDate,
          householdElements = models.output.tc.HouseholdElements(
            basic = false,
            hours30 = false,
            childcare = false,
            loneParent = false,
            secondParent = false,
            family = false,
            wtc = wtcEligibility,
            ctc = ctcEligibility
          ),
          claimants = List.empty,
          children = List.empty
        )
      )
    }

    val eligibilityTest = Table(
      ("WTC Eligibility, CTC Eligibility for period", "Is eligible"),
      (List((true, true)), true),
      (List((true, false)), false),
      (List((false, true)), false),
      (List((false, false)), false),
      (List((true, true), (true, true)), true),
      (List((true, true), (true, false)), true),
      (List((true, true), (false, true)), true),
      (List((true, true), (false, false)), true),
      (List((true, false), (true, true)), true),
      (List((true, false), (true, false)), false),
      (List((true, false), (false, true)), false),
      (List((true, false), (false, false)), false),
      (List((false, true), (true, true)), true),
      (List((false, true), (true, false)), false),
      (List((false, true), (false, true)), false),
      (List((false, true), (false, false)), false),
      (List((false, false), (true, true)), true),
      (List((false, false), (true, false)), false),
      (List((false, false), (false, true)), false),
      (List((false, false), (false, false)), false)
    )

    forAll(eligibilityTest) { case (wtcCtcEligibilityForPeriods, result) =>
        s"determine TC eligibility corectly if there are ${wtcCtcEligibilityForPeriods.length} periods with WTC and CTC: ${wtcCtcEligibilityForPeriods}" in {
          val ty = buildTaxYear(wtcCtcEligibilityForPeriods)
          TCEligibility.isEligibleForTC(List(ty)) shouldBe result
        }
    }

    "calculate and populate TC Eligibility model (one tax year, one period)" in {
      val periodStartDate = LocalDate.parse("2015-09-01", formatter)
      val periodEndDate = LocalDate.parse("2016-04-05", formatter)
      val dateOfBirth1 = LocalDate.parse("2011-12-01", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val claimant1 = Claimant(isPartner = false, hours = 16, disability = Disability(disabled = false, severelyDisabled = false), carersAllowance = false)
      val ty = models.input.tc.TaxYear(from = periodStartDate, until = periodEndDate, claimants = List(claimant1), children = List(child1))

      val outputClaimant = models.output.tc.OutputClaimant(qualifying = true, isPartner = false, claimantDisability = ClaimantDisability(disability = false, severeDisability = false))
      val outputChild = models.output.tc.OutputChild(childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = false, severeDisability = false, childcare = true))
      val outputHhElements = models.output.tc.HouseholdElements(basic = true, hours30 = false, childcare = true, loneParent = true, secondParent = false, family = true, wtc = true, ctc = true)
      val outputPeriod = models.output.tc.TCPeriod(from = periodStartDate, until = periodEndDate, householdElements = outputHhElements, claimants = List(outputClaimant), children = List(outputChild))
      val outputTaxYear = models.output.tc.TaxYear(from = periodStartDate, until = periodEndDate, periods = List(outputPeriod))
      val tcEligibilityModel = TCEligibilityModel(eligible = true, taxYears = List(outputTaxYear), wtc = true, ctc = true)

      val result  = await(TCEligibility.eligibility(TCEligibilityInput(taxYears = List(ty))))
      result shouldBe tcEligibilityModel
    }

    "calculate and populate TC Eligibility model (one tax year, one period) (no income)" in {
      val periodStartDate = LocalDate.parse("2015-09-01", formatter)
      val periodEndDate = LocalDate.parse("2016-04-05", formatter)
      val dateOfBirth1 = LocalDate.parse("2011-12-01", formatter)

      val child1 = Child(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = false, severelyDisabled = false), education = None)
      val claimant1 = Claimant(isPartner = false, hours = 16, disability = Disability(disabled = false, severelyDisabled = false), carersAllowance = false)
      val ty = models.input.tc.TaxYear(from = periodStartDate, until = periodEndDate, claimants = List(claimant1), children = List(child1))

      val outputClaimant = models.output.tc.OutputClaimant(qualifying = true, isPartner = false, claimantDisability = ClaimantDisability(disability = false, severeDisability = false))
      val outputChild = models.output.tc.OutputChild(childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, qualifying = true, childElements = ChildElements(child = true, youngAdult = false, disability = false, severeDisability = false, childcare = true))
      val outputHhElements = models.output.tc.HouseholdElements(basic = true, hours30 = false, childcare = true, loneParent = true, secondParent = false, family = true, wtc = true, ctc = true)
      val outputPeriod = models.output.tc.TCPeriod(from = periodStartDate, until = periodEndDate, householdElements = outputHhElements, claimants = List(outputClaimant), children = List(outputChild))
      val outputTaxYear = models.output.tc.TaxYear(from = periodStartDate, until = periodEndDate, periods = List(outputPeriod))
      val tcEligibilityModel = TCEligibilityModel(eligible = true, taxYears = List(outputTaxYear), wtc = true, ctc = true)

      val result  = await(TCEligibility.eligibility(TCEligibilityInput(taxYears = List(ty))))
      result shouldBe tcEligibilityModel
    }
  }
}
