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

import controllers.FakeCCEligibilityApplication
import eligibility.TFCEligibility.TFCEligibilityService
import models.input.tfc._
import models.output.OutputAPIModel.Eligibility
import models.output.tfc.{OutputChild, OutputClaimant, TFCEligibilityModel, TFCPeriod}
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.scalatest.mock.MockitoSugar
import spec.CCSpecConfig
import utils.Periods

import scala.concurrent.Future

class TFCEligibilitySpec extends CCSpecConfig with FakeCCEligibilityApplication with org.scalatest.PrivateMethodTester with MockitoSugar {

  "TFCEligibilityService" should {

    "return an instance of TFCEligibilityService" in {
      val service = TFCEligibility
      service.eligibility shouldBe a[TFCEligibilityService]
    }

    "return a Future[Eligibility] result" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val today = LocalDate.parse("2016-08-27", formatter)
      val untilDate = LocalDate.parse("2017-06-01", formatter)
      val tfc = TFC(from = today, numberOfPeriods = 3, claimants = List(), children = List())
      val result = TFCEligibility.eligibility.eligibility(Request(payload = Payload(tfc)))
      result.isInstanceOf[Future[Eligibility]] shouldBe true
    }

    "determine claimant's eligibility if claimant fails live or work" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2016-08-27", formatter)

      val current = LocalDate.parse("2017-08-01", formatter)
      val claimant = Claimant(liveOrWork = false, hoursPerWeek = 16.50, totalIncome = 99000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())

      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))

      val result = TFCEligibility.eligibility.determineClaimantsEligibility(List(claimant),current)
      result shouldBe List(
        OutputClaimant(
          qualifying = false,
          isPartner = false,
          failures = List()
        )
      )
    }

    "determine claimant's eligibility if qualifies all rules" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2016-08-27", formatter)

      val current = LocalDate.parse("2017-08-01", formatter)
      val claimant = Claimant(liveOrWork = true, hoursPerWeek = 3.50, totalIncome = 99000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())

      val result = TFCEligibility.eligibility.determineClaimantsEligibility(List(claimant), current)
      result shouldBe List(
        OutputClaimant(
          qualifying = true,
          isPartner = false,
          failures = List()
        )
      )
    }

    "determine claimant's and partner's eligibility if qualifies for all rules" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val current = LocalDate.parse("2017-08-01", formatter)

      val claimant = Claimant(liveOrWork = true, hoursPerWeek = 16.99, totalIncome = 99000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val partner = Claimant(liveOrWork = true, hoursPerWeek = 7.99, totalIncome = 99000, earnedIncome = 2709, isPartner = true, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())

      val result = TFCEligibility.eligibility.determineClaimantsEligibility(List(claimant, partner), current)
      result shouldBe List(
        OutputClaimant(
          qualifying = true,
          isPartner = false,
          failures = List()
        ),
        OutputClaimant(
          qualifying = true,
          isPartner = true,
          failures = List()
        )
      )
    }

    "determine claimant's eligibility if claimant fails maximum earnings rule" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2016-08-27", formatter)
      val claimant = Claimant(liveOrWork = true, hoursPerWeek = 9.50, totalIncome = 182000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val current = LocalDate.parse("2017-08-01", formatter)

      val result = TFCEligibility.eligibility.determineClaimantsEligibility(List(claimant), current)
      result shouldBe List(
        OutputClaimant(
          qualifying = false,
          isPartner = false,
          failures = List()
        )
      )
    }

    "determine claimant's eligibility if partner fails max earnings rule" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2003-08-27", formatter)
      val current = LocalDate.parse("2017-08-01", formatter)
      val claimant = Claimant(liveOrWork = true, hoursPerWeek = 5.50, totalIncome = 12000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val claimant2 = Claimant(liveOrWork = true, hoursPerWeek = 16.50, totalIncome = 151000, earnedIncome = 2709, isPartner = true, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())

      val result = TFCEligibility.eligibility.determineClaimantsEligibility(List(claimant, claimant2), current)
      result shouldBe List(
        OutputClaimant(
          qualifying = true,
          isPartner = false,
          failures = List()
        ),
        OutputClaimant(
          qualifying = false,
          isPartner = true,
          failures = List()
        )
      )
    }

    "determine periods(4 periods) based on from (claim date) and until date of TFC for eligible claimant" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2013-08-27", formatter)
      val from = LocalDate.parse("2016-10-15", formatter)
      val until = LocalDate.parse("2017-08-31", formatter)
      val claimant = Claimant(liveOrWork = true, hoursPerWeek = 16.50, totalIncome = 12000, earnedIncome = 2700, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val tfc = TFC(from = from, numberOfPeriods = 4, List(claimant), List(child))
      val result = TFCEligibility.eligibility.determineTFCPeriods(tfc)

      val outputClaimant = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val startPeriod1 = LocalDate.parse("2016-10-15", formatter)
      val untilPeriod1 = LocalDate.parse("2017-01-15", formatter)
      val startPeriod2 = LocalDate.parse("2017-01-15", formatter)
      val untilPeriod2 = LocalDate.parse("2017-04-15", formatter)
      val startPeriod3 = LocalDate.parse("2017-04-15", formatter)
      val untilPeriod3 = LocalDate.parse("2017-07-15", formatter)
      val startPeriod4 = LocalDate.parse("2017-07-15", formatter)
      val untilPeriod4 = LocalDate.parse("2017-10-15", formatter)


      val outputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())
      val outputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod2, until = untilPeriod2, failures = List())
      val outputChild3 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod3, until = untilPeriod3, failures = List())
      val outputChild4 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod4, until = untilPeriod4, failures = List())

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(outputChild1)
        ),
        TFCPeriod(
          from = startPeriod2,
          until = untilPeriod2,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(outputChild2)
        ),
        TFCPeriod(
          from = startPeriod3,
          until = untilPeriod3,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(outputChild3)
        ),
        TFCPeriod(
          from = startPeriod4,
          until = untilPeriod4,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(outputChild4)
        )
      )
    }

    "determine periods(2 periods) based on from (claim date) and until date of TFC for eligible claimant" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-08-27", formatter)
      val from = LocalDate.parse("2016-08-31", formatter)
      val claimant = Claimant(liveOrWork = true, hoursPerWeek = 16.50, totalIncome = 12000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val tfc = TFC(from = from, numberOfPeriods = 2, List(claimant), List(child))
      val result = TFCEligibility.eligibility.determineTFCPeriods(tfc)

      val outputClaimant = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val startPeriod1 = LocalDate.parse("2016-08-31", formatter)
      val untilPeriod1 = LocalDate.parse("2016-11-30", formatter)
      val startPeriod2 = LocalDate.parse("2016-11-30", formatter)
      val untilPeriod2 = LocalDate.parse("2017-02-28", formatter)

      val childFrom = LocalDate.parse("2016-08-31", formatter)
      val childUntil = LocalDate.parse("2016-09-04", formatter)
      val outputChild = OutputChild(id = 0, name = None, qualifying = true, from = childFrom, until = childUntil, failures = List())
      val outputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(outputChild)
        ),
        TFCPeriod(
          from = startPeriod2,
          until = untilPeriod2,
          periodEligibility = false,
          claimants = List(outputClaimant),
          children = List(outputChild2)
        )
      )
    }

    "determine periods(1 period) based on from (claim date) and until date of TFC for failed claimant eligibility(live or work)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2003-08-27", formatter)
      val from = LocalDate.parse("2016-08-27", formatter)
      val until = LocalDate.parse("2017-08-27", formatter)
      val claimant = Claimant(liveOrWork = false, hoursPerWeek = 16.50, totalIncome = 12000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false))
      val tfc = TFC(from = from, numberOfPeriods = 1, List(claimant), List(child))
      val result = TFCEligibility.eligibility.determineTFCPeriods(tfc)

      val outputClaimant = OutputClaimant(qualifying = false, isPartner = false, failures = List())
      val startPeriod1 = LocalDate.parse("2016-08-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-11-27", formatter)


      val outputChild = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          claimants = List(outputClaimant),
          children = List(outputChild)
        )
      )
    }

    "determine periods(1 period) based on from (claim date) and until date of TFC for failed partner eligibility(hours)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2003-08-27", formatter)
      val from = LocalDate.parse("2016-08-27", formatter)
      val until = LocalDate.parse("2017-08-27", formatter)
      val claimant = Claimant(liveOrWork = true, hoursPerWeek = 16.50, totalIncome = 12000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val partner = Claimant(liveOrWork = true, hoursPerWeek = 5.50, totalIncome = 12000, earnedIncome = 2709, isPartner = true, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val tfc = TFC(from = from, numberOfPeriods = 1, List(claimant, partner), List(child))
      val result = TFCEligibility.eligibility.determineTFCPeriods(tfc)

      val outputClaimant = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val outputPartner = OutputClaimant(qualifying = true, isPartner = true, failures = List())
      val startPeriod1 = LocalDate.parse("2016-08-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-11-27", formatter)

      val childFrom = LocalDate.parse("2016-08-27", formatter)
      val childUntil = LocalDate.parse("2016-11-27", formatter)
      val outputChild = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          claimants = List(outputClaimant, outputPartner),
          children = List(outputChild)
        )
      )
    }

    "determine child's start date in a TFC period where child is born on the period start date (in current period)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2016-08-01", formatter)
      val from = LocalDate.parse("2016-08-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = from, until = until, claimants = List(), children = List())
      val result = TFCEligibility.eligibility.determineChildStartDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until)
      result shouldBe dateOfBirth
    }

    "determine child's start date in a TFC period where child is yet to be born (in current period)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2016-08-27", formatter)
      val from = LocalDate.parse("2016-08-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = from, until = until, claimants = List(), children = List())
      val result = TFCEligibility.eligibility.determineChildStartDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until)
      result shouldBe dateOfBirth
    }

    "determine child's start date in a TFC period where child is yet to be born (after current period)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2016-12-27", formatter)
      val from = LocalDate.parse("2016-08-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = from, until = until, claimants = List(), children = List())
      val result = TFCEligibility.eligibility.determineChildStartDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until)
      result shouldBe null
    }

    "determine child's start date in a TFC period where child is eligible (in current period)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2013-12-27", formatter)
      val from = LocalDate.parse("2016-08-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = from, until = until, claimants = List(), children = List())
      val result = TFCEligibility.eligibility.determineChildStartDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until)
      result shouldBe from
    }

    "determine child's start date in a TFC period where child 11th birthday is in current period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-12-27", formatter)
      val from = LocalDate.parse("2016-08-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = from, until = until, claimants = List(), children = List())
      val result = TFCEligibility.eligibility.determineChildStartDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until)
      result shouldBe from
    }

    "determine child's start date in a TFC period where child 11th birthday sept date is before current period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2003-08-27", formatter)
      val from = LocalDate.parse("2016-08-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = from, until = until, claimants = List(), children = List())
      val result = TFCEligibility.eligibility.determineChildStartDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until)
      result shouldBe null
    }


    "determine child's start date in a TFC period where child 11th birthday sept date is after current period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-08-27", formatter)
      val from = LocalDate.parse("2016-04-01", formatter)
      val until = LocalDate.parse("2016-07-01", formatter)
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = from, until = until, claimants = List(), children = List())
      val result = TFCEligibility.eligibility.determineChildStartDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until)
      result shouldBe from
    }

    "determine child's start date in a TFC period where child 11th birthday sept date is within current period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-04-27", formatter)
      val from = LocalDate.parse("2016-08-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = from, until = until, claimants = List(), children = List())
      val result = TFCEligibility.eligibility.determineChildStartDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until)
      result shouldBe from
    }


    "determine child's start date in a TFC period where child(disabled) 16th birthday sept date is within current period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("1999-09-27", formatter)
      val from = LocalDate.parse("2016-09-06", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = from, until = until, claimants = List(), children = List())
      val childSeptDate = child.endWeek1stOfSeptemberDate(from)
      LocalDate.fromDateFields(childSeptDate) shouldBe LocalDate.parse("2016-09-04", formatter)
      val result = TFCEligibility.eligibility.determineChildStartDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until)
      result shouldBe null
    }

    "determine child's end date in a TFC period where the birthday after period end date" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2016-11-27", formatter)
      val from = LocalDate.parse("2016-09-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = from, until = until, claimants = List(), children = List())
      val result = TFCEligibility.eligibility.determineChildEndDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until)
      result shouldBe null
    }

    "determine child's end date in a TFC period where the birthday is on period until date" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2016-11-01", formatter)
      val from = LocalDate.parse("2016-09-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = from, until = until, claimants = List(), children = List())
      val result = TFCEligibility.eligibility.determineChildEndDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until)
      result shouldBe null
    }

    "determine child's end date in a TFC period where the birthday is within the TFC period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2016-10-01", formatter)
      val from = LocalDate.parse("2016-09-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = from, until = until, claimants = List(), children = List())
      val result = TFCEligibility.eligibility.determineChildEndDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until)
      result shouldBe until
    }

    "determine child's end date in a TFC period where the birthday is before TFC period start date" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2016-06-01", formatter)
      val from = LocalDate.parse("2016-09-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = from, until = until, claimants = List(), children = List())
      val result = TFCEligibility.eligibility.determineChildEndDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until)
      result shouldBe until
    }

    "determine child's end date in a TFC period where the 11th birthday sept date is after TFC period until date" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-06-01", formatter)
      val from = LocalDate.parse("2016-04-01", formatter)
      val until = LocalDate.parse("2016-07-01", formatter)
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = from, until = until, claimants = List(), children = List())
      val result = TFCEligibility.eligibility.determineChildEndDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until)
      result shouldBe until
    }

    "determine child's end date in a TFC period where the 11th birthday sept date is within TFC period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-06-01", formatter)
      val from = LocalDate.parse("2016-08-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val childSeptDate = child.endWeek1stOfSeptemberDate(from)
      val tfcPeriod = TFCPeriod(from = from, until = until, claimants = List(), children = List())
      val result = TFCEligibility.eligibility.determineChildEndDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until)
      result shouldBe LocalDate.fromDateFields(childSeptDate)
    }

    "determine child's end date in a TFC period where the 11th birthday sept date is before TFC period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-06-01", formatter)
      val from = LocalDate.parse("2016-10-01", formatter)
      val until = LocalDate.parse("2017-01-01", formatter)
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = from, until = until, claimants = List(), children = List())
      val result = TFCEligibility.eligibility.determineChildEndDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until)
      result shouldBe null
    }

    "determine periods(2 period) based on from (claim date) and until date of TFC for one eligible child" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2003-08-27", formatter)
      val from = LocalDate.parse("2016-08-27", formatter)
      val claimant = Claimant(liveOrWork = false, hoursPerWeek = 16.50, totalIncome = 12000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val tfc = TFC(from = from, 1, List(claimant), List(child))
      val result = TFCEligibility.eligibility.determineTFCPeriods(tfc)

      val outputClaimant = OutputClaimant(qualifying = false, isPartner = false, failures = List())
      val startPeriod1 = LocalDate.parse("2016-08-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-11-27", formatter)
      val outputChild = OutputChild(id = 0, name = None, qualifying = false, null, null, failures = List())

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          claimants = List(outputClaimant),
          children = List(outputChild)
        )
      )
    }

    "populate child output model for eligible child" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2003-08-27", formatter)
      val from = LocalDate.parse("2016-08-27", formatter)
      val until = LocalDate.parse("2016-11-27", formatter)
      val child = Child(id = 2, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val result = TFCEligibility.eligibility.determineChildrenEligibility(List(child), from, until)

      result shouldBe List(
        OutputChild(
          id = 2,
          name = None,
          qualifying = false,
          from = null,
          until = null,
          failures = List()
        )
      )
    }

    "determine child eligibility for 1 period - for 2 children where 1 child is disabled" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirthChild1 = LocalDate.parse("2003-07-27", formatter)
      val dateOfBirthChild2 = LocalDate.parse("2005-08-21", formatter)
      val from = LocalDate.parse("2016-09-27", formatter)
      val claimant = Claimant(liveOrWork = true, hoursPerWeek = 16.50, totalIncome = 12000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val child1 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild1, disability = Disability(disabled = true, severelyDisabled = false))
      val child2 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild2, disability = Disability(disabled = false, severelyDisabled = false))
      val tfc = TFC(from = from, 1, List(claimant), List(child1, child2))
      val result = TFCEligibility.eligibility.determineTFCPeriods(tfc)

      val outputClaimant = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())
      val outputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(outputChild1, outputChild2)
        )
      )
    }

    "determine child eligibility-2 period - 3 children - 1 child is disabled & eligible, 1 - not eligible and 1 child - eligible for 1 period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirthChild1 = LocalDate.parse("2002-07-27", formatter)
      val dateOfBirthChild2 = LocalDate.parse("2016-12-21", formatter)
      val dateOfBirthChild3 = LocalDate.parse("2005-08-21", formatter)
      val from = LocalDate.parse("2016-08-01", formatter)
      val claimant = Claimant(liveOrWork = true, hoursPerWeek = 16.50, totalIncome = 12000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val child1 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild1, disability = Disability(disabled = false, severelyDisabled = false))
      val child2 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild2, disability = Disability(disabled = false, severelyDisabled = false))
      val child3 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild3, disability = Disability(disabled = true, severelyDisabled = false))
      val tfc = TFC(from = from, 2, List(claimant), List(child1, child2, child3))
      val result = TFCEligibility.eligibility.determineTFCPeriods(tfc)

      val outputClaimant = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val startPeriod1 = LocalDate.parse("2016-08-01", formatter)
      val untilPeriod1 = LocalDate.parse("2016-11-01", formatter)
      val startPeriod2 = LocalDate.parse("2016-11-01", formatter)
      val untilPeriod2 = LocalDate.parse("2017-02-01", formatter)

      val Period1OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period1OutputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period1OutputChild3 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())

      val Period2OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period2OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = dateOfBirthChild2, until = untilPeriod2, failures = List())
      val Period2OutputChild3 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod2, until = untilPeriod2, failures = List())

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period1OutputChild1, Period1OutputChild2, Period1OutputChild3)
        ),
        TFCPeriod(
          from = startPeriod2,
          until = untilPeriod2,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period2OutputChild1, Period2OutputChild2, Period2OutputChild3)
        )
      )
    }

    "determine child eligibility-8 periods - 3 children " in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirthChild1 = LocalDate.parse("2005-07-01", formatter)
      val dateOfBirthChild2 = LocalDate.parse("2001-05-10", formatter)
      val dateOfBirthChild3 = LocalDate.parse("2018-02-15", formatter)
      val from = LocalDate.parse("2016-07-31", formatter)
      val claimant = Claimant(liveOrWork = true, hoursPerWeek = 16.50, totalIncome = 12000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val child1 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild1, disability = Disability(disabled = false, severelyDisabled = false))
      val child2 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild2, disability = Disability(disabled = true, severelyDisabled = false))
      val child3 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild3, disability = Disability(disabled = false, severelyDisabled = false))
      val tfc = TFC(from = from, 8, List(claimant), List(child1, child2, child3))
      val result = TFCEligibility.eligibility.determineTFCPeriods(tfc)

      val outputClaimant = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val startPeriod1 = LocalDate.parse("2016-07-31", formatter)
      val untilPeriod1 = LocalDate.parse("2016-10-31", formatter)
      val startPeriod2 = LocalDate.parse("2016-10-31", formatter)
      val untilPeriod2 = LocalDate.parse("2017-01-31", formatter)
      val startPeriod3 = LocalDate.parse("2017-01-31", formatter)
      val untilPeriod3 = LocalDate.parse("2017-04-30", formatter)
      val startPeriod4 = LocalDate.parse("2017-04-30", formatter)
      val untilPeriod4 = LocalDate.parse("2017-07-30", formatter)
      val startPeriod5 = LocalDate.parse("2017-07-30", formatter)
      val untilPeriod5 = LocalDate.parse("2017-10-30", formatter)
      val startPeriod6 = LocalDate.parse("2017-10-30", formatter)
      val untilPeriod6 = LocalDate.parse("2018-01-30", formatter)
      val startPeriod7 = LocalDate.parse("2018-01-30", formatter)
      val untilPeriod7 = LocalDate.parse("2018-04-30", formatter)
      val startPeriod8 = LocalDate.parse("2018-04-30", formatter)
      val untilPeriod8 = LocalDate.parse("2018-07-30", formatter)

      val child1EndDate = LocalDate.parse("2016-09-04", formatter)
      val child2EndDate = LocalDate.parse("2017-09-03", formatter)

      val Period1OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = child1EndDate, failures = List())
      val Period1OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())
      val Period1OutputChild3 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      val Period2OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period2OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod2, until = untilPeriod2, failures = List())
      val Period2OutputChild3 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      val Period3OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period3OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod3, until = untilPeriod3, failures = List())
      val Period3OutputChild3 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      val Period4OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period4OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod4, until = untilPeriod4, failures = List())
      val Period4OutputChild3 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      val Period5OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period5OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod5, until = child2EndDate, failures = List())
      val Period5OutputChild3 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      val Period6OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period6OutputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period6OutputChild3 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      val Period7OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period7OutputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period7OutputChild3 = OutputChild(id = 0, name = None, qualifying = true, from = dateOfBirthChild3, until = untilPeriod7, failures = List())

      val Period8OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period8OutputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period8OutputChild3 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod8, until = untilPeriod8, failures = List())

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period1OutputChild1, Period1OutputChild2, Period1OutputChild3)
        ),
        TFCPeriod(
          from = startPeriod2,
          until = untilPeriod2,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period2OutputChild1, Period2OutputChild2, Period2OutputChild3)
        ),
        TFCPeriod(
          from = startPeriod3,
          until = untilPeriod3,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period3OutputChild1, Period3OutputChild2, Period3OutputChild3)
        ),
        TFCPeriod(
          from = startPeriod4,
          until = untilPeriod4,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period4OutputChild1, Period4OutputChild2, Period4OutputChild3)
        ),
        TFCPeriod(
          from = startPeriod5,
          until = untilPeriod5,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period5OutputChild1, Period5OutputChild2, Period5OutputChild3)
        ),
        TFCPeriod(
          from = startPeriod6,
          until = untilPeriod6,
          periodEligibility = false,
          claimants = List(outputClaimant),
          children = List(Period6OutputChild1, Period6OutputChild2, Period6OutputChild3)
        ),
        TFCPeriod(
          from = startPeriod7,
          until = untilPeriod7,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period7OutputChild1, Period7OutputChild2, Period7OutputChild3)
        ),
        TFCPeriod(
          from = startPeriod8,
          until = untilPeriod8,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period8OutputChild1, Period8OutputChild2, Period8OutputChild3)
        )
      )
    }

    "determine child eligibility-4 periods - 2 children " in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirthChild1 = LocalDate.parse("2005-07-01", formatter)
      val dateOfBirthChild2 = LocalDate.parse("2001-05-10", formatter)

      val from = LocalDate.parse("2016-05-30", formatter)
      val claimant = Claimant(liveOrWork = false, hoursPerWeek = 16.50, totalIncome = 12000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val child1 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild1, disability = Disability(disabled = false, severelyDisabled = false))
      val child2 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild2, disability = Disability(disabled = true, severelyDisabled = false))

      val tfc = TFC(from = from, 4, List(claimant), List(child1, child2))
      val result = TFCEligibility.eligibility.determineTFCPeriods(tfc)

      val outputClaimant = OutputClaimant(qualifying = false, isPartner = false, failures = List())
      val startPeriod1 = LocalDate.parse("2016-05-30", formatter)
      val untilPeriod1 = LocalDate.parse("2016-08-30", formatter)
      val startPeriod2 = LocalDate.parse("2016-08-30", formatter)
      val untilPeriod2 = LocalDate.parse("2016-11-30", formatter)
      val startPeriod3 = LocalDate.parse("2016-11-30", formatter)
      val untilPeriod3 = LocalDate.parse("2017-02-28", formatter)
      val startPeriod4 = LocalDate.parse("2017-02-28", formatter)
      val untilPeriod4 = LocalDate.parse("2017-05-28", formatter)

      val child1EndDate = LocalDate.parse("2016-09-04", formatter)

      val Period1OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())
      val Period1OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())

      val Period2OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod2, until = child1EndDate, failures = List())
      val Period2OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod2, until = untilPeriod2, failures = List())

      val Period3OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period3OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod3, until = untilPeriod3, failures = List())

      val Period4OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period4OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod4, until = untilPeriod4, failures = List())


      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          claimants = List(outputClaimant),
          children = List(Period1OutputChild1, Period1OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod2,
          until = untilPeriod2,
          claimants = List(outputClaimant),
          children = List(Period2OutputChild1, Period2OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod3,
          until = untilPeriod3,
          claimants = List(outputClaimant),
          children = List(Period3OutputChild1, Period3OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod4,
          until = untilPeriod4,
          claimants = List(outputClaimant),
          children = List(Period4OutputChild1, Period4OutputChild2)
        )
      )
    }

    "determine child eligibility-6 periods - 3 children " in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirthChild1 = LocalDate.parse("2002-10-11", formatter)
      val dateOfBirthChild2 = LocalDate.parse("2003-09-17", formatter)
      val dateOfBirthChild3 = LocalDate.parse("2011-03-18", formatter)
      val from = LocalDate.parse("2016-10-31", formatter)
      val claimant = Claimant(liveOrWork = true, hoursPerWeek = 16.50, totalIncome = 12000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val child1 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild1, disability = Disability(disabled = false, severelyDisabled = false))
      val child2 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild2, disability = Disability(disabled = true, severelyDisabled = false))
      val child3 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild3, disability = Disability(disabled = false, severelyDisabled = false))
      val tfc = TFC(from = from, 6, List(claimant), List(child1, child2, child3))
      val result = TFCEligibility.eligibility.determineTFCPeriods(tfc)

      val outputClaimant = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val startPeriod1 = LocalDate.parse("2016-10-31", formatter)
      val untilPeriod1 = LocalDate.parse("2017-01-31", formatter)
      val startPeriod2 = LocalDate.parse("2017-01-31", formatter)
      val untilPeriod2 = LocalDate.parse("2017-04-30", formatter)
      val startPeriod3 = LocalDate.parse("2017-04-30", formatter)
      val untilPeriod3 = LocalDate.parse("2017-07-30", formatter)
      val startPeriod4 = LocalDate.parse("2017-07-30", formatter)
      val untilPeriod4 = LocalDate.parse("2017-10-30", formatter)
      val startPeriod5 = LocalDate.parse("2017-10-30", formatter)
      val untilPeriod5 = LocalDate.parse("2018-01-30", formatter)
      val startPeriod6 = LocalDate.parse("2018-01-30", formatter)
      val untilPeriod6 = LocalDate.parse("2018-04-30", formatter)

      val Period1OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period1OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())
      val Period1OutputChild3 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())

      val Period2OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period2OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod2, until = untilPeriod2, failures = List())
      val Period2OutputChild3 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod2, until = untilPeriod2, failures = List())

      val Period3OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period3OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod3, until = untilPeriod3, failures = List())
      val Period3OutputChild3 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod3, until = untilPeriod3, failures = List())

      val Period4OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period4OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod4, until = untilPeriod4, failures = List())
      val Period4OutputChild3 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod4, until = untilPeriod4, failures = List())

      val Period5OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period5OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod5, until = untilPeriod5, failures = List())
      val Period5OutputChild3 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod5, until = untilPeriod5, failures = List())

      val Period6OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period6OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod6, until = untilPeriod6, failures = List())
      val Period6OutputChild3 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod6, until = untilPeriod6, failures = List())

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period1OutputChild1, Period1OutputChild2, Period1OutputChild3)
        ),
        TFCPeriod(
          from = startPeriod2,
          until = untilPeriod2,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period2OutputChild1, Period2OutputChild2, Period2OutputChild3)
        ),
        TFCPeriod(
          from = startPeriod3,
          until = untilPeriod3,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period3OutputChild1, Period3OutputChild2, Period3OutputChild3)
        ),
        TFCPeriod(
          from = startPeriod4,
          until = untilPeriod4,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period4OutputChild1, Period4OutputChild2, Period4OutputChild3)
        ),
        TFCPeriod(
          from = startPeriod5,
          until = untilPeriod5,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period5OutputChild1, Period5OutputChild2, Period5OutputChild3)
        ),
        TFCPeriod(
          from = startPeriod6,
          until = untilPeriod6,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period6OutputChild1, Period6OutputChild2, Period6OutputChild3)
        )
      )
    }

    "determine child eligibility-3 periods - 2 children " in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirthChild1 = LocalDate.parse("2006-07-01", formatter)
      val dateOfBirthChild2 = LocalDate.parse("2003-05-10", formatter)

      val from = LocalDate.parse("2017-06-30", formatter)
      val claimant = Claimant(liveOrWork = false, hoursPerWeek = 16.50, totalIncome = 12000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val claimant1 = Claimant(liveOrWork = true, hoursPerWeek = 16.50, totalIncome = 12000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())

      val child1 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild1, disability = Disability(disabled = false, severelyDisabled = false))
      val child2 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild2, disability = Disability(disabled = true, severelyDisabled = false))

      val tfc = TFC(from = from, 3, List(claimant, claimant1), List(child1, child2))
      val result = TFCEligibility.eligibility.determineTFCPeriods(tfc)

      val outputClaimant = OutputClaimant(qualifying = false, isPartner = false, failures = List())
      val outputClaimant1 = OutputClaimant(qualifying = true, isPartner = false, failures = List())

      val startPeriod1 = LocalDate.parse("2017-06-30", formatter)
      val untilPeriod1 = LocalDate.parse("2017-09-30", formatter)
      val startPeriod2 = LocalDate.parse("2017-09-30", formatter)
      val untilPeriod2 = LocalDate.parse("2017-12-30", formatter)
      val startPeriod3 = LocalDate.parse("2017-12-30", formatter)
      val untilPeriod3 = LocalDate.parse("2018-03-30", formatter)

      val child1EndDate = LocalDate.parse("2017-09-03", formatter)

      val Period1OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = child1EndDate, failures = List())
      val Period1OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())

      val Period2OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period2OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod2, until = untilPeriod2, failures = List())

      val Period3OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period3OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod3, until = untilPeriod3, failures = List())

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          claimants = List(outputClaimant, outputClaimant1),
          children = List(Period1OutputChild1, Period1OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod2,
          until = untilPeriod2,
          claimants = List(outputClaimant, outputClaimant1),
          children = List(Period2OutputChild1, Period2OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod3,
          until = untilPeriod3,
          claimants = List(outputClaimant, outputClaimant1),
          children = List(Period3OutputChild1, Period3OutputChild2)
        )
      )
    }

    "determine child eligibility-5 periods - 3 children " in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirthChild1 = LocalDate.parse("2005-10-11", formatter)
      val dateOfBirthChild2 = LocalDate.parse("2006-10-17", formatter)
      val dateOfBirthChild3 = LocalDate.parse("2018-03-18", formatter)
      val from = LocalDate.parse("2017-08-31", formatter)

      val claimant = Claimant(liveOrWork = true, hoursPerWeek = 16.50, totalIncome = 12000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val claimant1 = Claimant(liveOrWork = true, hoursPerWeek = 16.50, totalIncome = 12000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())

      val child1 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild1, disability = Disability(disabled = true, severelyDisabled = false))
      val child2 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild2, disability = Disability(disabled = false, severelyDisabled = false))
      val child3 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild3, disability = Disability(disabled = false, severelyDisabled = false))
      val tfc = TFC(from = from, 5, List(claimant, claimant1), List(child1, child2, child3))
      val result = TFCEligibility.eligibility.determineTFCPeriods(tfc)

      val outputClaimant = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val outputClaimant1 = OutputClaimant(qualifying = true, isPartner = false, failures = List())

      val startPeriod1 = LocalDate.parse("2017-08-31", formatter)
      val untilPeriod1 = LocalDate.parse("2017-11-30", formatter)
      val startPeriod2 = LocalDate.parse("2017-11-30", formatter)
      val untilPeriod2 = LocalDate.parse("2018-02-28", formatter)
      val startPeriod3 = LocalDate.parse("2018-02-28", formatter)
      val untilPeriod3 = LocalDate.parse("2018-05-28", formatter)
      val startPeriod4 = LocalDate.parse("2018-05-28", formatter)
      val untilPeriod4 = LocalDate.parse("2018-08-28", formatter)
      val startPeriod5 = LocalDate.parse("2018-08-28", formatter)
      val untilPeriod5 = LocalDate.parse("2018-11-28", formatter)
      val child2EndDate = LocalDate.parse("2018-09-02", formatter)

      val Period1OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())
      val Period1OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())
      val Period1OutputChild3 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      val Period2OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod2, until = untilPeriod2, failures = List())
      val Period2OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod2, until = untilPeriod2, failures = List())
      val Period2OutputChild3 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      val Period3OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod3, until = untilPeriod3, failures = List())
      val Period3OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod3, until = untilPeriod3, failures = List())
      val Period3OutputChild3 = OutputChild(id = 0, name = None, qualifying = true, from = dateOfBirthChild3, until = untilPeriod3, failures = List())

      val Period4OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod4, until = untilPeriod4, failures = List())
      val Period4OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod4, until = untilPeriod4, failures = List())
      val Period4OutputChild3 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod4, until = untilPeriod4, failures = List())

      val Period5OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod5, until = untilPeriod5, failures = List())
      val Period5OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod5, until = child2EndDate, failures = List())
      val Period5OutputChild3 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod5, until = untilPeriod5, failures = List())

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          periodEligibility = true,
          claimants = List(outputClaimant, outputClaimant1),
          children = List(Period1OutputChild1, Period1OutputChild2, Period1OutputChild3)
        ),
        TFCPeriod(
          from = startPeriod2,
          until = untilPeriod2,
          periodEligibility = true,
          claimants = List(outputClaimant, outputClaimant1),
          children = List(Period2OutputChild1, Period2OutputChild2, Period2OutputChild3)
        ),
        TFCPeriod(
          from = startPeriod3,
          until = untilPeriod3,
          periodEligibility = true,
          claimants = List(outputClaimant, outputClaimant1),
          children = List(Period3OutputChild1, Period3OutputChild2, Period3OutputChild3)
        ),
        TFCPeriod(
          from = startPeriod4,
          until = untilPeriod4,
          periodEligibility = true,
          claimants = List(outputClaimant, outputClaimant1),
          children = List(Period4OutputChild1, Period4OutputChild2, Period4OutputChild3)
        ),
        TFCPeriod(
          from = startPeriod5,
          until = untilPeriod5,
          periodEligibility = true,
          claimants = List(outputClaimant, outputClaimant1),
          children = List(Period5OutputChild1, Period5OutputChild2, Period5OutputChild3)
        )
      )
    }

    "determine child eligibility-7 periods - 2 children " in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirthChild1 = LocalDate.parse("2007-06-15", formatter)
      val dateOfBirthChild2 = LocalDate.parse("2009-01-01", formatter)
      val from = LocalDate.parse("2016-01-15", formatter)
      val claimant = Claimant(liveOrWork = false, hoursPerWeek = 16.50, totalIncome = 12000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val child1 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild1, disability = Disability(disabled = true, severelyDisabled = false))
      val child2 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild2, disability = Disability(disabled = false, severelyDisabled = false))
      val tfc = TFC(from = from, 7, List(claimant), List(child1, child2))
      val result = TFCEligibility.eligibility.determineTFCPeriods(tfc)

      val outputClaimant = OutputClaimant(qualifying = false, isPartner = false, failures = List())
      val startPeriod1 = LocalDate.parse("2016-01-15", formatter)
      val untilPeriod1 = LocalDate.parse("2016-04-15", formatter)
      val startPeriod2 = LocalDate.parse("2016-04-15", formatter)
      val untilPeriod2 = LocalDate.parse("2016-07-15", formatter)
      val startPeriod3 = LocalDate.parse("2016-07-15", formatter)
      val untilPeriod3 = LocalDate.parse("2016-10-15", formatter)
      val startPeriod4 = LocalDate.parse("2016-10-15", formatter)
      val untilPeriod4 = LocalDate.parse("2017-01-15", formatter)
      val startPeriod5 = LocalDate.parse("2017-01-15", formatter)
      val untilPeriod5 = LocalDate.parse("2017-04-15", formatter)
      val startPeriod6 = LocalDate.parse("2017-04-15", formatter)
      val untilPeriod6 = LocalDate.parse("2017-07-15", formatter)
      val startPeriod7 = LocalDate.parse("2017-07-15", formatter)
      val untilPeriod7 = LocalDate.parse("2017-10-15", formatter)

      val Period1OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())
      val Period1OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())

      val Period2OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod2, until = untilPeriod2, failures = List())
      val Period2OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod2, until = untilPeriod2, failures = List())

      val Period3OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod3, until = untilPeriod3, failures = List())
      val Period3OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod3, until = untilPeriod3, failures = List())

      val Period4OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod4, until = untilPeriod4, failures = List())
      val Period4OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod4, until = untilPeriod4, failures = List())

      val Period5OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod5, until = untilPeriod5, failures = List())
      val Period5OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod5, until = untilPeriod5, failures = List())

      val Period6OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod6, until = untilPeriod6, failures = List())
      val Period6OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod6, until = untilPeriod6, failures = List())

      val Period7OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod7, until = untilPeriod7, failures = List())
      val Period7OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod7, until = untilPeriod7, failures = List())

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          claimants = List(outputClaimant),
          children = List(Period1OutputChild1, Period1OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod2,
          until = untilPeriod2,
          claimants = List(outputClaimant),
          children = List(Period2OutputChild1, Period2OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod3,
          until = untilPeriod3,
          claimants = List(outputClaimant),
          children = List(Period3OutputChild1, Period3OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod4,
          until = untilPeriod4,
          claimants = List(outputClaimant),
          children = List(Period4OutputChild1, Period4OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod5,
          until = untilPeriod5,
          claimants = List(outputClaimant),
          children = List(Period5OutputChild1, Period5OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod6,
          until = untilPeriod6,
          claimants = List(outputClaimant),
          children = List(Period6OutputChild1, Period6OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod7,
          until = untilPeriod7,
          claimants = List(outputClaimant),
          children = List(Period7OutputChild1, Period7OutputChild2)
        )
      )
    }

    "determine child eligibility-8 periods - 2 children (both disabled) " in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirthChild1 = LocalDate.parse("2005-07-01", formatter)
      val dateOfBirthChild2 = LocalDate.parse("2000-12-10", formatter)
      val from = LocalDate.parse("2016-07-31", formatter)
      val claimant = Claimant(liveOrWork = false, hoursPerWeek = 16.50, totalIncome = 12000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val child1 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild1, disability = Disability(disabled = true, severelyDisabled = false))
      val child2 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild2, disability = Disability(disabled = true, severelyDisabled = false))
      val tfc = TFC(from = from, 8, List(claimant), List(child1, child2))
      val result = TFCEligibility.eligibility.determineTFCPeriods(tfc)

      val outputClaimant = OutputClaimant(qualifying = false, isPartner = false, failures = List())
      val startPeriod1 = LocalDate.parse("2016-07-31", formatter)
      val untilPeriod1 = LocalDate.parse("2016-10-31", formatter)
      val startPeriod2 = LocalDate.parse("2016-10-31", formatter)
      val untilPeriod2 = LocalDate.parse("2017-01-31", formatter)
      val startPeriod3 = LocalDate.parse("2017-01-31", formatter)
      val untilPeriod3 = LocalDate.parse("2017-04-30", formatter)
      val startPeriod4 = LocalDate.parse("2017-04-30", formatter)
      val untilPeriod4 = LocalDate.parse("2017-07-30", formatter)
      val startPeriod5 = LocalDate.parse("2017-07-30", formatter)
      val untilPeriod5 = LocalDate.parse("2017-10-30", formatter)
      val startPeriod6 = LocalDate.parse("2017-10-30", formatter)
      val untilPeriod6 = LocalDate.parse("2018-01-30", formatter)
      val startPeriod7 = LocalDate.parse("2018-01-30", formatter)
      val untilPeriod7 = LocalDate.parse("2018-04-30", formatter)
      val startPeriod8 = LocalDate.parse("2018-04-30", formatter)
      val untilPeriod8 = LocalDate.parse("2018-07-30", formatter)

      val child2EndDate = LocalDate.parse("2017-09-03", formatter)

      val Period1OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())
      val Period1OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())

      val Period2OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod2, until = untilPeriod2, failures = List())
      val Period2OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod2, until = untilPeriod2, failures = List())

      val Period3OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod3, until = untilPeriod3, failures = List())
      val Period3OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod3, until = untilPeriod3, failures = List())

      val Period4OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod4, until = untilPeriod4, failures = List())
      val Period4OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod4, until = untilPeriod4, failures = List())

      val Period5OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod5, until = untilPeriod5, failures = List())
      val Period5OutputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod5, until = child2EndDate, failures = List())

      val Period6OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod6, until = untilPeriod6, failures = List())
      val Period6OutputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      val Period7OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod7, until = untilPeriod7, failures = List())
      val Period7OutputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      val Period8OutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod8, until = untilPeriod8, failures = List())
      val Period8OutputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          claimants = List(outputClaimant),
          children = List(Period1OutputChild1, Period1OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod2,
          until = untilPeriod2,
          claimants = List(outputClaimant),
          children = List(Period2OutputChild1, Period2OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod3,
          until = untilPeriod3,
          claimants = List(outputClaimant),
          children = List(Period3OutputChild1, Period3OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod4,
          until = untilPeriod4,
          claimants = List(outputClaimant),
          children = List(Period4OutputChild1, Period4OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod5,
          until = untilPeriod5,
          claimants = List(outputClaimant),
          children = List(Period5OutputChild1, Period5OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod6,
          until = untilPeriod6,
          claimants = List(outputClaimant),
          children = List(Period6OutputChild1, Period6OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod7,
          until = untilPeriod7,
          claimants = List(outputClaimant),
          children = List(Period7OutputChild1, Period7OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod8,
          until = untilPeriod8,
          claimants = List(outputClaimant),
          children = List(Period8OutputChild1, Period8OutputChild2)
        )
      )
    }

    "determine child eligibility-4 periods - 2 children - both do not qualify" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirthChild1 = LocalDate.parse("2002-07-01", formatter)
      val dateOfBirthChild2 = LocalDate.parse("2017-06-01", formatter)

      val from = LocalDate.parse("2016-05-23", formatter)
      val claimant = Claimant(liveOrWork = false, hoursPerWeek = 16.50, totalIncome = 12000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val child1 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild1, disability = Disability(disabled = false, severelyDisabled = false))
      val child2 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild2, disability = Disability(disabled = false, severelyDisabled = false))

      val tfc = TFC(from = from, 4, List(claimant), List(child1, child2))
      val result = TFCEligibility.eligibility.determineTFCPeriods(tfc)

      val outputClaimant = OutputClaimant(qualifying = false, isPartner = false, failures = List())
      val startPeriod1 = LocalDate.parse("2016-05-23", formatter)
      val untilPeriod1 = LocalDate.parse("2016-08-23", formatter)
      val startPeriod2 = LocalDate.parse("2016-08-23", formatter)
      val untilPeriod2 = LocalDate.parse("2016-11-23", formatter)
      val startPeriod3 = LocalDate.parse("2016-11-23", formatter)
      val untilPeriod3 = LocalDate.parse("2017-02-23", formatter)
      val startPeriod4 = LocalDate.parse("2017-02-23", formatter)
      val untilPeriod4 = LocalDate.parse("2017-05-23", formatter)

      val Period1OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period1OutputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      val Period2OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period2OutputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      val Period3OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period3OutputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      val Period4OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period4OutputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          claimants = List(outputClaimant),
          children = List(Period1OutputChild1, Period1OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod2,
          until = untilPeriod2,
          claimants = List(outputClaimant),
          children = List(Period2OutputChild1, Period2OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod3,
          until = untilPeriod3,
          claimants = List(outputClaimant),
          children = List(Period3OutputChild1, Period3OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod4,
          until = untilPeriod4,
          claimants = List(outputClaimant),
          children = List(Period4OutputChild1, Period4OutputChild2)
        )
      )
    }

    "determine child eligibility-4 periods - 2 children - both disabled - both do not qualify" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirthChild1 = LocalDate.parse("1999-07-01", formatter)
      val dateOfBirthChild2 = LocalDate.parse("1994-06-01", formatter)

      val from = LocalDate.parse("2016-05-30", formatter)
      val claimant = Claimant(liveOrWork = false, hoursPerWeek = 16.50, totalIncome = 12000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val child1 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild1, disability = Disability(disabled = true, severelyDisabled = false))
      val child2 = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild2, disability = Disability(disabled = true, severelyDisabled = false))

      val tfc = TFC(from = from, 4, List(claimant), List(child1, child2))
      val result = TFCEligibility.eligibility.determineTFCPeriods(tfc)

      val outputClaimant = OutputClaimant(qualifying = false, isPartner = false, failures = List())
      val startPeriod1 = LocalDate.parse("2016-05-30", formatter)
      val untilPeriod1 = LocalDate.parse("2016-08-30", formatter)
      val startPeriod2 = LocalDate.parse("2016-08-30", formatter)
      val untilPeriod2 = LocalDate.parse("2016-11-30", formatter)
      val startPeriod3 = LocalDate.parse("2016-11-30", formatter)
      val untilPeriod3 = LocalDate.parse("2017-02-28", formatter)
      val startPeriod4 = LocalDate.parse("2017-02-28", formatter)
      val untilPeriod4 = LocalDate.parse("2017-05-28", formatter)

      val Period1OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period1OutputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      val Period2OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period2OutputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      val Period3OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period3OutputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      val Period4OutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val Period4OutputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          claimants = List(outputClaimant),
          children = List(Period1OutputChild1, Period1OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod2,
          until = untilPeriod2,
          claimants = List(outputClaimant),
          children = List(Period2OutputChild1, Period2OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod3,
          until = untilPeriod3,
          claimants = List(outputClaimant),
          children = List(Period3OutputChild1, Period3OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod4,
          until = untilPeriod4,
          claimants = List(outputClaimant),
          children = List(Period4OutputChild1, Period4OutputChild2)
        )
      )
    }

    "determine period eligibility for 1 period - 1 qualifying child" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = OutputClaimant(qualifying = false, isPartner = false, failures = List())
      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())

      val result = TFCEligibility.eligibility.determinePeriodEligibility(List(outputClaimant), List(outputChild1))

      result shouldBe false
    }


    "determine period eligibility for 1 period - 1 qualifying child, partner exists " in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val outputClaimant1 = OutputClaimant(qualifying = true, isPartner = true, failures = List())

      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())

      val result = TFCEligibility.eligibility.determinePeriodEligibility(List(outputClaimant, outputClaimant1), List(outputChild1))

      result shouldBe true
    }

    "determine period eligibility for 1 period - 1 qualifying child, partner exists does not qualify " in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val outputClaimant1 = OutputClaimant(qualifying = false, isPartner = true, failures = List())

      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())

      val result = TFCEligibility.eligibility.determinePeriodEligibility(List(outputClaimant, outputClaimant1), List(outputChild1))

      result shouldBe false
    }

    "determine period eligibility for 1 period - 1 qualifying child, partner exists, claimant does not qualify " in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = OutputClaimant(qualifying = false, isPartner = false, failures = List())
      val outputClaimant1 = OutputClaimant(qualifying = true, isPartner = true, failures = List())

      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())

      val result = TFCEligibility.eligibility.determinePeriodEligibility(List(outputClaimant, outputClaimant1), List(outputChild1))

      result shouldBe false
    }

    "determine period eligibility for 1 period - 1 child, not qualifying, partner not qualifying, claimant does not qualify " in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = OutputClaimant(qualifying = false, isPartner = false, failures = List())
      val outputClaimant1 = OutputClaimant(qualifying = false, isPartner = true, failures = List())

      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = startPeriod1, until = untilPeriod1, failures = List())

      val result = TFCEligibility.eligibility.determinePeriodEligibility(List(outputClaimant, outputClaimant1), List(outputChild1))

      result shouldBe false
    }

    "determine period eligibility for 1 period - 1 child, not qualifying, 1 partner qualifying " in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = OutputClaimant(qualifying = false, isPartner = false, failures = List())
      val outputClaimant1 = OutputClaimant(qualifying = true, isPartner = true, failures = List())

      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = startPeriod1, until = untilPeriod1, failures = List())

      val result = TFCEligibility.eligibility.determinePeriodEligibility(List(outputClaimant, outputClaimant1), List(outputChild1))

      result shouldBe false
    }

    "determine period eligibility for 1 period - no qualifying child and non qual claimant" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = OutputClaimant(qualifying = false, isPartner = false, failures = List())
      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = startPeriod1, until = untilPeriod1, failures = List())

      val result = TFCEligibility.eligibility.determinePeriodEligibility(List(outputClaimant), List(outputChild1))

      result shouldBe false
    }


    "determine period eligibility for 1 period - 1 qualifying child and qual claimant" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())

      val result = TFCEligibility.eligibility.determinePeriodEligibility(List(outputClaimant), List(outputChild1))

      result shouldBe true
    }


    "determine period eligibility for 1 period - non qualifying child and qual claimant" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = startPeriod1, until = untilPeriod1, failures = List())

      val result = TFCEligibility.eligibility.determinePeriodEligibility(List(outputClaimant), List(outputChild1))

      result shouldBe false
    }

    "determine period eligibility for 1 period - 2 children, both qualifying" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())

      val outputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())

      val result = TFCEligibility.eligibility.determinePeriodEligibility(List(outputClaimant), List(outputChild1, outputChild2))

      result shouldBe true
    }

    "determine period eligibility for 1 period - 2 children, both non qualifying" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = startPeriod1, until = untilPeriod1, failures = List())

      val outputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = startPeriod1, until = untilPeriod1, failures = List())

      val result = TFCEligibility.eligibility.determinePeriodEligibility(List(outputClaimant), List(outputChild1, outputChild2))

      result shouldBe false
    }

    "determine period eligibility for 1 period - 2 children, one qualifying" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())

      val outputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = startPeriod1, until = untilPeriod1, failures = List())

      val result = TFCEligibility.eligibility.determinePeriodEligibility(List(outputClaimant), List(outputChild1, outputChild2))

      result shouldBe true
    }

    "determine period eligibility for 1 period - 2 children, one qualifying, one claimaint qualifying" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val outputClaimant1 = OutputClaimant(qualifying = false, isPartner = false, failures = List())

      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())

      val outputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = startPeriod1, until = untilPeriod1, failures = List())

      val result = TFCEligibility.eligibility.determinePeriodEligibility(List(outputClaimant, outputClaimant1), List(outputChild1, outputChild2))

      result shouldBe false
    }

    "determine period eligibility for 1 period - 2 children, one qualifying, both claimaints qualifying" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val outputClaimant1 = OutputClaimant(qualifying = true, isPartner = false, failures = List())

      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())

      val outputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = startPeriod1, until = untilPeriod1, failures = List())

      val result = TFCEligibility.eligibility.determinePeriodEligibility(List(outputClaimant, outputClaimant1), List(outputChild1, outputChild2))

      result shouldBe true
    }

    "determine period eligibility for 1 period - 3 children, all qualifying, both claimaints qualifying" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val outputClaimant1 = OutputClaimant(qualifying = true, isPartner = false, failures = List())

      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())

      val outputChild2 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())

      val outputChild3 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())

      val result = TFCEligibility.eligibility.determinePeriodEligibility(List(outputClaimant, outputClaimant1), List(outputChild1, outputChild2, outputChild3))

      result shouldBe true
    }

    "determine period eligibility for 1 period - 3 children, all non qualifying, both claimaints qualifying" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val outputClaimant1 = OutputClaimant(qualifying = true, isPartner = false, failures = List())

      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = startPeriod1, until = untilPeriod1, failures = List())

      val outputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = startPeriod1, until = untilPeriod1, failures = List())

      val outputChild3 = OutputChild(id = 0, name = None, qualifying = false, from = startPeriod1, until = untilPeriod1, failures = List())

      val result = TFCEligibility.eligibility.determinePeriodEligibility(List(outputClaimant, outputClaimant1), List(outputChild1, outputChild2, outputChild3))

      result shouldBe false
    }

    "determine household if claimant fails eligibility" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-08-27", formatter)
      val from = LocalDate.parse("2016-03-30", formatter)
      val claimant = Claimant(liveOrWork = false, hoursPerWeek = 16.50, totalIncome = 12000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val tfc = TFC(from = from, numberOfPeriods = 1, List(claimant), List(child))
      val result : Future[Eligibility] = TFCEligibility.eligibility.eligibility(Request(payload = Payload(tfc)))

      val outputClaimant = OutputClaimant(qualifying = false, isPartner = false, failures = List())
      val startPeriod = LocalDate.parse("2016-03-30", formatter)
      val untilPeriod = LocalDate.parse("2016-06-30", formatter)
      val PeriodOutputChild = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod, until = untilPeriod, failures = List())
      val tfcPeriods = List(TFCPeriod(from = startPeriod, until = untilPeriod, claimants = List(outputClaimant), children = List(PeriodOutputChild)))
      val tfcEligibilityModel = TFCEligibilityModel(from = from, until = tfcPeriods.last.until, householdEligibility = false, periods = tfcPeriods)
      val eligibilityOutputModel = Eligibility(tfc = Some(tfcEligibilityModel))
      result.tfc shouldBe eligibilityOutputModel.tfc
    }

    "determine household if claimant has eligibility - 2 periods" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-08-27", formatter)
      val from = LocalDate.parse("2016-06-30", formatter)
      val claimant = Claimant(liveOrWork = true, hoursPerWeek = 16.50, totalIncome = 12000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val tfc = TFC(from = from, numberOfPeriods = 2, List(claimant), List(child))
      val result : Future[Eligibility] = TFCEligibility.eligibility.eligibility(Request(payload = Payload(tfc)))

      val outputClaimant = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val outputClaimant1 = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val startPeriod = LocalDate.parse("2016-06-30", formatter)
      val untilPeriod = LocalDate.parse("2016-09-30", formatter)
      val startPeriod1 = LocalDate.parse("2016-09-30", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-30", formatter)
      val septDate = LocalDate.parse("2016-09-04", formatter)
      val PeriodOutputChild = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod, until = septDate, failures = List())
      val PeriodOutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      val tfcPeriods = List(
        TFCPeriod(from = startPeriod,
          until = untilPeriod,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(PeriodOutputChild)),
        TFCPeriod(from = startPeriod1,
          until = untilPeriod1,
          periodEligibility = false,
          claimants = List(outputClaimant1),
          children = List(PeriodOutputChild1))

      )
      val tfcEligibilityModel = TFCEligibilityModel(from = from, until = tfcPeriods.last.until, householdEligibility = true, periods = tfcPeriods)
      val eligibilityOutputModel = Eligibility(tfc = Some(tfcEligibilityModel))
      result.tfc shouldBe eligibilityOutputModel.tfc
    }

    "determine household if claimant and child has eligibility - 2 periods" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2006-08-27", formatter)
      val from = LocalDate.parse("2016-06-30", formatter)
      val claimant = Claimant(liveOrWork = true, hoursPerWeek = 16.50, totalIncome = 12000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val tfc = TFC(from = from, numberOfPeriods = 2, List(claimant), List(child))
      val result : Future[Eligibility] = TFCEligibility.eligibility.eligibility(Request(payload = Payload(tfc)))

      val outputClaimant = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val outputClaimant1 = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val startPeriod = LocalDate.parse("2016-06-30", formatter)
      val untilPeriod = LocalDate.parse("2016-09-30", formatter)
      val startPeriod1 = LocalDate.parse("2016-09-30", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-30", formatter)

      val PeriodOutputChild = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod, until = untilPeriod, failures = List())
      val PeriodOutputChild1 = OutputChild(id = 0, name = None, qualifying = true, from = startPeriod1, until = untilPeriod1, failures = List())

      val tfcPeriods = List(
        TFCPeriod(from = startPeriod,
          until = untilPeriod,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(PeriodOutputChild)),
        TFCPeriod(from = startPeriod1,
          until = untilPeriod1,
          periodEligibility = true,
          claimants = List(outputClaimant1),
          children = List(PeriodOutputChild1))
      )
      val tfcEligibilityModel = TFCEligibilityModel(from = from, until = tfcPeriods.last.until, householdEligibility = true, periods = tfcPeriods)
      val eligibilityOutputModel = Eligibility(tfc = Some(tfcEligibilityModel))
      result.tfc shouldBe eligibilityOutputModel.tfc
    }

    "determine household if claimant has eligibility and child does not have eligibility- 3 periods" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2002-08-27", formatter)
      val from = LocalDate.parse("2016-06-30", formatter)
      val claimant = Claimant(liveOrWork = true, hoursPerWeek = 16.50, totalIncome = 12000, earnedIncome = 2709, isPartner = false, schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val tfc = TFC(from = from, numberOfPeriods = 3, List(claimant), List(child))
      val result : Future[Eligibility] = TFCEligibility.eligibility.eligibility(Request(payload = Payload(tfc)))

      val outputClaimant = OutputClaimant(qualifying = true, isPartner = false, failures = List())
      val startPeriod = LocalDate.parse("2016-06-30", formatter)
      val untilPeriod = LocalDate.parse("2016-09-30", formatter)
      val startPeriod1 = LocalDate.parse("2016-09-30", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-30", formatter)
      val startPeriod2 = LocalDate.parse("2016-12-30", formatter)
      val untilPeriod2 = LocalDate.parse("2017-03-30", formatter)

      val PeriodOutputChild = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val PeriodOutputChild1 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())
      val PeriodOutputChild2 = OutputChild(id = 0, name = None, qualifying = false, from = null, until = null, failures = List())

      val tfcPeriods = List(
        TFCPeriod(from = startPeriod,
          until = untilPeriod,
          periodEligibility = false,
          claimants = List(outputClaimant),
          children = List(PeriodOutputChild)),
        TFCPeriod(from = startPeriod1,
          until = untilPeriod1,
          periodEligibility = false,
          claimants = List(outputClaimant),
          children = List(PeriodOutputChild1)),
        TFCPeriod(from = startPeriod2,
          until = untilPeriod2,
          periodEligibility = false,
          claimants = List(outputClaimant),
          children = List(PeriodOutputChild2))
      )
      val tfcEligibilityModel = TFCEligibilityModel(from = from, until = tfcPeriods.last.until, householdEligibility = false, periods = tfcPeriods)
      val eligibilityOutputModel = Eligibility(tfc = Some(tfcEligibilityModel))
      result.tfc shouldBe eligibilityOutputModel.tfc
    }
  }
}
