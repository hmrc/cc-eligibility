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
import models.input.esc._

import models.output.esc.{ESCEligibilityOutput,ESCPeriod}
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.scalatest.mock.MockitoSugar
import spec.CCSpecConfig
import scala.concurrent.Future

class ESCEligibilitySpec extends CCSpecConfig with FakeCCEligibilityApplication with org.scalatest.PrivateMethodTester with MockitoSugar {

  "ESCEligibilityService" should {

    "return a Future[Eligibility] result" in {
      val service = ESCEligibility
      val result = service.eligibility(ESCEligibilityInput(taxYears = List()))
      result.isInstanceOf[Future[ESCEligibilityOutput]] shouldBe true
    }

    "(no change) determine start dates of periods in the tax year" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2011-08-27", formatter)
      val child = ESCChild(id = 0, dob = dateOfBirth, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = ESCTaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child))

      val decoratedDetermineStartDatesOfPeriodsInTaxYear = PrivateMethod[List[LocalDate]]('determineStartDatesOfPeriodsInTaxYear)
      val result = ESCEligibility invokePrivate decoratedDetermineStartDatesOfPeriodsInTaxYear(taxYear)

      result.length shouldBe 1
    }

    "(1 child, 1 child turning 15) determine start dates of periods in the tax year" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2001-08-27", formatter)
      val child = ESCChild(id = 0, dob = dateOfBirth, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = ESCTaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child))

      val decoratedDetermineStartDatesOfPeriodsInTaxYear = PrivateMethod[List[LocalDate]]('determineStartDatesOfPeriodsInTaxYear)
      val result = ESCEligibility invokePrivate decoratedDetermineStartDatesOfPeriodsInTaxYear(taxYear)

      result.length shouldBe 2
    }

    "(1 children, 1 child turning 16) determine start dates of periods in the tax year" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
      val child = ESCChild(id = 0, dob = dateOfBirth, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = ESCTaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child))

      val decoratedDetermineStartDatesOfPeriodsInTaxYear = PrivateMethod[List[LocalDate]]('determineStartDatesOfPeriodsInTaxYear)
      val result = ESCEligibility invokePrivate decoratedDetermineStartDatesOfPeriodsInTaxYear(taxYear)

      result.length shouldBe 1
    }

    "(1 children, 1 child turning 0) determine start dates of periods in the tax year" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2016-08-27", formatter)
      val child = ESCChild(id = 0, dob = dateOfBirth, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = ESCTaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child))

      val decoratedDetermineStartDatesOfPeriodsInTaxYear = PrivateMethod[List[LocalDate]]('determineStartDatesOfPeriodsInTaxYear)
      val result = ESCEligibility invokePrivate decoratedDetermineStartDatesOfPeriodsInTaxYear(taxYear)

      result.length shouldBe 2
    }

    "(2 children, 1 child turning 0, 1 child not causing a split) determine start dates of periods in the tax year" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2016-08-27", formatter)
      val child = ESCChild(id = 0, dob = dateOfBirth, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = ESCTaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child))

      val decoratedDetermineStartDatesOfPeriodsInTaxYear = PrivateMethod[List[LocalDate]]('determineStartDatesOfPeriodsInTaxYear)
      val result = ESCEligibility invokePrivate decoratedDetermineStartDatesOfPeriodsInTaxYear(taxYear)

      result.length shouldBe 2
    }


    "Determine split dates for tax year, when two children are born before September (return first child's date)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2016-08-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2016-08-28", formatter)
      val child = ESCChild(id = 0, dob = dateOfBirth, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = ESCTaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child, child2))

      val result = ESCEligibility.generateSplitDates(taxYear)
      result shouldBe List(dateOfBirth)
    }

    "Determine split dates for tax year, when two children are born 1 on 1st September and other on 2nd September (return first child's date)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2016-09-01", formatter)
      val dateOfBirth2 = LocalDate.parse("2016-09-02", formatter)
      val child = ESCChild(id = 0, dob = dateOfBirth, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = ESCTaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child, child2))

      val result = ESCEligibility.generateSplitDates(taxYear)
      result shouldBe List(dateOfBirth)
    }

    "Determine split dates for tax year, one turns 15 and other child being born on 2nd September (return 1st Sept and child's dob)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2001-08-01", formatter)
      val dateOfBirth2 = LocalDate.parse("2016-09-02", formatter)
      val september = LocalDate.parse("2016-09-01", formatter)
      val child = ESCChild(id = 0, dob = dateOfBirth, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = ESCTaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child, child2))

      val result = ESCEligibility.determineStartDatesOfPeriodsInTaxYear(taxYear)
      result shouldBe List(today,september,dateOfBirth2)
    }

    "(no split) determine the periods for a tax year" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("2016-05-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2003-06-27", formatter)
      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2), claimants = List(claimant1))
      val result = ESCEligibility.determinePeriodsForTaxYear(taxYear)

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 1,
        qualifying = true
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 10,
        vouchers = true
      )

      result shouldBe List(
        ESCPeriod(
          from = periodStart,
          until = periodEnd,
          claimants = List(
            outputClaimant1
          )
        )
      )
    }

    "(no split, all children too old) determine the periods for a tax year" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("1992-05-27", formatter)
      val dateOfBirth2 = LocalDate.parse("1992-06-27", formatter)
      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2), claimants = List(claimant1))
      val result = ESCEligibility.determinePeriodsForTaxYear(taxYear)

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 1,
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 0,
        vouchers = false
      )

      result shouldBe List(
        ESCPeriod(
          from = periodStart,
          until = periodEnd,
          claimants = List(
            outputClaimant1
          )
        )
      )
    }

    "(no split)(1 child eligible, 1 child being born before 1st September) determine the periods for a tax year" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("2016-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2003-06-27", formatter)
      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child2 = ESCChild(id = 0, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2), claimants = List(claimant1))
      val result = ESCEligibility.determinePeriodsForTaxYear(taxYear)

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 10,
        vouchers = true
      )

      result shouldBe List(
        ESCPeriod(
          from = periodStart,
          until = periodEnd,
          claimants = List(
            outputClaimant1
          )
        )
      )
    }

    "(no split)(child being born on 1st day of month) determine the periods for a tax year" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("2016-07-01", formatter)
      val dateOfBirth2 = LocalDate.parse("2003-06-27", formatter)
      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child2 = ESCChild(id = 0, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2), claimants = List(claimant1))
      val result = ESCEligibility.determinePeriodsForTaxYear(taxYear)

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 10,
        vouchers = true
      )

      result shouldBe List(
        ESCPeriod(
          from = periodStart,
          until = periodEnd,
          claimants = List(
            outputClaimant1
          )
        )
      )
    }

    "(2 split)(1 child becoming old - 15, 1 child being born after september) determine the periods for a tax year" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("2001-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2016-11-27", formatter)
      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)
      val september = LocalDate.parse("2016-09-01", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2), claimants = List(claimant1))
      val result = ESCEligibility.determinePeriodsForTaxYear(taxYear)

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 1,
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 3,
        vouchers = true
      )

      val outputChild3 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )
      val outputChild4 = models.output.esc.OutputChild(
        id = 1,
        qualifying = false
      )

      val outputClaimant2 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 0,
        vouchers = false
      )

      val outputChild5 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )
      val outputChild6 = models.output.esc.OutputChild(
        id = 1,
        qualifying = true
      )

      val outputClaimant3 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 5,
        vouchers = true
      )

      result shouldBe List(
        ESCPeriod(
          from = periodStart,
          until = september,
          claimants = List(
            outputClaimant1
          )
        ),
        ESCPeriod(
          from = september,
          until = dateOfBirth2,
          claimants = List(
            outputClaimant2
          )
        ),
        ESCPeriod(
          from = dateOfBirth2,
          until = periodEnd,
          claimants = List(
            outputClaimant3
          )
        )

      )
    }

    "(1 split)(2 children being born on different dates) determine the periods for a tax year" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("2016-09-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2016-09-28", formatter)
      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)
      val september = LocalDate.parse("2016-09-01", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2), claimants = List(claimant1))
      val result = ESCEligibility.determinePeriodsForTaxYear(taxYear)

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 1,
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 0,
        vouchers = false
      )

      val outputChild3 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )
      val outputChild4 = models.output.esc.OutputChild(
        id = 1,
        qualifying = false
      )

      val outputClaimant2 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 7,
        vouchers = true
      )

      result shouldBe List(
        ESCPeriod(
          from = periodStart,
          until = dateOfBirth1,
          claimants = List(
          outputClaimant1
          )
        ),
        ESCPeriod(
          from = dateOfBirth1,
          until = periodEnd,
          claimants = List(
            outputClaimant2
          )
        )

      )
    }


    "(1 split)(1 child - child being born on 1st Sept) determine the periods for a tax year" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("2016-09-01", formatter)
      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)
      val september = LocalDate.parse("2016-09-01", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1), claimants = List(claimant1))
      val result = ESCEligibility.determinePeriodsForTaxYear(taxYear)

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 0,
        vouchers = false
      )

      val outputChild2 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputClaimant2 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 7,
        vouchers = true
      )

      result shouldBe List(
        ESCPeriod(
          from = periodStart,
          until = september,
          claimants = List(
            outputClaimant1
          )
        ),
        ESCPeriod(
          from = september,
          until = periodEnd,
          claimants = List(outputClaimant2)
        )
      )
    }

    "(no split)(1child turning 16, 1 child eligible ) determine the periods for a tax year" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("2000-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2003-06-27", formatter)
      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)
      val september = LocalDate.parse("2016-09-01", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 0, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2), claimants = List(claimant1))
      val result = ESCEligibility.determinePeriodsForTaxYear(taxYear)

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 10,
        vouchers = true
      )

      result shouldBe List(
        ESCPeriod(
          from = periodStart,
          until = periodEnd,
          claimants = List(
            outputClaimant1
          )
        )
      )
    }

    "(1 split)(child turning 15 and child being born  on 1 Sept) determine the periods for a tax year" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("2001-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2016-09-01", formatter)
      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)
      val september = LocalDate.parse("2016-09-01", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child2 = ESCChild(id = 0, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2), claimants = List(claimant1))
      val result = ESCEligibility.determinePeriodsForTaxYear(taxYear)

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 3,
        vouchers = true
      )

      val outputChild3 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )
      val outputChild4 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputClaimant2 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 7,
        vouchers = true
      )

      result shouldBe List(
        ESCPeriod(
          from = periodStart,
          until = dateOfBirth2,
          claimants = List(
          outputClaimant1
          )
        ),
        ESCPeriod(
          from = dateOfBirth2,
          until = periodEnd,
          claimants = List(
            outputClaimant2
          )
        )
      )
    }

    "(no split)(child turning 15(disabled) and 2 children being born before 1st september) determine the periods for a tax year" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("2001-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2016-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2016-08-19", formatter)

      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)
      val september = LocalDate.parse("2016-09-01", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 0, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 0, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val result = ESCEligibility.determinePeriodsForTaxYear(taxYear)

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )

      val outputChild3 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 10,
        vouchers = true
      )

      result shouldBe List(
        ESCPeriod(
          from = periodStart,
          until = periodEnd,
          claimants = List(
          outputClaimant1
          )
        )
      )
    }

    "(Single tax year)(No splits) determine tax years with periods" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("2004-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2005-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2006-08-19", formatter)

      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 0, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 0, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val request = ESCEligibilityInput(List(taxYear))

      val result = ESCEligibility.constructTaxYearsWithPeriods(request)

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputChild3 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 10,
        vouchers = true
      )

      result shouldBe List(
        models.output.esc.TaxYear(
          from = periodStart,
          until = periodEnd,
          periods = List(
            ESCPeriod(
              from = periodStart,
              until = periodEnd,
              claimants = List(
                outputClaimant1
              )
            )
          )
        )
      )
    }

    "(Single tax year)(No splits - child born after tax year) determine tax years with periods" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("2004-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2005-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-08-19", formatter)

      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 0, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 0, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val request = ESCEligibilityInput(List(taxYear))
      val result = ESCEligibility.constructTaxYearsWithPeriods(request)

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputChild3 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 10,
        vouchers = true
      )

      result shouldBe List(
        models.output.esc.TaxYear(
          from = periodStart,
          until = periodEnd,
          periods = List(
            ESCPeriod(
              from = periodStart,
              until = periodEnd,
              claimants = List(
                outputClaimant1
              )
            )
          )
        )
      )
    }

    "(Single tax year)(No splits - children too old) determine tax years with periods" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("1992-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("1992-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("1992-08-19", formatter)

      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 0, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 0, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val request = ESCEligibilityInput(List(taxYear))

      val result = ESCEligibility.constructTaxYearsWithPeriods(request)

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )

      val outputChild3 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 0,
        vouchers = false
      )

      result shouldBe List(
        models.output.esc.TaxYear(
          from = periodStart,
          until = periodEnd,
          periods = List(
            ESCPeriod(
              from = periodStart,
              until = periodEnd,
              claimants = List(
                outputClaimant1
              )
            )
          )
        )
      )
    }

    "(Single tax year)(no split - child being born when other child is eligible) determine tax years with periods" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("2016-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2005-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2006-08-19", formatter)

      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 0, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 0, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val request = ESCEligibilityInput(List(taxYear))

      val result = ESCEligibility.constructTaxYearsWithPeriods(request)

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputChild3 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 10,
        vouchers = true
      )

      result shouldBe List(
        models.output.esc.TaxYear(
          from = periodStart,
          until = periodEnd,
          periods = List(
            ESCPeriod(
              from = periodStart,
              until = periodEnd,
              claimants = List(
                outputClaimant1
              )
            )
          )
        )
      )
    }

    "(Single tax year)(no split - child turning 15) determine tax years with periods" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("2001-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2005-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2006-08-19", formatter)

      val september = LocalDate.parse("2016-09-01", formatter)

      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child2 = ESCChild(id = 0, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 0, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val request = ESCEligibilityInput(List(taxYear))

      val result = ESCEligibility.constructTaxYearsWithPeriods(request)

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputChild3 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 10,
        vouchers = true
      )

      result shouldBe List(
        models.output.esc.TaxYear(
          from = periodStart,
          until = periodEnd,
          periods = List(
            ESCPeriod(
              from = periodStart,
              until = periodEnd,
              claimants = List(
                outputClaimant1
              )
            )
          )
        )
      )
    }

    "(Single tax year)(no split - child turning 16) determine tax years with periods" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("2000-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2005-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2006-08-19", formatter)

      val september = LocalDate.parse("2016-09-01", formatter)

      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 0, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 0, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val request = ESCEligibilityInput(List(taxYear))

      val result = ESCEligibility.constructTaxYearsWithPeriods(request)

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputChild3 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 10,
        vouchers = true
      )

      result shouldBe List(
        models.output.esc.TaxYear(
          from = periodStart,
          until = periodEnd,
          periods = List(
            ESCPeriod(
              from = periodStart,
              until = periodEnd,
              claimants = List(
                outputClaimant1
              )
            )
          )
        )
      )
    }

    "(Single tax year)(no split - child turning 15, child turning 16, child being born before the other turn 15, 16) determine tax years with periods" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("2000-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2001-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2007-08-19", formatter)

      val september = LocalDate.parse("2016-09-01", formatter)

      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 0, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 0, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val request = ESCEligibilityInput(List(taxYear))

      val result = ESCEligibility.constructTaxYearsWithPeriods(request)

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputChild3 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 10,
        vouchers = true
      )

      result shouldBe List(
        models.output.esc.TaxYear(
          from = periodStart,
          until = periodEnd,
          periods = List(
            ESCPeriod(
              from = periodStart,
              until = periodEnd,
              claimants = List(
                outputClaimant1
              )
            )
          )
        )
      )
    }

    "(Single tax year)(no split - child being born before 1st sept, child turning 16) determine tax years with periods" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("2000-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2005-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2016-08-19", formatter)

      val september = LocalDate.parse("2016-09-01", formatter)

      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 0, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 0, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val request = ESCEligibilityInput(List(taxYear))

      val result = ESCEligibility.constructTaxYearsWithPeriods(request)

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputChild3 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 10,
        vouchers = true
      )

      result shouldBe List(
        models.output.esc.TaxYear(
          from = periodStart,
          until = periodEnd,
          periods = List(
            ESCPeriod(
              from = periodStart,
              until = periodEnd,
              claimants = List(
                outputClaimant1
              )
            )
          )
        )
      )
    }

    "(Single tax year)(1 split - child being born on 1st Sept in the tax year) determine tax years with periods" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("2016-09-01", formatter)

      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1), claimants = List(claimant1))
      val request = ESCEligibilityInput(List(taxYear))

      val result = ESCEligibility.constructTaxYearsWithPeriods(request)

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 0,

        vouchers = false
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputClaimant2 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 7,
        vouchers = true
      )

      result shouldBe List(
        models.output.esc.TaxYear(
          from = periodStart,
          until = periodEnd,
          periods = List(
            ESCPeriod(
              from = periodStart,
              until = dateOfBirth1,
              claimants = List(
                outputClaimant1
              )
            ),
            ESCPeriod(
              from = dateOfBirth1,
              until = periodEnd,
              claimants = List(
                outputClaimant2
              )
            )
          )
        )
      )
    }

    "(Multiple tax years)(TY 1 - 1 split child turning 15 non disabled)(TY 2 - 1 split - child being born after 1st September) determine tax years with periods" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("2001-06-21", formatter)
      val dateOfBirth2 = LocalDate.parse("2017-10-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-11-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-06-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)
      val ty2periodStart = LocalDate.parse("2017-04-06", formatter)
      val ty2periodEnd = LocalDate.parse("2018-04-06", formatter)

      val september1 = LocalDate.parse("2016-09-01", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 2, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear1 = ESCTaxYear(from = ty1periodStart, until = ty1periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val taxYear2 = ESCTaxYear(from = ty2periodStart, until = ty2periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val request = ESCEligibilityInput(List(taxYear1, taxYear2))

      val result = ESCEligibility.constructTaxYearsWithPeriods(request)

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 1,
        qualifying = false
      )

      val outputChild3 = models.output.esc.OutputChild(
        id = 2,
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 3,
        vouchers = true
      )

      val outputChild4 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )
      val outputChild5 = models.output.esc.OutputChild(
        id = 1,
        qualifying = false
      )

      val outputChild6 = models.output.esc.OutputChild(
        id = 2,
        qualifying = false
      )

      val outputClaimant2 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 0,

        vouchers = false
      )


      val outputChild7 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )
      val outputChild8 = models.output.esc.OutputChild(
        id = 1,
        qualifying = false
      )

      val outputChild9 = models.output.esc.OutputChild(
        id = 2,
        qualifying = false
      )

      val outputClaimant3 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 0,

        vouchers = false
      )

      val outputChild10 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )
      val outputChild11 = models.output.esc.OutputChild(
        id = 1,
        qualifying = true
      )

      val outputChild12 = models.output.esc.OutputChild(
        id = 2,
        qualifying = false
      )

      val outputClaimant4 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 6,
        vouchers = true
      )

      result shouldBe List(
        models.output.esc.TaxYear(
          from = ty1periodStart,
          until = ty1periodEnd,
          periods = List(
            ESCPeriod(
              from = ty1periodStart,
              until = september1,
              claimants = List(
                outputClaimant1
              )
            ),
            ESCPeriod(
              from = september1,
              until = ty1periodEnd,
              claimants = List(
                outputClaimant2
              )
            )
          )
        ),
        models.output.esc.TaxYear(
          from = ty2periodStart,
          until = ty2periodEnd,
          periods = List(
            ESCPeriod(
              from = ty2periodStart,
              until = dateOfBirth2,
              claimants = List(
                outputClaimant3
              )
            ),
            ESCPeriod(
              from = dateOfBirth2,
              until = ty2periodEnd,
              claimants = List(
                outputClaimant4
              )
            )
          )
        )
      )
    }

    "(Multiple tax years)(TY 1 - No splits)(TY 2 - no splits - child being born, child turning 15(Disabled)) determine tax years with periods" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("2001-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2014-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-08-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-06-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)
      val ty2periodStart = LocalDate.parse("2017-04-06", formatter)
      val ty2periodEnd = LocalDate.parse("2018-04-06", formatter)

      val september = LocalDate.parse("2017-09-01", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 0, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 0, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear1 = ESCTaxYear(from = ty1periodStart, until = ty1periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val taxYear2 = ESCTaxYear(from = ty2periodStart, until = ty2periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val request = ESCEligibilityInput(List(taxYear1, taxYear2))

      val result = ESCEligibility.constructTaxYearsWithPeriods(request)

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputChild3 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 10,
        vouchers = true
      )

      val outputChild4 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )
      val outputChild5 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputChild6 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )

      val outputClaimant2 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 12,
        vouchers = true
      )

      result shouldBe List(
        models.output.esc.TaxYear(
          from = ty1periodStart,
          until = ty1periodEnd,
          periods = List(
            ESCPeriod(
              from = ty1periodStart,
              until = ty1periodEnd,
              claimants = List(
              outputClaimant1
              )
            )
          )
        ),
        models.output.esc.TaxYear(
          from = ty2periodStart,
          until = ty2periodEnd,
          periods = List(
            ESCPeriod(
              from = ty2periodStart,
              until = ty2periodEnd,
              claimants = List(
                outputClaimant2
              )
            )
          )
        )
      )
    }

    "(Multiple tax years)(TY 1 - No splits)(TY 2 - no splits - child being born, child turning 15) determine tax years with periods" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("2002-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2014-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-08-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-06-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)
      val ty2periodStart = LocalDate.parse("2017-04-06", formatter)
      val ty2periodEnd = LocalDate.parse("2018-04-06", formatter)

      val september = LocalDate.parse("2017-09-01", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 0, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 0, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear1 = ESCTaxYear(from = ty1periodStart, until = ty1periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val taxYear2 = ESCTaxYear(from = ty2periodStart, until = ty2periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val request = ESCEligibilityInput(List(taxYear1, taxYear2))

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputChild3 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 10,
        vouchers = true
      )

      val outputChild4 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )
      val outputChild5 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputChild6 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )

      val outputClaimant2 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 12,
        vouchers = true
      )

      val result = ESCEligibility.constructTaxYearsWithPeriods(request)
      result shouldBe List(
        models.output.esc.TaxYear(
          from = ty1periodStart,
          until = ty1periodEnd,
          periods = List(
            ESCPeriod(
              from = ty1periodStart,
              until = ty1periodEnd,
              claimants = List(
              outputClaimant1
              )
            )
          )
        ),
        models.output.esc.TaxYear(
          from = ty2periodStart,
          until = ty2periodEnd,
          periods = List(
            ESCPeriod(
              from = ty2periodStart,
              until = ty2periodEnd,
              claimants = List(
                outputClaimant2
              )
            )
          )
        )
      )
    }

    "(Multiple tax years)(TY 1 - no split)(TY 2 - 1 split - child turning 16(disabled), child being born) determine tax years with periods" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("2001-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2017-09-01", formatter)
      //val dateOfBirth3 = LocalDate.parse("2016-08-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-06-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)
      val ty2periodStart = LocalDate.parse("2017-04-06", formatter)
      val ty2periodEnd = LocalDate.parse("2018-04-06", formatter)

      val september = LocalDate.parse("2017-09-01", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 0, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear1 = ESCTaxYear(from = ty1periodStart, until = ty1periodEnd, children = List(child1, child2), claimants = List(claimant1))
      val taxYear2 = ESCTaxYear(from = ty2periodStart, until = ty2periodEnd, children = List(child1, child2), claimants = List(claimant1))
      val request = ESCEligibilityInput(List(taxYear1, taxYear2))

      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 10,
        vouchers = true
      )

      val outputChild3 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputChild4 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )

      val outputClaimant2 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 5,
        vouchers = true
      )


      val outputChild5 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )
      val outputChild6 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputClaimant3 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 7,
        vouchers = true
      )

      val result = ESCEligibility.constructTaxYearsWithPeriods(request)
      result shouldBe List(
        models.output.esc.TaxYear(
          from = ty1periodStart,
          until = ty1periodEnd,
          periods = List(
            ESCPeriod(
              from = ty1periodStart,
              until = ty1periodEnd,
              claimants = List(
              outputClaimant1
              )
            )
          )
        ),
        models.output.esc.TaxYear(
          from = ty2periodStart,
          until = ty2periodEnd,
          periods = List(
            ESCPeriod(
              from = ty2periodStart,
              until = september,
              claimants = List(
                outputClaimant2
              )
            ),
            ESCPeriod(
              from = september,
              until = ty2periodEnd,
              claimants = List(
                outputClaimant3
              )
            )
          )
        )
      )
    }

    "(Multiple tax years)(TY 1 - no split - child being born)(TY 2 - no splits - child turning 15, child being born) determine tax years with periods" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("2002-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2017-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2016-08-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-06-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)
      val ty2periodStart = LocalDate.parse("2017-04-06", formatter)
      val ty2periodEnd = LocalDate.parse("2018-04-06", formatter)

      val september = LocalDate.parse("2017-09-01", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 0, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 0, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear1 = ESCTaxYear(from = ty1periodStart, until = ty1periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val taxYear2 = ESCTaxYear(from = ty2periodStart, until = ty2periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val request = ESCEligibilityInput(List(taxYear1, taxYear2))


      val outputChild1 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )
      val outputChild2 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )

      val outputChild3 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 10,
        vouchers = true
      )

      val outputChild4 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )
      val outputChild5 = models.output.esc.OutputChild(
        id = 0,
        qualifying = false
      )

      val outputChild6 = models.output.esc.OutputChild(
        id = 0,
        qualifying = true
      )

      val outputClaimant2 = models.output.esc.ESCOutputClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 12,
        vouchers = true
      )

      val result = ESCEligibility.constructTaxYearsWithPeriods(request)
      result shouldBe List(
        models.output.esc.TaxYear(
          from = ty1periodStart,
          until = ty1periodEnd,
          periods = List(
            ESCPeriod(
              from = ty1periodStart,
              until = ty1periodEnd,
              claimants = List(
              outputClaimant1
              )
            )
          )
        ),
        models.output.esc.TaxYear(
          from = ty2periodStart,
          until = ty2periodEnd,
          periods = List(
            ESCPeriod(
              from = ty2periodStart,
              until = ty2periodEnd,
              claimants = List(
                outputClaimant2
              )
            )
          )
        )
      )
    }

    "determine children's eligibility for a period 1" in {

      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("2000-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2017-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2016-08-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-06-20", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 2, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val result = ESCEligibility.determineChildrensEligibilityForPeriod(List(child1,child2,child3),ty1periodStart)
      result shouldBe List(
        models.output.esc.OutputChild(
          id = 0,
        qualifying = true),
        models.output.esc.OutputChild(
          id = 1,
          qualifying = false),
        models.output.esc.OutputChild(
          id = 2,
          qualifying = false)
      )
    }

    "determine children's eligibility for a period 2" in {

      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("2000-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2017-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2016-08-19", formatter)
      val ty1periodStart = LocalDate.parse("2016-09-20", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 2, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val result = ESCEligibility.determineChildrensEligibilityForPeriod(List(child1,child2,child3),ty1periodStart)
      result shouldBe List(
        models.output.esc.OutputChild(
          id = 0,
          qualifying = false),
        models.output.esc.OutputChild(
          id = 1,
          qualifying = false),
        models.output.esc.OutputChild(
          id = 2,
          qualifying = true)
      )
    }


    "determine children's eligibility for a period 3" in {

      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("1998-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2016-10-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2016-08-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-09-20", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 2, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val result = ESCEligibility.determineChildrensEligibilityForPeriod(List(child1,child2,child3),ty1periodStart)
      result shouldBe List(
        models.output.esc.OutputChild(
          id = 0,
          qualifying = false),
        models.output.esc.OutputChild(
          id = 1,
          qualifying = false),
        models.output.esc.OutputChild(
          id = 2,
          qualifying = true)
      )
    }


    "determine children's eligibility for a period 4" in {

      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("1998-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2016-10-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2016-08-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 2, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val result = ESCEligibility.determineChildrensEligibilityForPeriod(List(child1,child2,child3),ty1periodStart)
      result shouldBe List(
        models.output.esc.OutputChild(
          id = 0,
          qualifying = false),
        models.output.esc.OutputChild(
          id = 1,
          qualifying = true),
        models.output.esc.OutputChild(
          id = 2,
          qualifying = true)
      )
    }

    "determine children's eligibility for a period 5" in {

      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth1 = LocalDate.parse("1998-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2017-12-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-01-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 2, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val result = ESCEligibility.determineChildrensEligibilityForPeriod(List(child1,child2,child3),ty1periodStart)
      result shouldBe List(
        models.output.esc.OutputChild(
          id = 0,
          qualifying = false),
        models.output.esc.OutputChild(
          id = 1,
          qualifying = false),
        models.output.esc.OutputChild(
          id = 2,
          qualifying = false)
      )
    }

    "determine children's eligibility(five children) for a period 6" in {

      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("1998-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2001-12-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-01-19", formatter)
      val dateOfBirth4 = LocalDate.parse("2000-01-19", formatter)
      val dateOfBirth5= LocalDate.parse("2018-01-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 2, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child4 = ESCChild(id = 3, dob = dateOfBirth4, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child5 = ESCChild(id = 4, dob = dateOfBirth5, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val result = ESCEligibility.determineChildrensEligibilityForPeriod(List(child1,child2,child3,child4, child5),ty1periodStart)
      result shouldBe List(
        models.output.esc.OutputChild(
          id = 0,
          qualifying = false),
        models.output.esc.OutputChild(
          id = 1,
          qualifying = true),
        models.output.esc.OutputChild(
          id = 2,
          qualifying = false),
        models.output.esc.OutputChild(
          id = 3,
          qualifying = false),
        models.output.esc.OutputChild(
          id = 4,
          qualifying = false)
      )
    }

    "(qualifying) determine if have a qualifying child for a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("1998-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2001-12-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-01-19", formatter)
      val dateOfBirth4 = LocalDate.parse("2000-01-19", formatter)
      val dateOfBirth5= LocalDate.parse("2018-01-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 2, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child4 = ESCChild(id = 3, dob = dateOfBirth4, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child5 = ESCChild(id = 4, dob = dateOfBirth5, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val result = ESCEligibility.hasQualifyingChildForPeriod(List(child1,child2,child3,child4,child5), ty1periodStart)
      result shouldBe true
    }

    "(non qualifying) determine if have a qualifying child for a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("1998-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("1992-12-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-01-19", formatter)
      val dateOfBirth4 = LocalDate.parse("2000-01-19", formatter)
      val dateOfBirth5= LocalDate.parse("2018-01-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 2, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child4 = ESCChild(id = 3, dob = dateOfBirth4, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child5 = ESCChild(id = 4, dob = dateOfBirth5, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val result = ESCEligibility.hasQualifyingChildForPeriod(List(child1,child2,child3,child4,child5), ty1periodStart)
      result shouldBe false
    }

    "(single claimant)(no children)(employer providing vouchers) determine a claimants eligibility for a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)

      val result = ESCEligibility.determineClaimantsEligibilityForPeriod(List(), List(claimant1), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCOutputClaimant(
          qualifying = true,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        )
      )
    }

    "(multiple claimants)(no children)(employer providing vouchers) determine a claimants eligibility for a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val claimant2 = ESCClaimant(isPartner = true, employerProvidesESC = true)

      val result = ESCEligibility.determineClaimantsEligibilityForPeriod(List(), List(claimant1, claimant2), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCOutputClaimant(
          qualifying = true,
          isPartner = false,
          eligibleMonthsInPeriod = 0,vouchers = false
        ),
        models.output.esc.ESCOutputClaimant(
          qualifying = true,
          isPartner = true,
          eligibleMonthsInPeriod = 0,vouchers = false
        )
      )
    }

    "(multiple claimants)(no children)(employer providing vouchers for 1 claimant) determine a claimants eligibility for a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = false)
      val claimant2 = ESCClaimant(isPartner = true, employerProvidesESC = true)

      val result = ESCEligibility.determineClaimantsEligibilityForPeriod(List(), List(claimant1, claimant2), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCOutputClaimant(
          qualifying = false,
          isPartner = false,
          eligibleMonthsInPeriod = 0,vouchers = false
        ),
        models.output.esc.ESCOutputClaimant(
          qualifying = true,
          isPartner = true,
          eligibleMonthsInPeriod = 0,vouchers = false
        )
      )
    }

    "(single claimant)(no children)(employer not providing vouchers) determine a claimants eligibility for a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = false)

      val result = ESCEligibility.determineClaimantsEligibilityForPeriod(List(), List(claimant1), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCOutputClaimant(
          qualifying = false,
          isPartner = false,
          eligibleMonthsInPeriod = 0,vouchers = false
        )
      )
    }

    "(multiple claimants)(no children)(employer not providing vouchers) determine a claimants eligibility for a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = false)
      val claimant2 = ESCClaimant(isPartner = true, employerProvidesESC = false)

      val result = ESCEligibility.determineClaimantsEligibilityForPeriod(List(), List(claimant1, claimant2), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCOutputClaimant(
          qualifying = false,
          isPartner = false,
          eligibleMonthsInPeriod = 0,vouchers = false
        ),
        models.output.esc.ESCOutputClaimant(
          qualifying = false,
          isPartner = true,
          eligibleMonthsInPeriod = 0,vouchers = false
        )
      )
    }

    "(single claimant)(qualifying child)(employer providing vouchers) determine a claimants eligibility for a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("1998-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2001-12-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-01-19", formatter)
      val dateOfBirth4 = LocalDate.parse("2000-01-19", formatter)
      val dateOfBirth5= LocalDate.parse("2018-01-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 2, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child4 = ESCChild(id = 3, dob = dateOfBirth4, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child5 = ESCChild(id = 4, dob = dateOfBirth5, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)

      val result = ESCEligibility.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCOutputClaimant(
          qualifying = true,
          isPartner = false,
          eligibleMonthsInPeriod = 5,

          vouchers = true
        )
      )
    }

    "(multiple claimants, one qualifying claimant)(qualifying child)(employer providing vouchers) determine a claimants eligibility for a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("1998-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2001-12-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-01-19", formatter)
      val dateOfBirth4 = LocalDate.parse("2000-01-19", formatter)
      val dateOfBirth5= LocalDate.parse("2018-01-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 2, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child4 = ESCChild(id = 3, dob = dateOfBirth4, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child5 = ESCChild(id = 4, dob = dateOfBirth5, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = false)
      val claimant2 = ESCClaimant(isPartner = true, employerProvidesESC = true)

      val result = ESCEligibility.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1, claimant2), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCOutputClaimant(
          qualifying = false,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        ),
        models.output.esc.ESCOutputClaimant(
          qualifying = true,
          isPartner = true,
          eligibleMonthsInPeriod = 5,
          vouchers = true
        )
      )
    }

    "(multiple claimants)(qualifying child)(employer providing vouchers for 1 claimant) determine a claimants eligibility for a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("1998-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2001-12-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-01-19", formatter)
      val dateOfBirth4 = LocalDate.parse("2000-01-19", formatter)
      val dateOfBirth5= LocalDate.parse("2018-01-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 2, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child4 = ESCChild(id = 3, dob = dateOfBirth4, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child5 = ESCChild(id = 4, dob = dateOfBirth5, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val claimant2 = ESCClaimant(isPartner = true, employerProvidesESC = false)

      val result = ESCEligibility.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1, claimant2), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCOutputClaimant(
          qualifying = true,
          isPartner = false,
          eligibleMonthsInPeriod = 5,
          vouchers = true
        ),
        models.output.esc.ESCOutputClaimant(
          qualifying = false,
          isPartner = true,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        )
      )
    }

    "(single claimant)(qualifying child)(employer not providing vouchers) determine a claimants eligibility for a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("1998-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2001-12-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-01-19", formatter)
      val dateOfBirth4 = LocalDate.parse("2000-01-19", formatter)
      val dateOfBirth5= LocalDate.parse("2018-01-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 2, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child4 = ESCChild(id = 3, dob = dateOfBirth4, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child5 = ESCChild(id = 4, dob = dateOfBirth5, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = false)

      val result = ESCEligibility.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCOutputClaimant(
          qualifying = false,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        )
      )
    }

    "(multiple claimants)(qualifying child)(employer not providing vouchers) determine a claimants eligibility for a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("1998-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2001-12-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-01-19", formatter)
      val dateOfBirth4 = LocalDate.parse("2000-01-19", formatter)
      val dateOfBirth5= LocalDate.parse("2018-01-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 2, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child4 = ESCChild(id = 3, dob = dateOfBirth4, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child5 = ESCChild(id = 4, dob = dateOfBirth5, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = false)
      val claimant2 = ESCClaimant(isPartner = true, employerProvidesESC = false)

      val result = ESCEligibility.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1, claimant2), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCOutputClaimant(
          qualifying = false,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        ),
        models.output.esc.ESCOutputClaimant(
          qualifying = false,
          isPartner = true,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        )
      )
    }

    "(single claimant)(no qualifying children)(employer providing vouchers) determine a claimants eligibility for a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("1998-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("1992-12-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-01-19", formatter)
      val dateOfBirth4 = LocalDate.parse("2000-01-19", formatter)
      val dateOfBirth5= LocalDate.parse("2018-01-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 2, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child4 = ESCChild(id = 3, dob = dateOfBirth4, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child5 = ESCChild(id = 4, dob = dateOfBirth5, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)

      val result = ESCEligibility.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCOutputClaimant(
          qualifying = true,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        )
      )
    }

    "(multiple claimants)(no qualifying children)(employer providing vouchers) determine a claimants eligibility for a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("1998-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("1992-12-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-01-19", formatter)
      val dateOfBirth4 = LocalDate.parse("2000-01-19", formatter)
      val dateOfBirth5= LocalDate.parse("2018-01-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 2, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child4 = ESCChild(id = 3, dob = dateOfBirth4, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child5 = ESCChild(id = 4, dob = dateOfBirth5, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val claimant2 = ESCClaimant(isPartner = true, employerProvidesESC = true)

      val result = ESCEligibility.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1, claimant2), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCOutputClaimant(
          qualifying = true,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        ),
        models.output.esc.ESCOutputClaimant(
          qualifying = true,
          isPartner = true,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        )
      )
    }

    "(multiple claimants)(no qualifying children)(employer providing vouchers for 1 claimant) determine a claimants eligibility for a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("1998-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("1992-12-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-01-19", formatter)
      val dateOfBirth4 = LocalDate.parse("2000-01-19", formatter)
      val dateOfBirth5= LocalDate.parse("2018-01-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 2, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child4 = ESCChild(id = 3, dob = dateOfBirth4, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child5 = ESCChild(id = 4, dob = dateOfBirth5, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val claimant2 = ESCClaimant(isPartner = true, employerProvidesESC = false)

      val result = ESCEligibility.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1, claimant2), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCOutputClaimant(
          qualifying = true,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        ),
        models.output.esc.ESCOutputClaimant(
          qualifying = false,
          isPartner = true,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        )
      )
    }

    "(single claimant)(no qualifying children)(employer not providing vouchers) determine a claimants eligibility for a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("1998-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("1992-12-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-01-19", formatter)
      val dateOfBirth4 = LocalDate.parse("2000-01-19", formatter)
      val dateOfBirth5= LocalDate.parse("2018-01-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 2, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child4 = ESCChild(id = 3, dob = dateOfBirth4, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child5 = ESCChild(id = 4, dob = dateOfBirth5, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = false)

      val result = ESCEligibility.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCOutputClaimant(
          qualifying = false,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        )
      )
    }

    "(multiple claimants)(no qualifying children)(employer not providing vouchers) determine a claimants eligibility for a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("1998-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("1992-12-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-01-19", formatter)
      val dateOfBirth4 = LocalDate.parse("2000-01-19", formatter)
      val dateOfBirth5= LocalDate.parse("2018-01-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 2, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child4 = ESCChild(id = 3, dob = dateOfBirth4, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child5 = ESCChild(id = 4, dob = dateOfBirth5, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = false)
      val claimant2 = ESCClaimant(isPartner = true, employerProvidesESC = false)

      val result = ESCEligibility.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1, claimant2), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCOutputClaimant(
          qualifying = false,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        ),
        models.output.esc.ESCOutputClaimant(
          qualifying = false,
          isPartner = true,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        )
      )
    }

    "(single claimant)(1 qualifying child)(employer providing vouchers, claimant not receiving vouchers) determine a claimants eligibility for a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("1998-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2013-12-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-01-19", formatter)
      val dateOfBirth4 = LocalDate.parse("2000-01-19", formatter)
      val dateOfBirth5= LocalDate.parse("2018-01-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 2, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child4 = ESCChild(id = 3, dob = dateOfBirth4, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child5 = ESCChild(id = 4, dob = dateOfBirth5, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = false)

      val result = ESCEligibility.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCOutputClaimant(
          qualifying = false,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        )
      )
    }

    "(single claimant)(1 qualifying child)(claimant does not work in UK) determine a claimants eligibility for a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val dateOfBirth1 = LocalDate.parse("1998-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2013-12-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-01-19", formatter)
      val dateOfBirth4 = LocalDate.parse("2000-01-19", formatter)
      val dateOfBirth5= LocalDate.parse("2018-01-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = ESCChild(id = 0, dob = dateOfBirth1, disability = ESCDisability(disabled = true, severelyDisabled = false))
      val child2 = ESCChild(id = 1, dob = dateOfBirth2, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child3 = ESCChild(id = 2, dob = dateOfBirth3, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child4 = ESCChild(id = 3, dob = dateOfBirth4, disability = ESCDisability(disabled = false, severelyDisabled = false))
      val child5 = ESCChild(id = 4, dob = dateOfBirth5, disability = ESCDisability(disabled = false, severelyDisabled = false))

      val claimant1 = ESCClaimant(isPartner = false)

      val result = ESCEligibility.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCOutputClaimant(
          qualifying = false,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        )
      )
    }

    "(Single tax year)(multiple claimants, no qualifying children) determine the number of qualifying months in a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val ty1periodStart = LocalDate.parse("2016-06-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val result = ESCEligibility.numberOfQualifyingMonthsForPeriod(qualifying = false, ty1periodStart, ty1periodEnd)
      result shouldBe 0
    }

    "(Single tax year)(multiple claimants, qualifying children) determine the number of qualifying months in a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val ty1periodStart = LocalDate.parse("2016-06-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val result = ESCEligibility.numberOfQualifyingMonthsForPeriod(qualifying = true, ty1periodStart, ty1periodEnd)
      result shouldBe 10
    }

    "(Multiple tax year)(multiple claimants, qualifying children) determine the number of qualifying months in a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

      val ty1periodStart = LocalDate.parse("2015-06-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val result = ESCEligibility.numberOfQualifyingMonthsForPeriod(qualifying = true, ty1periodStart, ty1periodEnd)
      result shouldBe 22
    }

    "(Single tax year) calculate qualifying months period start 1st of the month" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val ty1periodStart = LocalDate.parse("2016-04-1", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val result = ESCEligibility.numberOfQualifyingMonthsForPeriod(qualifying = true, ty1periodStart, ty1periodEnd)
      result shouldBe 12
    }

    "calculate qualifying months period start in middle of the month" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val ty1periodStart = LocalDate.parse("2016-04-15", formatter)
      val ty1periodEnd = LocalDate.parse("2016-09-01", formatter)

      val result = ESCEligibility.numberOfQualifyingMonthsForPeriod(qualifying = true, ty1periodStart, ty1periodEnd)
      result shouldBe 5
    }

    "calculate qualifying months period start in middle of the month, period end is 6th april" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val ty1periodStart = LocalDate.parse("2016-06-15", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val result = ESCEligibility.numberOfQualifyingMonthsForPeriod(qualifying = true, ty1periodStart, ty1periodEnd)
      result shouldBe 10
    }

    "calculate qualifying months period start in middle of the month period end end of the month" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val ty1periodStart = LocalDate.parse("2017-04-06", formatter)
      val ty1periodEnd = LocalDate.parse("2017-06-15", formatter)

      val result = ESCEligibility.numberOfQualifyingMonthsForPeriod(qualifying = true, ty1periodStart, ty1periodEnd)
      result shouldBe 2
    }
  }
}
