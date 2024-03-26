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

package eligibility

import controllers.FakeCCEligibilityApplication
import fixtures.ESCChildren
import models.input.esc._
import models.output.esc.{ESCEligibilityOutput, ESCPeriod}
import java.time.LocalDate
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar
import utils.{CCConfig, ESCConfig}

import scala.concurrent.Future

class ESCEligibilitySpec extends FakeCCEligibilityApplication with Matchers
  with org.scalatest.PrivateMethodTester with MockitoSugar
  with ESCChildren {


  override def eSCConfig: Option[ESCConfig] = Some(app.injector.instanceOf[ESCConfig])
  override def ccConfig: Option[CCConfig] = Some(app.injector.instanceOf[CCConfig])

  val service = app.injector.instanceOf[ESCEligibility]

  "ESCEligibilityService" must {

    "return a Future[Eligibility] result" in {
      val result = service.eligibility(ESCEligibilityInput(escTaxYears = List()), eSCConfig.get, ccConfig.get)
      result.isInstanceOf[Future[ESCEligibilityOutput]] shouldBe true
    }

    "(no change) determine start dates of periods in the tax year" in {
      val dateOfBirth = LocalDate.parse("2011-08-27", formatter)
      val child = buildChild(dob = dateOfBirth)
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = ESCTaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child))

      val decoratedDetermineStartDatesOfPeriodsInTaxYear = PrivateMethod[List[LocalDate]](Symbol("determineStartDatesOfPeriodsInTaxYear"))
      val result = service invokePrivate decoratedDetermineStartDatesOfPeriodsInTaxYear(taxYear)

      result.length shouldBe 1
    }

    "(1 child, 1 child turning 15) determine start dates of periods in the tax year" in {
      val dateOfBirth = LocalDate.parse("2001-08-27", formatter)
      val child = buildChild(dob = dateOfBirth)
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = ESCTaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child))

      val decoratedDetermineStartDatesOfPeriodsInTaxYear = PrivateMethod[List[LocalDate]](Symbol("determineStartDatesOfPeriodsInTaxYear"))
      val result = service invokePrivate decoratedDetermineStartDatesOfPeriodsInTaxYear(taxYear)

      result.length shouldBe 2
    }

    "(1 children, 1 child turning 16) determine start dates of periods in the tax year" in {
      val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
      val child = buildChild(dob = dateOfBirth)
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = ESCTaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child))

      val decoratedDetermineStartDatesOfPeriodsInTaxYear = PrivateMethod[List[LocalDate]](Symbol("determineStartDatesOfPeriodsInTaxYear"))
      val result = service invokePrivate decoratedDetermineStartDatesOfPeriodsInTaxYear(taxYear)

      result.length shouldBe 1
    }

    "(1 children, 1 child turning 0) determine start dates of periods in the tax year" in {
      val dateOfBirth = LocalDate.parse("2016-08-27", formatter)
      val child = buildChild(dob = dateOfBirth)
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = ESCTaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child))

      val decoratedDetermineStartDatesOfPeriodsInTaxYear = PrivateMethod[List[LocalDate]](Symbol("determineStartDatesOfPeriodsInTaxYear"))
      val result = service invokePrivate decoratedDetermineStartDatesOfPeriodsInTaxYear(taxYear)

      result.length shouldBe 2
    }

    "(2 children, 1 child turning 0, 1 child not causing a split) determine start dates of periods in the tax year" in {
      val dateOfBirth = LocalDate.parse("2016-08-27", formatter)
      val child = buildChild(dob = dateOfBirth)
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = ESCTaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child))

      val decoratedDetermineStartDatesOfPeriodsInTaxYear = PrivateMethod[List[LocalDate]](Symbol("determineStartDatesOfPeriodsInTaxYear"))
      val result = service invokePrivate decoratedDetermineStartDatesOfPeriodsInTaxYear(taxYear)

      result.length shouldBe 2
    }


    "Determine split dates for tax year, when two children are born before September" in {
      val dateOfBirth = LocalDate.parse("2016-08-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2016-08-28", formatter)
      val child = buildChild(dob = dateOfBirth)
      val child2 = buildChild(id = 1, dob = dateOfBirth2)
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = ESCTaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child, child2))

      val result = service.generateSplitDates(taxYear)
      result shouldBe List(dateOfBirth, dateOfBirth2)
    }

    "Determine split dates for tax year, when two children are born 1 on 1st September and other on 2nd September" in {
      val dateOfBirth = LocalDate.parse("2016-09-01", formatter)
      val dateOfBirth2 = LocalDate.parse("2016-09-02", formatter)
      val child = buildChild(dob = dateOfBirth)
      val child2 = buildChild(id = 1, dob = dateOfBirth2)
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = ESCTaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child, child2))

      val result = service.generateSplitDates(taxYear)
      result shouldBe List(dateOfBirth, dateOfBirth2)
    }

    "Determine split dates for tax year, one turns 15 and other child being born on 2nd September (return 1st Sept and child's dob)" in {
      val dateOfBirth = LocalDate.parse("2001-08-01", formatter)
      val dateOfBirth2 = LocalDate.parse("2016-09-02", formatter)
      val september = LocalDate.parse("2016-09-01", formatter)
      val child = buildChild(dob = dateOfBirth)
      val child2 = buildChild(id = 1, dob = dateOfBirth2)
      val today = LocalDate.parse("2016-07-26", formatter)
      val endTaxYear = LocalDate.parse("2017-04-06", formatter)
      val taxYear = ESCTaxYear(from = today, until = endTaxYear, claimants = List(), children = List(child, child2))

      val result = service.determineStartDatesOfPeriodsInTaxYear(taxYear)
      result shouldBe List(today,september,dateOfBirth2)
    }

    "(no split) determine the periods for a tax year" in {
      val dateOfBirth1 = LocalDate.parse("2016-05-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2003-06-27", formatter)
      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildChild(dob = dateOfBirth1)
      val child2 = buildChild(id = 1, dob = dateOfBirth2)
      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2), claimants = List(claimant1))
      val result = service.determinePeriodsForTaxYear(taxYear)

      val outputChild1 = buildOutputChild(
        qualifying = true
      )
      val outputChild2 = buildOutputChild(
        qualifying = true
      )

      val outputClaimant1 = models.output.esc.ESCClaimant(
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
          ),
          children = List(
            outputChild1,
            outputChild2
          )
        )
      )
    }

    "(no split, all children too old) determine the periods for a tax year" in {
      val dateOfBirth1 = LocalDate.parse("1992-05-27", formatter)
      val dateOfBirth2 = LocalDate.parse("1992-06-27", formatter)
      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildChild(dob = dateOfBirth1)
      val child2 = buildChild(id = 1, dob = dateOfBirth2)
      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2), claimants = List(claimant1))
      val result = service.determinePeriodsForTaxYear(taxYear)

      val outputChild1 = buildOutputChild(
        qualifying = false
      )
      val outputChild2 = buildOutputChild(
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCClaimant(
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
          ),
          children = List(
            outputChild1,
            outputChild2
          )
        )
      )
    }

    "(split)(1 child eligible, 1 child being born before 1st September) determine the periods for a tax year" in {
      val dateOfBirth1 = LocalDate.parse("2016-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2003-06-27", formatter)
      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildChild(dob = dateOfBirth1)
      val child2 = buildChild(dob = dateOfBirth2)
      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2), claimants = List(claimant1))
      val result = service.determinePeriodsForTaxYear(taxYear)

      result shouldBe List(
        ESCPeriod(
          from = periodStart,
          until = dateOfBirth1,
          claimants = List(
            models.output.esc.ESCClaimant(
              qualifying = true,
              isPartner = false,
              eligibleMonthsInPeriod = 0,
              vouchers = true
            )
          ),
          children = List(
            buildOutputChild(qualifying = false),
            buildOutputChild(qualifying = true)
          )
        ),
        ESCPeriod(
          from = dateOfBirth1,
          until = periodEnd,
          claimants = List(
            models.output.esc.ESCClaimant(
              qualifying = true,
              isPartner = false,
              eligibleMonthsInPeriod = 10,
              vouchers = true
            )
          ),
          children = List(
            buildOutputChild(qualifying = true),
            buildOutputChild(qualifying = true)
          )
        )
      )
    }

    "(split)(child being born on 1st day of month) determine the periods for a tax year" in {
      val dateOfBirth1 = LocalDate.parse("2016-07-01", formatter)
      val dateOfBirth2 = LocalDate.parse("2003-06-27", formatter)
      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildChild(dob = dateOfBirth1)
      val child2 = buildChild(dob = dateOfBirth2)
      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2), claimants = List(claimant1))
      val result = service.determinePeriodsForTaxYear(taxYear)

      result shouldBe List(
        ESCPeriod(
          from = periodStart,
          until = dateOfBirth1,
          claimants = List(
            models.output.esc.ESCClaimant(
              qualifying = true,
              isPartner = false,
              eligibleMonthsInPeriod = 1,
              vouchers = true
            )
          ),
          children = List(
            buildOutputChild(qualifying = false),
            buildOutputChild(qualifying = true)
          )
        ),
        ESCPeriod(
          from = dateOfBirth1,
          until = periodEnd,
          claimants = List(
            models.output.esc.ESCClaimant(
              qualifying = true,
              isPartner = false,
              eligibleMonthsInPeriod = 9,
              vouchers = true
            )
          ),
          children = List(
            buildOutputChild(qualifying = true),
            buildOutputChild(qualifying = true)
          )
        )
      )
    }

    "(2 split)(1 child becoming old - 15, 1 child being born after september) determine the periods for a tax year" in {
      val dateOfBirth1 = LocalDate.parse("2001-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2016-11-27", formatter)
      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)
      val september = LocalDate.parse("2016-09-01", formatter)

      val child1 = buildChild(dob = dateOfBirth1)
      val child2 = buildChild(dob = dateOfBirth2)
      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2), claimants = List(claimant1))
      val result = service.determinePeriodsForTaxYear(taxYear)

      val outputChild1 = buildOutputChild(
        qualifying = true
      )
      val outputChild2 = buildOutputChild(
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 3,
        vouchers = true
      )

      val outputChild3 = buildOutputChild(
        qualifying = false
      )
      val outputChild4 = buildOutputChild(
        qualifying = false
      )

      val outputClaimant2 = models.output.esc.ESCClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 0,
        vouchers = false
      )

      val outputChild5 = buildOutputChild(
        qualifying = false
      )
      val outputChild6 = buildOutputChild(
        qualifying = true
      )

      val outputClaimant3 = models.output.esc.ESCClaimant(
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
          ),
          children = List(
            outputChild1,
            outputChild2
          )
        ),
        ESCPeriod(
          from = september,
          until = dateOfBirth2,
          claimants = List(
            outputClaimant2
          ),
          children = List(
            outputChild3,
            outputChild4
          )
        ),
        ESCPeriod(
          from = dateOfBirth2,
          until = periodEnd,
          claimants = List(
            outputClaimant3
          ),
          children = List(
            outputChild5,
            outputChild6
          )
        )

      )
    }

    "(2 splits)(2 children being born on different dates) determine the periods for a tax year" in {
      val dateOfBirth1 = LocalDate.parse("2016-09-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2016-09-28", formatter)
      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildChild(dob = dateOfBirth1)
      val child2 = buildChild(dob = dateOfBirth2)
      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2), claimants = List(claimant1))
      val result = service.determinePeriodsForTaxYear(taxYear)

      result shouldBe List(
        ESCPeriod(
          from = periodStart,
          until = dateOfBirth1,
          claimants = List(
            models.output.esc.ESCClaimant(
              qualifying = true,
              isPartner = false,
              eligibleMonthsInPeriod = 0,
              vouchers = false
            )
          ),
          children = List(
            buildOutputChild(qualifying = false),
            buildOutputChild(qualifying = false)
          )
        ),
        ESCPeriod(
          from = dateOfBirth1,
          until = dateOfBirth2,
          claimants = List(
            models.output.esc.ESCClaimant(
              qualifying = true,
              isPartner = false,
              eligibleMonthsInPeriod = 0,
              vouchers = true
            )
          ),
          children = List(
            buildOutputChild(qualifying = true),
            buildOutputChild(qualifying = false)
          )
        ),
        ESCPeriod(
          from = dateOfBirth2,
          until = periodEnd,
          claimants = List(
            models.output.esc.ESCClaimant(
              qualifying = true,
              isPartner = false,
              eligibleMonthsInPeriod = 7,
              vouchers = true
            )
          ),
          children = List(
            buildOutputChild(qualifying = true),
            buildOutputChild(qualifying = true)
          )
        )
      )
    }


    "(1 split)(1 child - child being born on 1st Sept) determine the periods for a tax year" in {
      val dateOfBirth1 = LocalDate.parse("2016-09-01", formatter)
      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)
      val september = LocalDate.parse("2016-09-01", formatter)

      val child1 = buildChild(dob = dateOfBirth1)
      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1), claimants = List(claimant1))
      val result = service.determinePeriodsForTaxYear(taxYear)

      val outputChild1 = buildOutputChild(
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 0,
        vouchers = false
      )

      val outputChild2 = buildOutputChild(
        qualifying = true
      )

      val outputClaimant2 = models.output.esc.ESCClaimant(
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
          ),
          children = List(
            outputChild1
          )
        ),
        ESCPeriod(
          from = september,
          until = periodEnd,
          claimants = List(
            outputClaimant2
          ),
          children = List(
            outputChild2
          )
        )
      )
    }

    "(split)(1child turning 16, 1 child eligible ) determine the periods for a tax year" in {
      val dateOfBirth1 = LocalDate.parse("2000-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2003-06-27", formatter)
      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)
      val september = LocalDate.parse("2016-09-01", formatter)

      val child1 = buildChild(dob = dateOfBirth1, disabled = true)
      val child2 = buildChild(dob = dateOfBirth2)
      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2), claimants = List(claimant1))
      val result = service.determinePeriodsForTaxYear(taxYear)

      result shouldBe List(
        ESCPeriod(
          from = periodStart,
          until = september,
          claimants = List(
            models.output.esc.ESCClaimant(
              qualifying = true,
              isPartner = false,
              eligibleMonthsInPeriod = 3,
              vouchers = true
            )
          ),
          children = List(
            buildOutputChild(qualifying = true),
            buildOutputChild(qualifying = true)
          )
        ),
        ESCPeriod(
          from = september,
          until = periodEnd,
          claimants = List(
            models.output.esc.ESCClaimant(
              qualifying = true,
              isPartner = false,
              eligibleMonthsInPeriod = 7,
              vouchers = true
            )
          ),
          children = List(
            buildOutputChild(qualifying = false),
            buildOutputChild(qualifying = true)
          )
        )
      )
    }

    "(1 split)(child turning 15 and child being born  on 1 Sept) determine the periods for a tax year" in {
      val dateOfBirth1 = LocalDate.parse("2001-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2016-09-01", formatter)
      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildChild(dob = dateOfBirth1)
      val child2 = buildChild(dob = dateOfBirth2)
      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2), claimants = List(claimant1))
      val result = service.determinePeriodsForTaxYear(taxYear)

      val outputChild1 = buildOutputChild(
        qualifying = true
      )
      val outputChild2 = buildOutputChild(
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 3,
        vouchers = true
      )

      val outputChild3 = buildOutputChild(
        qualifying = false
      )
      val outputChild4 = buildOutputChild(
        qualifying = true
      )

      val outputClaimant2 = models.output.esc.ESCClaimant(
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
          ),
          children = List(
            outputChild1,
            outputChild2
          )
        ),
        ESCPeriod(
          from = dateOfBirth2,
          until = periodEnd,
          claimants = List(
            outputClaimant2
          ),
          children = List(
            outputChild3,
            outputChild4
          )
        )
      )
    }

    "(split)(child turning 15(disabled) and 2 children being born before 1st september) determine the periods for a tax year" in {
      val dateOfBirth1 = LocalDate.parse("2001-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2016-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2016-08-19", formatter)

      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildChild(dob = dateOfBirth1, disabled = true)
      val child2 = buildChild(dob = dateOfBirth2)
      val child3 = buildChild(dob = dateOfBirth3)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val result = service.determinePeriodsForTaxYear(taxYear)

      result shouldBe List(
        ESCPeriod(
          from = periodStart,
          until = dateOfBirth2,
          claimants = List(
            models.output.esc.ESCClaimant(
              qualifying = true,
              isPartner = false,
              eligibleMonthsInPeriod = 0,
              vouchers = true
            )
          ),
          children = List(
            buildOutputChild(qualifying = true),
            buildOutputChild(qualifying = false),
            buildOutputChild(qualifying = false)
          )
        ),
        ESCPeriod(
          from = dateOfBirth2,
          until = dateOfBirth3,
          claimants = List(
            models.output.esc.ESCClaimant(
              qualifying = true,
              isPartner = false,
              eligibleMonthsInPeriod = 2,
              vouchers = true
            )
          ),
          children = List(
            buildOutputChild(qualifying = true),
            buildOutputChild(qualifying = true),
            buildOutputChild(qualifying = false)
          )
        ),
        ESCPeriod(
          from = dateOfBirth3,
          until = periodEnd,
          claimants = List(
            models.output.esc.ESCClaimant(
              qualifying = true,
              isPartner = false,
              eligibleMonthsInPeriod = 8,
              vouchers = true
            )
          ),
          children = List(
            buildOutputChild(qualifying = true),
            buildOutputChild(qualifying = true),
            buildOutputChild(qualifying = true)
          )
        )
      )
    }

    "(Single tax year)(No splits) determine tax years with periods" in {
      val dateOfBirth1 = LocalDate.parse("2004-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2005-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2006-08-19", formatter)

      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildChild(dob = dateOfBirth1, disabled = true)
      val child2 = buildChild(dob = dateOfBirth2)
      val child3 = buildChild(dob = dateOfBirth3)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)

      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val result = service.constructTaxYearsWithPeriods(List(taxYear))

      val outputChild1 = buildOutputChild(
        qualifying = true
      )
      val outputChild2 = buildOutputChild(
        qualifying = true
      )

      val outputChild3 = buildOutputChild(
        qualifying = true
      )

      val outputClaimant1 = models.output.esc.ESCClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 10,
        vouchers = true
      )

      result shouldBe List(
        models.output.esc.ESCTaxYear(
          from = periodStart,
          until = periodEnd,
          periods = List(
            ESCPeriod(
              from = periodStart,
              until = periodEnd,
              claimants = List(
                outputClaimant1
              ),
              children = List(
                outputChild1,
                outputChild2,
                outputChild3
              )
            )
          )
        )
      )
    }

    "(Single tax year)(No splits - child born after tax year) determine tax years with periods" in {
      val dateOfBirth1 = LocalDate.parse("2004-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2005-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-08-19", formatter)

      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildChild(dob = dateOfBirth1, disabled = true)
      val child2 = buildChild(dob = dateOfBirth2)
      val child3 = buildChild(dob = dateOfBirth3)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val result = service.constructTaxYearsWithPeriods(List(taxYear))

      val outputChild1 = buildOutputChild(
        qualifying = true
      )
      val outputChild2 = buildOutputChild(
        qualifying = true
      )

      val outputChild3 = buildOutputChild(
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 10,
        vouchers = true
      )

      result shouldBe List(
        models.output.esc.ESCTaxYear(
          from = periodStart,
          until = periodEnd,
          periods = List(
            ESCPeriod(
              from = periodStart,
              until = periodEnd,
              claimants = List(
                outputClaimant1
              ),
              children = List(
                outputChild1,
                outputChild2,
                outputChild3
              )
            )
          )
        )
      )
    }

    "(Single tax year)(No splits - children too old) determine tax years with periods" in {
      val dateOfBirth1 = LocalDate.parse("1992-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("1992-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("1992-08-19", formatter)

      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildChild(dob = dateOfBirth1, disabled = true)
      val child2 = buildChild(dob = dateOfBirth2)
      val child3 = buildChild(dob = dateOfBirth3)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)

      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val result = service.constructTaxYearsWithPeriods(List(taxYear))

      val outputChild1 = buildOutputChild(
        qualifying = false
      )
      val outputChild2 = buildOutputChild(
        qualifying = false
      )

      val outputChild3 = buildOutputChild(
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 0,
        vouchers = false
      )

      result shouldBe List(
        models.output.esc.ESCTaxYear(
          from = periodStart,
          until = periodEnd,
          periods = List(
            ESCPeriod(
              from = periodStart,
              until = periodEnd,
              claimants = List(
                outputClaimant1
              ),
              children = List(
                outputChild1,
                outputChild2,
                outputChild3
              )
            )
          )
        )
      )
    }

    "(Single tax year)(split - child being born when other child is eligible) determine tax years with periods" in {
      val dateOfBirth1 = LocalDate.parse("2016-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2005-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2006-08-19", formatter)

      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildChild(dob = dateOfBirth1, disabled = true)
      val child2 = buildChild(dob = dateOfBirth2)
      val child3 = buildChild(dob = dateOfBirth3)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)

      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val result = service.constructTaxYearsWithPeriods(List(taxYear))

      result shouldBe List(
        models.output.esc.ESCTaxYear(
          from = periodStart,
          until = periodEnd,
          periods = List(
            ESCPeriod(
              from = periodStart,
              until = dateOfBirth1,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 0,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = false),
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true)
              )
            ),
            ESCPeriod(
              from = dateOfBirth1,
              until = periodEnd,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 10,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true)
              )
            )
          )
        )
      )
    }

    "(Single tax year)(split - child turning 15) determine tax years with periods" in {
      val dateOfBirth1 = LocalDate.parse("2001-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2005-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2006-08-19", formatter)

      val september = LocalDate.parse("2016-09-01", formatter)

      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildChild(dob = dateOfBirth1)
      val child2 = buildChild(dob = dateOfBirth2)
      val child3 = buildChild(dob = dateOfBirth3)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)

      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val result = service.constructTaxYearsWithPeriods(List(taxYear))

      result shouldBe List(
        models.output.esc.ESCTaxYear(
          from = periodStart,
          until = periodEnd,
          periods = List(
            ESCPeriod(
              from = periodStart,
              until = september,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 3,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true)
              )
            ),
            ESCPeriod(
              from = september,
              until = periodEnd,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 7,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = false),
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true)
              )
            )
          )
        )
      )
    }

    "(Single tax year)(split - child turning 16) determine tax years with periods" in {
      val dateOfBirth1 = LocalDate.parse("2000-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2005-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2006-08-19", formatter)

      val september = LocalDate.parse("2016-09-01", formatter)

      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildChild(dob = dateOfBirth1, disabled = true)
      val child2 = buildChild(dob = dateOfBirth2)
      val child3 = buildChild(dob = dateOfBirth3)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)

      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val result = service.constructTaxYearsWithPeriods(List(taxYear))

      result shouldBe List(
        models.output.esc.ESCTaxYear(
          from = periodStart,
          until = periodEnd,
          periods = List(
            ESCPeriod(
              from = periodStart,
              until = september,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 3,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true)
              )
            ),
            ESCPeriod(
              from = september,
              until = periodEnd,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 7,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = false),
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true)
              )
            )
          )
        )
      )
    }

    "(Single tax year)(split - child turning 15, child turning 16, child being born before the other turn 15, 16) determine tax years with periods" in {
      val dateOfBirth1 = LocalDate.parse("2000-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2001-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2007-08-19", formatter)

      val september = LocalDate.parse("2016-09-01", formatter)

      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildChild(dob = dateOfBirth1, disabled = true)
      val child2 = buildChild(dob = dateOfBirth2)
      val child3 = buildChild(dob = dateOfBirth3)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)

      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val result = service.constructTaxYearsWithPeriods(List(taxYear))

      result shouldBe List(
        models.output.esc.ESCTaxYear(
          from = periodStart,
          until = periodEnd,
          periods = List(
            ESCPeriod(
              from = periodStart,
              until = september,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 3,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true)
              )
            ),
            ESCPeriod(
              from = september,
              until = periodEnd,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 7,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = false),
                buildOutputChild(qualifying = false),
                buildOutputChild(qualifying = true)
              )
            )
          )
        )
      )
    }

    "(Single tax year)(no split - child being born before 1st sept, child turning 16) determine tax years with periods" in {
      val dateOfBirth1 = LocalDate.parse("2000-06-27", formatter)
      val dateOfBirth2 = LocalDate.parse("2005-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2016-08-19", formatter)

      val september = LocalDate.parse("2016-09-01", formatter)

      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildChild(dob = dateOfBirth1, disabled = true)
      val child2 = buildChild(dob = dateOfBirth2)
      val child3 = buildChild(dob = dateOfBirth3)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)

      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val result = service.constructTaxYearsWithPeriods(List(taxYear))



      result shouldBe List(
        models.output.esc.ESCTaxYear(
          from = periodStart,
          until = periodEnd,
          periods = List(
            ESCPeriod(
              from = periodStart,
              until = dateOfBirth3,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 2,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = false)
              )
            ),
            ESCPeriod(
              from = dateOfBirth3,
              until = september,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 1,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true)
              )
            ),
            ESCPeriod(
              from = september,
              until = periodEnd,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 7,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = false),
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true)
              )
            )
          )
        )
      )
    }

    "(Single tax year)(1 split - child being born on 1st Sept in the tax year) determine tax years with periods" in {
      val dateOfBirth1 = LocalDate.parse("2016-09-01", formatter)

      val periodStart = LocalDate.parse("2016-06-20", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildChild(dob = dateOfBirth1, disabled = true)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)

      val taxYear = ESCTaxYear(from = periodStart, until = periodEnd, children = List(child1), claimants = List(claimant1))
      val result = service.constructTaxYearsWithPeriods(List(taxYear))

      val outputChild1 = buildOutputChild(
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 0,

        vouchers = false
      )
      val outputChild2 = buildOutputChild(
        qualifying = true
      )

      val outputClaimant2 = models.output.esc.ESCClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 7,
        vouchers = true
      )

      result shouldBe List(
        models.output.esc.ESCTaxYear(
          from = periodStart,
          until = periodEnd,
          periods = List(
            ESCPeriod(
              from = periodStart,
              until = dateOfBirth1,
              claimants = List(
                outputClaimant1
              ),
              children = List(
                outputChild1
              )
            ),
            ESCPeriod(
              from = dateOfBirth1,
              until = periodEnd,
              claimants = List(
                outputClaimant2
              ),
              children = List(
                outputChild2
              )
            )
          )
        )
      )
    }

    "(Multiple tax years)(TY 1 - 1 split child turning 15 non disabled)(TY 2 - 2 splits - children being born after 1st September) determine tax years with periods" in {
      val dateOfBirth1 = LocalDate.parse("2001-06-21", formatter)
      val dateOfBirth2 = LocalDate.parse("2017-10-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-11-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-06-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)
      val ty2periodStart = LocalDate.parse("2017-04-06", formatter)
      val ty2periodEnd = LocalDate.parse("2018-04-06", formatter)

      val september1 = LocalDate.parse("2016-09-01", formatter)

      val child1 = buildChild(dob = dateOfBirth1)
      val child2 = buildChild(dob = dateOfBirth2)
      val child3 = buildChild(dob = dateOfBirth3)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)

      val taxYear1 = ESCTaxYear(from = ty1periodStart, until = ty1periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val taxYear2 = ESCTaxYear(from = ty2periodStart, until = ty2periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val result = service.constructTaxYearsWithPeriods(List(taxYear1, taxYear2))

      result shouldBe List(
        models.output.esc.ESCTaxYear(
          from = ty1periodStart,
          until = ty1periodEnd,
          periods = List(
            ESCPeriod(
              from = ty1periodStart,
              until = september1,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 3,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = false),
                buildOutputChild(qualifying = false)
              )
            ),
            ESCPeriod(
              from = september1,
              until = ty1periodEnd,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 0,
                  vouchers = false
                )
              ),
              children = List(
                buildOutputChild(qualifying = false),
                buildOutputChild(qualifying = false),
                buildOutputChild(qualifying = false)
              )
            )
          )
        ),
        models.output.esc.ESCTaxYear(
          from = ty2periodStart,
          until = ty2periodEnd,
          periods = List(
            ESCPeriod(
              from = ty2periodStart,
              until = dateOfBirth2,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 0,
                  vouchers = false
                )
              ),
              children = List(
                buildOutputChild(qualifying = false),
                buildOutputChild(qualifying = false),
                buildOutputChild(qualifying = false)
              )
            ),
            ESCPeriod(
              from = dateOfBirth2,
              until = dateOfBirth3,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 1,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = false),
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = false)
              )
            ),
            ESCPeriod(
              from = dateOfBirth3,
              until = ty2periodEnd,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 5,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = false),
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true)
              )
            )
          )
        )
      )
    }

    "(Multiple tax years)(TY 1 - No splits)(TY 2 - splits - child being born, disabled child turns than 16) determine tax years with periods" in {
      val dateOfBirth1 = LocalDate.parse("2001-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2014-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-08-19", formatter)

      val september = LocalDate.parse("2017-09-01", formatter)

      val ty1periodStart = LocalDate.parse("2016-06-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)
      val ty2periodStart = LocalDate.parse("2017-04-06", formatter)
      val ty2periodEnd = LocalDate.parse("2018-04-06", formatter)

      val child1 = buildChild(dob = dateOfBirth1, disabled = true)
      val child2 = buildChild(dob = dateOfBirth2)
      val child3 = buildChild(dob = dateOfBirth3)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)

      val taxYear1 = ESCTaxYear(from = ty1periodStart, until = ty1periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val taxYear2 = ESCTaxYear(from = ty2periodStart, until = ty2periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val result = service.constructTaxYearsWithPeriods(List(taxYear1, taxYear2))

      result shouldBe List(
        models.output.esc.ESCTaxYear(
          from = ty1periodStart,
          until = ty1periodEnd,
          periods = List(
            ESCPeriod(
              from = ty1periodStart,
              until = ty1periodEnd,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 10,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = false)
              )
            )
          )
        ),
        models.output.esc.ESCTaxYear(
          from = ty2periodStart,
          until = ty2periodEnd,
          periods = List(
            ESCPeriod(
              from = ty2periodStart,
              until = dateOfBirth3,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 4,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = false)
              )
            ),
            ESCPeriod(
              from = dateOfBirth3,
              until = september,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 1,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true)
              )
            ),
            ESCPeriod(
              from = september,
              until = ty2periodEnd,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 7,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = false),
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true)
              )
            )
          )
        )
      )
    }

    "(Multiple tax years)(TY 1 - No splits)(TY 2 - splits - child being born) determine tax years with periods" in {
      val dateOfBirth1 = LocalDate.parse("2002-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2014-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-08-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-06-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)
      val ty2periodStart = LocalDate.parse("2017-04-06", formatter)
      val ty2periodEnd = LocalDate.parse("2018-04-06", formatter)

      val child1 = buildChild(dob = dateOfBirth1, disabled = true)
      val child2 = buildChild(dob = dateOfBirth2)
      val child3 = buildChild(dob = dateOfBirth3)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)

      val taxYear1 = ESCTaxYear(from = ty1periodStart, until = ty1periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))
      val taxYear2 = ESCTaxYear(from = ty2periodStart, until = ty2periodEnd, children = List(child1, child2, child3), claimants = List(claimant1))

      val result = service.constructTaxYearsWithPeriods(List(taxYear1, taxYear2))
      result shouldBe List(
        models.output.esc.ESCTaxYear(
          from = ty1periodStart,
          until = ty1periodEnd,
          periods = List(
            ESCPeriod(
              from = ty1periodStart,
              until = ty1periodEnd,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 10,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = false)
              )
            )
          )
        ),
        models.output.esc.ESCTaxYear(
          from = ty2periodStart,
          until = ty2periodEnd,
          periods = List(
            ESCPeriod(
              from = ty2periodStart,
              until = dateOfBirth3,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 4,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = false)
              )
            ),
            ESCPeriod(
              from = dateOfBirth3,
              until = ty2periodEnd,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 8,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true)
              )
            )
          )
        )
      )
    }

    "(Multiple tax years)(TY 1 - no split)(TY 2 - 1 split - child turning 16(disabled), child being born) determine tax years with periods" in {
      val dateOfBirth1 = LocalDate.parse("2001-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2017-09-01", formatter)

      val ty1periodStart = LocalDate.parse("2016-06-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)
      val ty2periodStart = LocalDate.parse("2017-04-06", formatter)
      val ty2periodEnd = LocalDate.parse("2018-04-06", formatter)

      val september = LocalDate.parse("2017-09-01", formatter)

      val child1 = buildChild(dob = dateOfBirth1, disabled = true)
      val child2 = buildChild(dob = dateOfBirth2)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)

      val taxYear1 = ESCTaxYear(from = ty1periodStart, until = ty1periodEnd, children = List(child1, child2), claimants = List(claimant1))
      val taxYear2 = ESCTaxYear(from = ty2periodStart, until = ty2periodEnd, children = List(child1, child2), claimants = List(claimant1))

      val outputChild1 = buildOutputChild(
        qualifying = true
      )
      val outputChild2 = buildOutputChild(
        qualifying = false
      )

      val outputClaimant1 = models.output.esc.ESCClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 10,
        vouchers = true
      )

      val outputChild3 = buildOutputChild(
        qualifying = true
      )

      val outputChild4 = buildOutputChild(
        qualifying = false
      )

      val outputClaimant2 = models.output.esc.ESCClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 5,
        vouchers = true
      )


      val outputChild5 = buildOutputChild(
        qualifying = false
      )
      val outputChild6 = buildOutputChild(
        qualifying = true
      )

      val outputClaimant3 = models.output.esc.ESCClaimant(
        qualifying = true,
        isPartner = false,
        eligibleMonthsInPeriod = 7,
        vouchers = true
      )

      val result = service.constructTaxYearsWithPeriods(List(taxYear1, taxYear2))
      result shouldBe List(
        models.output.esc.ESCTaxYear(
          from = ty1periodStart,
          until = ty1periodEnd,
          periods = List(
            ESCPeriod(
              from = ty1periodStart,
              until = ty1periodEnd,
              claimants = List(
                outputClaimant1
              ),
              children = List(
                outputChild1,
                outputChild2
              )
            )
          )
        ),
        models.output.esc.ESCTaxYear(
          from = ty2periodStart,
          until = ty2periodEnd,
          periods = List(
            ESCPeriod(
              from = ty2periodStart,
              until = september,
              claimants = List(
                outputClaimant2
              ),
              children = List(
                outputChild3,
                outputChild4
              )
            ),
            ESCPeriod(
              from = september,
              until = ty2periodEnd,
              claimants = List(
                outputClaimant3
              ),
              children = List(
                outputChild5,
                outputChild6
              )
            )
          )
        )
      )
    }

    "(Multiple tax years)(TY 1 - split - child being born)(TY 2 - splits - child less than 15, child being born) determine tax years with periods" in {
      val dateOfBirth1 = LocalDate.parse("2002-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2017-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2016-08-19", formatter)

      val ty1Start = LocalDate.parse("2016-06-20", formatter)
      val ty1End = LocalDate.parse("2017-04-06", formatter)
      val ty2Start = LocalDate.parse("2017-04-06", formatter)
      val ty2End = LocalDate.parse("2018-04-06", formatter)

      val child1 = buildChild(dob = dateOfBirth1, disabled = true)
      val child2 = buildChild(dob = dateOfBirth2)
      val child3 = buildChild(dob = dateOfBirth3)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)

      val taxYear1 = ESCTaxYear(from = ty1Start, until = ty1End, children = List(child1, child2, child3), claimants = List(claimant1))
      val taxYear2 = ESCTaxYear(from = ty2Start, until = ty2End, children = List(child1, child2, child3), claimants = List(claimant1))

      val result = service.constructTaxYearsWithPeriods(List(taxYear1, taxYear2))
      result shouldBe List(
        models.output.esc.ESCTaxYear(
          from = ty1Start,
          until = ty1End,
          periods = List(
            ESCPeriod(
              from = ty1Start,
              until = dateOfBirth3,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 2,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = false),
                buildOutputChild(qualifying = false)
              )
            ),
            ESCPeriod(
              from = dateOfBirth3,
              until = ty1End,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 8,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = false),
                buildOutputChild(qualifying = true)
              )
            )
          )
        ),
        models.output.esc.ESCTaxYear(
          from = ty2Start,
          until = ty2End,
          periods = List(
            ESCPeriod(
              from = ty2Start,
              until = dateOfBirth2,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 2,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = false),
                buildOutputChild(qualifying = true)
              )
            ),
            ESCPeriod(
              from = dateOfBirth2,
              until = ty2End,
              claimants = List(
                models.output.esc.ESCClaimant(
                  qualifying = true,
                  isPartner = false,
                  eligibleMonthsInPeriod = 10,
                  vouchers = true
                )
              ),
              children = List(
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true),
                buildOutputChild(qualifying = true)
              )
            )
          )
        )
      )
    }

    "determine children's eligibility for a period 1" in {
      val dateOfBirth1 = LocalDate.parse("2000-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2017-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2016-08-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-06-20", formatter)

      val child1 = buildChild(dob = dateOfBirth1, disabled = true)
      val child2 = buildChild(dob = dateOfBirth2)
      val child3 = buildChild(dob = dateOfBirth3)

      val result = service.determineChildrensEligibilityForPeriod(List(child1,child2,child3),ty1periodStart)
      result shouldBe List(
        buildOutputChild(qualifying = true),
        buildOutputChild(qualifying = false),
        buildOutputChild(qualifying = false)
      )
    }

    "determine children's eligibility for a period 2" in {
      val dateOfBirth1 = LocalDate.parse("2000-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2017-06-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2016-08-19", formatter)
      val ty1periodStart = LocalDate.parse("2016-09-20", formatter)

      val child1 = buildChild(dob = dateOfBirth1, disabled = true)
      val child2 = buildChild(dob = dateOfBirth2)
      val child3 = buildChild(dob = dateOfBirth3)

      val result = service.determineChildrensEligibilityForPeriod(List(child1,child2,child3),ty1periodStart)
      result shouldBe List(
        buildOutputChild(qualifying = false),
        buildOutputChild(qualifying = false),
        buildOutputChild(qualifying = true)
      )
    }


    "determine children's eligibility for a period 3" in {
      val dateOfBirth1 = LocalDate.parse("1998-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2016-10-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2016-08-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-09-20", formatter)

      val child1 = buildChild(dob = dateOfBirth1, disabled = true)
      val child2 = buildChild(dob = dateOfBirth2)
      val child3 = buildChild(dob = dateOfBirth3)

      val result = service.determineChildrensEligibilityForPeriod(List(child1,child2,child3),ty1periodStart)
      result shouldBe List(
        buildOutputChild(qualifying = false),
        buildOutputChild(qualifying = false),
        buildOutputChild(qualifying = true)
      )
    }


    "determine children's eligibility for a period 4" in {
      val dateOfBirth1 = LocalDate.parse("1998-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2016-10-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2016-08-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)

      val child1 = buildChild(dob = dateOfBirth1, disabled = true)
      val child2 = buildChild(dob = dateOfBirth2)
      val child3 = buildChild(dob = dateOfBirth3)

      val result = service.determineChildrensEligibilityForPeriod(List(child1,child2,child3),ty1periodStart)
      result shouldBe List(
        buildOutputChild(qualifying = false),
        buildOutputChild(qualifying = true),
        buildOutputChild(qualifying = true)
      )
    }

    "determine children's eligibility for a period 5" in {
      val dateOfBirth1 = LocalDate.parse("1998-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2017-12-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-01-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)

      val child1 = buildChild(dob = dateOfBirth1, disabled = true)
      val child2 = buildChild(dob = dateOfBirth2)
      val child3 = buildChild(dob = dateOfBirth3)

      val result = service.determineChildrensEligibilityForPeriod(List(child1,child2,child3),ty1periodStart)
      result shouldBe List(
        buildOutputChild(qualifying = false),
        buildOutputChild(qualifying = false),
        buildOutputChild(qualifying = false)
      )
    }

    "determine children's eligibility(five children) for a period 6" in {
      val dateOfBirth1 = LocalDate.parse("1998-06-18", formatter)
      val dateOfBirth2 = LocalDate.parse("2001-12-27", formatter)
      val dateOfBirth3 = LocalDate.parse("2017-01-19", formatter)
      val dateOfBirth4 = LocalDate.parse("2000-01-19", formatter)
      val dateOfBirth5 = LocalDate.parse("2018-01-19", formatter)

      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)

      val child1 = buildChild(dob = dateOfBirth1, disabled = true)
      val child2 = buildChild(dob = dateOfBirth2)
      val child3 = buildChild(dob = dateOfBirth3)
      val child4 = buildChild(dob = dateOfBirth4)
      val child5 = buildChild(dob = dateOfBirth5)

      val result = service.determineChildrensEligibilityForPeriod(List(child1,child2,child3,child4, child5),ty1periodStart)
      result shouldBe List(
        buildOutputChild(qualifying = false),
        buildOutputChild(qualifying = true),
        buildOutputChild(qualifying = false),
        buildOutputChild(qualifying = false),
        buildOutputChild(qualifying = false)
      )
    }

    "(single claimant)(no children)(employer providing vouchers) determine a claimants eligibility for a period" in {
      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true,
        previousIncome = Some(ESCIncome(Some(20000.0),Some(200.0), Some("1100L"))),
        currentIncome = Some(ESCIncome(Some(20000.0),Some(200.0), Some("1150L"))))

      val result = service.determineClaimantsEligibilityForPeriod(List(), List(claimant1), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCClaimant(
          qualifying = true,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          previousIncome = Some(models.output.esc.ESCIncome(Some(20000.0),Some(200.0), Some("1100L"))),
          currentIncome = Some(models.output.esc.ESCIncome(Some(20000.0),Some(200.0), Some("1150L"))),
          vouchers = false
        )
      )
    }

    "(multiple claimants)(no children)(employer providing vouchers) determine a claimants eligibility for a period" in {
      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val claimant2 = ESCClaimant(isPartner = true, employerProvidesESC = true)

      val result = service.determineClaimantsEligibilityForPeriod(List(), List(claimant1, claimant2), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCClaimant(
          qualifying = true,
          isPartner = false,
          eligibleMonthsInPeriod = 0,vouchers = false
        ),
        models.output.esc.ESCClaimant(
          qualifying = true,
          isPartner = true,
          eligibleMonthsInPeriod = 0,vouchers = false
        )
      )
    }

    "(multiple claimants)(no children)(employer providing vouchers for 1 claimant) determine a claimants eligibility for a period" in {
      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = false)
      val claimant2 = ESCClaimant(isPartner = true, employerProvidesESC = true)

      val result = service.determineClaimantsEligibilityForPeriod(List(), List(claimant1, claimant2), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCClaimant(
          qualifying = false,
          isPartner = false,
          eligibleMonthsInPeriod = 0,vouchers = false
        ),
        models.output.esc.ESCClaimant(
          qualifying = true,
          isPartner = true,
          eligibleMonthsInPeriod = 0,vouchers = false
        )
      )
    }

    "(single claimant)(no children)(employer not providing vouchers) determine a claimants eligibility for a period" in {
      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = false)

      val result = service.determineClaimantsEligibilityForPeriod(List(), List(claimant1), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCClaimant(
          qualifying = false,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        )
      )
    }

    "(multiple claimants)(no children)(employer not providing vouchers) determine a claimants eligibility for a period" in {
      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = false)
      val claimant2 = ESCClaimant(isPartner = true, employerProvidesESC = false)

      val result = service.determineClaimantsEligibilityForPeriod(List(), List(claimant1, claimant2), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCClaimant(
          qualifying = false,
          isPartner = false,
          eligibleMonthsInPeriod = 0,vouchers = false
        ),
        models.output.esc.ESCClaimant(
          qualifying = false,
          isPartner = true,
          eligibleMonthsInPeriod = 0,vouchers = false
        )
      )
    }

    "(single claimant)(qualifying child)(employer providing vouchers) determine a claimants eligibility for a period" in {
      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildOutputChild(qualifying = false)
      val child2 = buildOutputChild(qualifying = true)
      val child3 = buildOutputChild(qualifying = true)
      val child4 = buildOutputChild(qualifying = true)
      val child5 = buildOutputChild(qualifying = false)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)

      val result = service.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCClaimant(
          qualifying = true,
          isPartner = false,
          eligibleMonthsInPeriod = 5,
          vouchers = true
        )
      )
    }

    "(multiple claimants, one qualifying claimant)(qualifying child)(employer providing vouchers) determine a claimants eligibility for a period" in {
      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildOutputChild(qualifying = false)
      val child2 = buildOutputChild(qualifying = true)
      val child3 = buildOutputChild(qualifying = true)
      val child4 = buildOutputChild(qualifying = true)
      val child5 = buildOutputChild(qualifying = false)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = false)
      val claimant2 = ESCClaimant(isPartner = true, employerProvidesESC = true)

      val result = service.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1, claimant2), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCClaimant(
          qualifying = false,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        ),
        models.output.esc.ESCClaimant(
          qualifying = true,
          isPartner = true,
          eligibleMonthsInPeriod = 5,
          vouchers = true
        )
      )
    }

    "(multiple claimants)(qualifying child)(employer providing vouchers for 1 claimant) determine a claimants eligibility for a period" in {
      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildOutputChild(qualifying = false)
      val child2 = buildOutputChild(qualifying = true)
      val child3 = buildOutputChild(qualifying = true)
      val child4 = buildOutputChild(qualifying = true)
      val child5 = buildOutputChild(qualifying = false)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val claimant2 = ESCClaimant(isPartner = true, employerProvidesESC = false)

      val result = service.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1, claimant2), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCClaimant(
          qualifying = true,
          isPartner = false,
          eligibleMonthsInPeriod = 5,
          vouchers = true
        ),
        models.output.esc.ESCClaimant(
          qualifying = false,
          isPartner = true,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        )
      )
    }

    "(single claimant)(qualifying child)(employer not providing vouchers) determine a claimants eligibility for a period" in {
      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildOutputChild(qualifying = false)
      val child2 = buildOutputChild(qualifying = true)
      val child3 = buildOutputChild(qualifying = true)
      val child4 = buildOutputChild(qualifying = true)
      val child5 = buildOutputChild(qualifying = false)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = false)

      val result = service.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCClaimant(
          qualifying = false,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        )
      )
    }

    "(multiple claimants)(qualifying child)(employer not providing vouchers) determine a claimants eligibility for a period" in {
      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildOutputChild(qualifying = false)
      val child2 = buildOutputChild(qualifying = true)
      val child3 = buildOutputChild(qualifying = true)
      val child4 = buildOutputChild(qualifying = true)
      val child5 = buildOutputChild(qualifying = false)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = false)
      val claimant2 = ESCClaimant(isPartner = true, employerProvidesESC = false)

      val result = service.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1, claimant2), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCClaimant(
          qualifying = false,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        ),
        models.output.esc.ESCClaimant(
          qualifying = false,
          isPartner = true,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        )
      )
    }

    "(single claimant)(no qualifying children)(employer providing vouchers) determine a claimants eligibility for a period" in {
      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildOutputChild(qualifying = false)
      val child2 = buildOutputChild(qualifying = false)
      val child3 = buildOutputChild(qualifying = false)
      val child4 = buildOutputChild(qualifying = false)
      val child5 = buildOutputChild(qualifying = false)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)

      val result = service.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCClaimant(
          qualifying = true,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        )
      )
    }

    "(multiple claimants)(no qualifying children)(employer providing vouchers) determine a claimants eligibility for a period" in {
      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildOutputChild(qualifying = false)
      val child2 = buildOutputChild(qualifying = false)
      val child3 = buildOutputChild(qualifying = false)
      val child4 = buildOutputChild(qualifying = false)
      val child5 = buildOutputChild(qualifying = false)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val claimant2 = ESCClaimant(isPartner = true, employerProvidesESC = true)

      val result = service.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1, claimant2), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCClaimant(
          qualifying = true,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        ),
        models.output.esc.ESCClaimant(
          qualifying = true,
          isPartner = true,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        )
      )
    }

    "(multiple claimants)(no qualifying children)(employer providing vouchers for 1 claimant) determine a claimants eligibility for a period" in {
      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildOutputChild(qualifying = false)
      val child2 = buildOutputChild(qualifying = false)
      val child3 = buildOutputChild(qualifying = false)
      val child4 = buildOutputChild(qualifying = false)
      val child5 = buildOutputChild(qualifying = false)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = true)
      val claimant2 = ESCClaimant(isPartner = true, employerProvidesESC = false)

      val result = service.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1, claimant2), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCClaimant(
          qualifying = true,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        ),
        models.output.esc.ESCClaimant(
          qualifying = false,
          isPartner = true,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        )
      )
    }

    "(single claimant)(no qualifying children)(employer not providing vouchers) determine a claimants eligibility for a period" in {
      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildOutputChild(qualifying = false)
      val child2 = buildOutputChild(qualifying = false)
      val child3 = buildOutputChild(qualifying = false)
      val child4 = buildOutputChild(qualifying = false)
      val child5 = buildOutputChild(qualifying = false)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = false)

      val result = service.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCClaimant(
          qualifying = false,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        )
      )
    }

    "(multiple claimants)(no qualifying children)(employer not providing vouchers) determine a claimants eligibility for a period" in {
      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildOutputChild(qualifying = false)
      val child2 = buildOutputChild(qualifying = false)
      val child3 = buildOutputChild(qualifying = false)
      val child4 = buildOutputChild(qualifying = false)
      val child5 = buildOutputChild(qualifying = false)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = false)
      val claimant2 = ESCClaimant(isPartner = true, employerProvidesESC = false)

      val result = service.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1, claimant2), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCClaimant(
          qualifying = false,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        ),
        models.output.esc.ESCClaimant(
          qualifying = false,
          isPartner = true,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        )
      )
    }

    "(single claimant)(1 qualifying child)(employer providing vouchers, claimant not receiving vouchers) determine a claimants eligibility for a period" in {
      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildOutputChild(qualifying = false)
      val child2 = buildOutputChild(qualifying = true)
      val child3 = buildOutputChild(qualifying = false)
      val child4 = buildOutputChild(qualifying = false)
      val child5 = buildOutputChild(qualifying = false)

      val claimant1 = ESCClaimant(isPartner = false, employerProvidesESC = false)

      val result = service.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCClaimant(
          qualifying = false,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        )
      )
    }

    "(single claimant)(1 qualifying child)(claimant does not work in UK) determine a claimants eligibility for a period" in {
      val ty1periodStart = LocalDate.parse("2016-11-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val child1 = buildOutputChild(qualifying = false)
      val child2 = buildOutputChild(qualifying = true)
      val child3 = buildOutputChild(qualifying = false)
      val child4 = buildOutputChild(qualifying = false)
      val child5 = buildOutputChild(qualifying = false)

      val claimant1 = ESCClaimant(isPartner = false)

      val result = service.determineClaimantsEligibilityForPeriod(List(child1,child2,child3,child4,child5), List(claimant1), ty1periodStart, ty1periodEnd)
      result shouldBe List(
        models.output.esc.ESCClaimant(
          qualifying = false,
          isPartner = false,
          eligibleMonthsInPeriod = 0,
          vouchers = false
        )
      )
    }

    "(Single tax year)(multiple claimants, no qualifying children) determine the number of qualifying months in a period" in {
      val ty1periodStart = LocalDate.parse("2016-06-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val result = service.numberOfQualifyingMonthsForPeriod(qualifying = false, ty1periodStart, ty1periodEnd)
      result shouldBe 0
    }

    "(Single tax year)(multiple claimants, qualifying children) determine the number of qualifying months in a period" in {
      val ty1periodStart = LocalDate.parse("2016-06-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val result = service.numberOfQualifyingMonthsForPeriod(qualifying = true, ty1periodStart, ty1periodEnd)
      result shouldBe 10
    }

    "(Multiple tax year)(multiple claimants, qualifying children) determine the number of qualifying months in a period" in {
      val ty1periodStart = LocalDate.parse("2015-06-20", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val result = service.numberOfQualifyingMonthsForPeriod(qualifying = true, ty1periodStart, ty1periodEnd)
      result shouldBe 22
    }

    "(Single tax year) calculate qualifying months period start 1st of the month" in {
      val ty1periodStart = LocalDate.parse("2016-04-01", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val result = service.numberOfQualifyingMonthsForPeriod(qualifying = true, ty1periodStart, ty1periodEnd)
      result shouldBe 12
    }

    "calculate qualifying months period start in middle of the month" in {
      val ty1periodStart = LocalDate.parse("2016-04-15", formatter)
      val ty1periodEnd = LocalDate.parse("2016-09-01", formatter)

      val result = service.numberOfQualifyingMonthsForPeriod(qualifying = true, ty1periodStart, ty1periodEnd)
      result shouldBe 5
    }

    "calculate qualifying months period start in middle of the month, period end is 6th april" in {
      val ty1periodStart = LocalDate.parse("2016-06-15", formatter)
      val ty1periodEnd = LocalDate.parse("2017-04-06", formatter)

      val result = service.numberOfQualifyingMonthsForPeriod(qualifying = true, ty1periodStart, ty1periodEnd)
      result shouldBe 10
    }

    "calculate qualifying months period start in middle of the month period end end of the month" in {
      val ty1periodStart = LocalDate.parse("2017-04-06", formatter)
      val ty1periodEnd = LocalDate.parse("2017-06-15", formatter)

      val result = service.numberOfQualifyingMonthsForPeriod(qualifying = true, ty1periodStart, ty1periodEnd)
      result shouldBe 2
    }

  }
}
