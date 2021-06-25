/*
 * Copyright 2021 HM Revenue & Customs
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
import models.input.tfc._
import models.output.tfc.{TFCEligibilityOutput, TFCOutputChild, TFCOutputClaimant, TFCPeriod}
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.scalatest.PrivateMethodTester
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import service.AuditEvents
import utils.{Periods, TFCConfig}

import scala.concurrent.Future

class TFCEligibilitySpec extends FakeCCEligibilityApplication with PrivateMethodTester with Matchers {

  implicit val req: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

  lazy val tfcEligibility: TFCEligibility = app.injector.instanceOf[TFCEligibility]

  "TFCEligibility" must {

    "determine childs Birthday(11th or 16th) where child is not disable" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-08-27", formatter)
      val current = LocalDate.parse("2017-08-01", formatter)
      val child = new TFCChild(
        id = 0,
        childcareCost = BigDecimal(300.00),
        childcareCostPeriod = Periods.Monthly,
        dob = dateOfBirth,
        disability = TFCDisability())(None)
      val child11Birthday = tfcEligibility.getChildBirthday(current, "england", child)
      LocalDate.fromDateFields(child11Birthday) shouldBe LocalDate.parse("2016-08-27", formatter)
    }

    "determine childs Birthday (11th or 16th) where child is disabled" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2000-09-27", formatter)
      val current = LocalDate.parse("2017-08-01", formatter)
      val child = new TFCChild(
        id = 0,
        childcareCost = BigDecimal(300.00),
        childcareCostPeriod = Periods.Monthly,
        dob = dateOfBirth,
        disability = TFCDisability(disabled = true))(None)
      val child11Birthday = tfcEligibility.getChildBirthday(current, "england", child)
      LocalDate.fromDateFields(child11Birthday) shouldBe LocalDate.parse("2016-09-27", formatter)
    }

    "determine end of Week following 1st September Date where child is not disabled" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-08-27", formatter)
      val current = LocalDate.parse("2017-08-01", formatter)
      val child = new TFCChild(
        id = 0,
        childcareCost = BigDecimal(300.00),
        childcareCostPeriod = Periods.Monthly,
        dob = dateOfBirth,
        disability = TFCDisability())(None)
      val endWeek1stOfSeptemberDate = tfcEligibility.endWeek1stOfSeptemberDate(current, "england", child)
      LocalDate.fromDateFields(endWeek1stOfSeptemberDate) shouldBe LocalDate.parse("2016-09-04", formatter)
    }

    "determine end of Week following 1st September Date where child is not disabled, dob after 1st Sept" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-09-12", formatter)
      val current = LocalDate.parse("2017-08-01", formatter)
      val child = new TFCChild(
        id = 0,
        childcareCost = BigDecimal(300.00),
        childcareCostPeriod = Periods.Monthly,
        dob = dateOfBirth,
        disability = TFCDisability())(None)
      val endWeek1stOfSeptemberDate = tfcEligibility.endWeek1stOfSeptemberDate(current, "england", child)
      LocalDate.fromDateFields(endWeek1stOfSeptemberDate) shouldBe LocalDate.parse("2017-09-03", formatter)
    }

    "determine end of Week following 1st September Date where child is disabled" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-08-27", formatter)
      val current = LocalDate.parse("2017-08-01", formatter)
      val child = new TFCChild(
        id = 0,
        childcareCost = BigDecimal(300.00),
        childcareCostPeriod = Periods.Monthly,
        dob = dateOfBirth,
        disability = TFCDisability(disabled = true))(None)
      val endWeek1stOfSeptemberDate = tfcEligibility.endWeek1stOfSeptemberDate(current, "england", child)
      LocalDate.fromDateFields(endWeek1stOfSeptemberDate) shouldBe LocalDate.parse("2021-09-05", formatter)
    }

    "determine end of Week following 1st September Date where child is disabled, dob after 1st Sept" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-09-12", formatter)
      val current = LocalDate.parse("2017-08-01", formatter)
      val child = new TFCChild(
        id = 0,
        childcareCost = BigDecimal(300.00),
        childcareCostPeriod = Periods.Monthly,
        dob = dateOfBirth,
        disability = TFCDisability(disabled = true))(None)
      val endWeek1stOfSeptemberDate = tfcEligibility.endWeek1stOfSeptemberDate(current, "england", child)
      LocalDate.fromDateFields(endWeek1stOfSeptemberDate) shouldBe LocalDate.parse("2022-09-04", formatter)
    }

    "satisfyMinimumEarnings remaining checks" must {
      val fromDate = LocalDate.parse("2000-08-27", formatter)
      "with default values" in {

        val minimumEarnings = TFCMinimumEarnings(selection = false)
        val claimant = testClaimant(disability = TFCDisability(), carersAllowance = true, minimumEarnings =
          minimumEarnings, age = None)

        val partner = testClaimant(hoursPerWeek = 16.50, isPartner = true, disability = TFCDisability(), minimumEarnings =
          TFCMinimumEarnings(), age = None)

        val tfc = new TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List()){
           lazy val auditEvents: AuditEvents = mock[AuditEvents]
        }
        tfcEligibility.satisfyMinimumEarnings(fromDate, "england", tfc.claimants.head)(hc) shouldBe false
        tfcEligibility.satisfyMinimumEarnings(fromDate, "england", tfc.claimants.last)(hc) shouldBe true
      }

      "with undefined ages, parent - TFCMinimumEarnings(selection = false) selfemployed, partner - apprentice" in {

        val minimumEarnings = TFCMinimumEarnings(selection = false)
        val claimant = testClaimant(disability = TFCDisability(), carersAllowance = true,
          minimumEarnings = minimumEarnings,
          age = None,
          selfEmployedSelection = Some(true),
          employmentStatus = Some("selfEmployed"))

        val minimumEarnings1 = TFCMinimumEarnings(selection = false)
        val partner = testClaimant(hoursPerWeek = 16.50, isPartner = true, disability = TFCDisability(),
          minimumEarnings = minimumEarnings1,
          age = None,
          selfEmployedSelection = Some(false),
          employmentStatus = Some("apprentice"))

        val tfc = new TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List()){
           lazy val auditEvents: AuditEvents = mock[AuditEvents]
        }
        tfcEligibility.satisfyMinimumEarnings(tfc.from, "england", tfc.claimants.head)(hc) shouldBe true
        tfcEligibility.satisfyMinimumEarnings(tfc.from, "england", tfc.claimants.last)(hc) shouldBe false
      }

      "with an 18-20 old, selfemployed and partner under 18, TFCMinimumEarnings(selection = false, amount = 0.0), apprentice" in {

        val claimant = testClaimant(disability = TFCDisability(),
          minimumEarnings = TFCMinimumEarnings(),
          age = Some("18-20"),
          selfEmployedSelection = Some(true),
          employmentStatus = Some("selfEmployed")
        )

        val minimumEarnings1 = TFCMinimumEarnings(selection = false, amount = 70.0)
        val partner = testClaimant(hoursPerWeek = 28.00, isPartner = true, disability = TFCDisability(),
          minimumEarnings = minimumEarnings1,
          age = Some("under-18"),
          selfEmployedSelection = Some(false),
          employmentStatus = Some("apprentice")
        )

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())
       tfcEligibility.satisfyMinimumEarnings(tfc.from, "england", tfc.claimants.head)(hc) shouldBe true
        tfcEligibility.satisfyMinimumEarnings(tfc.from, "england", tfc.claimants.last)(hc) shouldBe true
      }
    }

    "return a Future[TFCEligibilityOutput] result" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val today = LocalDate.parse("2016-08-27", formatter)
      val tfcEligibilityInput = TFCEligibilityInput(from = today, numberOfPeriods = 3, location = "england", claimants = List(), children = List())
      val result = tfcEligibility.eligibility(tfcEligibilityInput)
      result.isInstanceOf[Future[TFCEligibilityOutput]] shouldBe true
    }

    "determine claimant's eligibility if qualifies all rules" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val current = LocalDate.parse("2017-08-01", formatter)
      val claimant = testClaimant(hoursPerWeek = 3.50, disability = TFCDisability(), minimumEarnings = TFCMinimumEarnings(), age = None)

      val result = tfcEligibility.determineClaimantsEligibility(List(claimant), current, location = "england")
      result shouldBe List(
        TFCOutputClaimant(
          qualifying = true,
          isPartner = false
        )
      )
    }

    "determine claimant's and partner's eligibility if qualifies for all rules" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val current = LocalDate.parse("2017-08-01", formatter)

      val claimant = testClaimant(hoursPerWeek = 16.99,
        disability = TFCDisability(), minimumEarnings = TFCMinimumEarnings(), age = None)
      val partner = testClaimant(hoursPerWeek = 7.99, isPartner = true,
        disability = TFCDisability(), minimumEarnings = TFCMinimumEarnings(), age = None)

      val result = tfcEligibility.determineClaimantsEligibility(List(claimant, partner), current, location = "england")
      result shouldBe List(
        TFCOutputClaimant(
          qualifying = true,
          isPartner = false
        ),
        TFCOutputClaimant(
          qualifying = true,
          isPartner = true
        )
      )
    }

    "determine claimant's eligibility if claimant fails maximum earnings rule" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val claimantIncome = Some(TFCIncome(Some(1199999.0),Some(100.0),Some(100.0)))
      val claimant = testClaimant(currentIncome = claimantIncome,
        hoursPerWeek = 9.50,
        disability = TFCDisability(), minimumEarnings = TFCMinimumEarnings(), age = None)
      val current = LocalDate.parse("2017-08-01", formatter)

      val result = tfcEligibility.determineClaimantsEligibility(List(claimant), current, location = "england")
      result shouldBe List(
        TFCOutputClaimant(
          qualifying = false,
          isPartner = false
        )
      )
    }

    "determine claimant's eligibility if partner fails max earnings rule" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val current = LocalDate.parse("2017-08-01", formatter)
      val claimantIncome = Some(TFCIncome(Some(99999.0),Some(100.0),Some(100.0)))
      val claimant2Income = Some(TFCIncome(Some(1199999.0),Some(100.0),Some(100.0)))

      val claimant = testClaimant(currentIncome = claimantIncome,
        hoursPerWeek = 5.50,
        disability = TFCDisability(), minimumEarnings = TFCMinimumEarnings(), age = None)
      val claimant2 = testClaimant(currentIncome = claimant2Income,
        hoursPerWeek = 16.50, isPartner = true,
        disability = TFCDisability(), minimumEarnings = TFCMinimumEarnings(), age = None)

      val result = tfcEligibility.determineClaimantsEligibility(List(claimant, claimant2), current, location = "england")
      result shouldBe List(
        TFCOutputClaimant(
          qualifying = true,
          isPartner = false
        ),
        TFCOutputClaimant(
          qualifying = false,
          isPartner = true
        )
      )
    }

    "determine periods(4 periods) based on from (claim date) and until date of TFC for eligible claimant" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2013-01-27", formatter)
      val from = LocalDate.parse("2016-10-15", formatter)
      val claimant = testClaimant(hoursPerWeek = 16.50,
        disability = TFCDisability(), minimumEarnings = TFCMinimumEarnings(), age = None)
      val child = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability())
      val tfc = TFCEligibilityInput(from = from, numberOfPeriods = 4, location = "england", List(claimant), List(child))

      val result = tfcEligibility.determineTFCPeriods(tfc)

      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val startPeriod1 = LocalDate.parse("2016-10-15", formatter)
      val untilPeriod1 = LocalDate.parse("2017-01-15", formatter)
      val startPeriod2 = LocalDate.parse("2017-01-15", formatter)
      val untilPeriod2 = LocalDate.parse("2017-04-15", formatter)
      val startPeriod3 = LocalDate.parse("2017-04-15", formatter)
      val untilPeriod3 = LocalDate.parse("2017-07-15", formatter)
      val startPeriod4 = LocalDate.parse("2017-07-15", formatter)
      val untilPeriod4 = LocalDate.parse("2017-10-15", formatter)


      val outputChild1 = TFCOutputChild(id = 0, qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability())
      val outputChild2 = TFCOutputChild(id = 0, qualifying = true, from = Some(startPeriod2), until = Some(untilPeriod2), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability())
      val outputChild3 = TFCOutputChild(id = 0, qualifying = true, from = Some(startPeriod3), until = Some(untilPeriod3), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability())
      val outputChild4 = TFCOutputChild(id = 0, qualifying = true, from = Some(startPeriod4), until = Some(untilPeriod4), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability())

      Json.toJson(result) shouldBe Json.toJson(List(
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
      ))
    }

    "determine periods(2 periods) based on from (claim date) and until date of TFC for eligible claimant" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-08-27", formatter)
      val from = LocalDate.parse("2016-08-31", formatter)
      val claimant = testClaimant(hoursPerWeek = 16.50,
        disability = TFCDisability(), minimumEarnings = TFCMinimumEarnings(), age = None)
      val child = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability())
      val tfc = TFCEligibilityInput(from = from, numberOfPeriods = 2, location = "england", List(claimant), List(child))
      val result = tfcEligibility.determineTFCPeriods(tfc)

      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val startPeriod1 = LocalDate.parse("2016-08-31", formatter)
      val untilPeriod1 = LocalDate.parse("2016-11-30", formatter)
      val startPeriod2 = LocalDate.parse("2016-11-30", formatter)
      val untilPeriod2 = LocalDate.parse("2017-02-28", formatter)

      val childFrom = LocalDate.parse("2016-08-31", formatter)
      val childUntil = LocalDate.parse("2016-09-04", formatter)

      val outputChild = TFCOutputChild(id = 0, qualifying = true, from = Some(childFrom), until = Some(childUntil),
        tfcRollout = false, BigDecimal(200.0), childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val outputChild2 = TFCOutputChild(id = 0, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.0), childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

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

    "determine periods(1 period) based on from (claim date) and until date of TFC for failed partner eligibility(hours)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2003-08-27", formatter)
      val from = LocalDate.parse("2016-08-27", formatter)
      val claimant = testClaimant(hoursPerWeek = 16.50,
        disability = TFCDisability(), minimumEarnings = TFCMinimumEarnings(), age = None)
      val partner = testClaimant(hoursPerWeek = 5.50, isPartner = true,
        disability = TFCDisability(), minimumEarnings = TFCMinimumEarnings(), age = None)
      val child = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = TFCDisability())
      val tfc = TFCEligibilityInput(from = from, numberOfPeriods = 1, location = "england", List(claimant, partner), List(child))
      val result = tfcEligibility.determineTFCPeriods(tfc)

      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val outputPartner = TFCOutputClaimant(qualifying = true, isPartner = true)
      val startPeriod1 = LocalDate.parse("2016-08-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-11-27", formatter)

      val outputChild = TFCOutputChild(id = 0, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.0), childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false))

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          periodEligibility = false,
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
      val child = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability())
      val tfcPeriod = TFCPeriod(from = from, until = until, periodEligibility = false, claimants = List(), children = List())
      val result = tfcEligibility.determineChildStartDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until, "england")
      result shouldBe Some(dateOfBirth)
    }

    "determine child's start date in a TFC period where child is yet to be born (in current period)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2016-08-27", formatter)
      val from = LocalDate.parse("2016-08-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability())
      val tfcPeriod = TFCPeriod(from = from, until = until, periodEligibility = false, claimants = List(), children = List())
      val result = tfcEligibility.determineChildStartDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until, "england")
      result shouldBe Some(dateOfBirth)
    }

    "determine child's start date in a TFC period where child is yet to be born (after current period)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2016-12-27", formatter)
      val from = LocalDate.parse("2016-08-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability())
      val tfcPeriod = TFCPeriod(from = from, until = until, periodEligibility = false, claimants = List(), children = List())
      val result = tfcEligibility.determineChildStartDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until, "england")
      result shouldBe None
    }

    "determine child's start date in a TFC period where child is eligible (in current period)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2013-12-27", formatter)
      val from = LocalDate.parse("2016-08-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability())
      val tfcPeriod = TFCPeriod(from = from, until = until, periodEligibility = false, claimants = List(), children = List())
      val result = tfcEligibility.determineChildStartDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until, "england")
      result shouldBe Some(from)
    }

    "determine child's start date in a TFC period where child 11th birthday is in current period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-12-27", formatter)
      val from = LocalDate.parse("2016-08-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability())
      val tfcPeriod = TFCPeriod(from = from, until = until, periodEligibility = false, claimants = List(), children = List())
      val result = tfcEligibility.determineChildStartDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until, "england")
      result shouldBe Some(from)
    }

    "determine child's start date in a TFC period where child 11th birthday sept date is before current period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2003-08-27", formatter)
      val from = LocalDate.parse("2016-08-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability())
      val tfcPeriod = TFCPeriod(from = from, until = until, periodEligibility = false, claimants = List(), children = List())
      val result = tfcEligibility.determineChildStartDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until, "england")
      result shouldBe None
    }

    "determine child's start date in a TFC period where child 11th birthday sept date is after current period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-08-27", formatter)
      val from = LocalDate.parse("2016-04-01", formatter)
      val until = LocalDate.parse("2016-07-01", formatter)
      val child = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability())
      val tfcPeriod = TFCPeriod(from = from, until = until, periodEligibility = false, claimants = List(), children = List())
      val result = tfcEligibility.determineChildStartDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until, "england")
      result shouldBe Some(from)
    }

    "determine child's start date in a TFC period where child 11th birthday sept date is within current period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-04-27", formatter)
      val from = LocalDate.parse("2016-08-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability())
      val tfcPeriod = TFCPeriod(from = from, until = until, periodEligibility = false, claimants = List(), children = List())
      val result = tfcEligibility.determineChildStartDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until, "england")
      result shouldBe Some(from)
    }


    "determine child's start date in a TFC period where child(disabled) 16th birthday sept date is within current period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("1999-09-27", formatter)
      val from = LocalDate.parse("2016-09-06", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability(disabled = true))
      val tfcPeriod = TFCPeriod(from = from, until = until, periodEligibility = false, claimants = List(), children = List())
      val childSeptDate = tfcEligibility.endWeek1stOfSeptemberDate(from, "england", child)
      LocalDate.fromDateFields(childSeptDate) shouldBe LocalDate.parse("2016-09-04", formatter)
      val result = tfcEligibility.determineChildStartDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until, "england")
      result shouldBe None
    }

    "determine child's end date in a TFC period where the birthday after period end date" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2016-11-27", formatter)
      val from = LocalDate.parse("2016-09-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability(disabled = false, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = from, until = until, periodEligibility = false, claimants = List(), children = List())
      val result = tfcEligibility.determineChildEndDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until, "england")
      result shouldBe None
    }

    "determine child's end date in a TFC period where the birthday is on period until date" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2016-11-01", formatter)
      val from = LocalDate.parse("2016-09-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability(disabled = false, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = from, until = until, periodEligibility = false, claimants = List(), children = List())
      val result = tfcEligibility.determineChildEndDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until, "england")
      result shouldBe None
    }

    "determine child's end date in a TFC period where the birthday is within the TFC period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2016-10-01", formatter)
      val from = LocalDate.parse("2016-09-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability(disabled = false, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = from, until = until, periodEligibility = false, claimants = List(), children = List())
      val result = tfcEligibility.determineChildEndDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until, "england")
      result shouldBe Some(until)
    }

    "determine child's end date in a TFC period where the birthday is before TFC period start date" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2016-06-01", formatter)
      val from = LocalDate.parse("2016-09-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability(disabled = false, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = from, until = until, periodEligibility = false, claimants = List(), children = List())
      val result = tfcEligibility.determineChildEndDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until, "england")
      result shouldBe Some(until)
    }

    "determine child's end date in a TFC period where the 11th birthday sept date is after TFC period until date" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-06-01", formatter)
      val from = LocalDate.parse("2016-04-01", formatter)
      val until = LocalDate.parse("2016-07-01", formatter)
      val child = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability(disabled = false, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = from, until = until, periodEligibility = false, claimants = List(), children = List())
      val result = tfcEligibility.determineChildEndDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until, "england")
      result shouldBe Some(until)
    }

    "determine child's end date in a TFC period where the 11th birthday sept date is within TFC period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-06-01", formatter)
      val from = LocalDate.parse("2016-08-01", formatter)
      val until = LocalDate.parse("2016-11-01", formatter)
      val child = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability(disabled = false, severelyDisabled = false))
      val childSeptDate = tfcEligibility.endWeek1stOfSeptemberDate(from, "england", child)
      val tfcPeriod = TFCPeriod(from = from, until = until, periodEligibility = false, claimants = List(), children = List())
      val result = tfcEligibility.determineChildEndDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until, "england")
      result shouldBe Some(LocalDate.fromDateFields(childSeptDate))
    }

    "determine child's end date in a TFC period where the 11th birthday sept date is before TFC period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-06-01", formatter)
      val from = LocalDate.parse("2016-10-01", formatter)
      val until = LocalDate.parse("2017-01-01", formatter)
      val child = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability(disabled = false, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = from, until = until, periodEligibility = false, claimants = List(), children = List())
      val result = tfcEligibility.determineChildEndDateInTFCPeriod(child, tfcPeriod.from, tfcPeriod.until, "england")
      result shouldBe None
    }

    "determine periods(2 period) based on from (claim date) and until date of TFC for one eligible child" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2003-08-27", formatter)
      val from = LocalDate.parse("2016-08-27", formatter)
      val claimant = testClaimant(hoursPerWeek = 16.50, isPartner = false,
        disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
      val child = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability(disabled = false, severelyDisabled = false))
      val tfc = TFCEligibilityInput(from = from, 1, location = "england", List(claimant), List(child))
      val result = tfcEligibility.determineTFCPeriods(tfc)

      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val startPeriod1 = LocalDate.parse("2016-08-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-11-27", formatter)
      val outputChild = TFCOutputChild(id = 0, qualifying = false, None, None, false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          periodEligibility = false,
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
      val child = testChild(id = 2, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability(disabled = false, severelyDisabled = false))
      val result = tfcEligibility.determineChildrenEligibility(List(child), from, until, "england")

      result shouldBe List(
        TFCOutputChild(
          id = 2,
          qualifying = false,
          from = None,
          until = None,
          tfcRollout = false,
          BigDecimal(200.00),
          childcareCostPeriod=Periods.Monthly,
          models.output.tfc.TFCDisability(false,false)
        )
      )
    }

    "determine child eligibility for 1 period - for 2 children where 1 child is disabled" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirthChild1 = LocalDate.parse("2003-07-27", formatter)
      val dateOfBirthChild2 = LocalDate.parse("2005-08-21", formatter)
      val from = LocalDate.parse("2016-09-27", formatter)
      val claimant = testClaimant(hoursPerWeek = 16.50, isPartner = false,
        disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
      val child1 = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild1,
        disability = TFCDisability(disabled = true, severelyDisabled = false))
      val child2 = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild2,
        disability = TFCDisability(disabled = false, severelyDisabled = false))
      val tfc = TFCEligibilityInput(from = from, 1, location = "england", List(claimant), List(child1, child2))
      val result = tfcEligibility.determineTFCPeriods(tfc)

      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = TFCOutputChild(id = 0, qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val outputChild2 = TFCOutputChild(id = 0, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

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
      val claimant = testClaimant(hoursPerWeek = 16.50, isPartner = false,
        disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
      val child1 = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild1,
        disability = TFCDisability(disabled = false, severelyDisabled = false))
      val child2 = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild2,
        disability = TFCDisability(disabled = false, severelyDisabled = false))
      val child3 = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild3,
        disability = TFCDisability(disabled = true, severelyDisabled = false))
      val tfc = TFCEligibilityInput(from = from, 2, location = "england", List(claimant), List(child1, child2, child3))
      val result = tfcEligibility.determineTFCPeriods(tfc)

      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val startPeriod1 = LocalDate.parse("2016-08-01", formatter)
      val untilPeriod1 = LocalDate.parse("2016-11-01", formatter)
      val startPeriod2 = LocalDate.parse("2016-11-01", formatter)
      val untilPeriod2 = LocalDate.parse("2017-02-01", formatter)

      val Period1OutputChild1 = TFCOutputChild(id = 0, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period1OutputChild2 = TFCOutputChild(id = 0, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period1OutputChild3 = TFCOutputChild(id = 0, qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))

      val Period2OutputChild1 = TFCOutputChild(id = 0, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period2OutputChild2 = TFCOutputChild(id = 0, qualifying = true, from = Some(dateOfBirthChild2), until = Some(untilPeriod2), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period2OutputChild3 = TFCOutputChild(id = 0, qualifying = true, from = Some(startPeriod2), until = Some(untilPeriod2), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))

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
      val claimant = testClaimant(hoursPerWeek = 16.50, isPartner = false,
        disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
      val child1 = testChild(id = 1, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild1,
        disability = TFCDisability(disabled = false, severelyDisabled = false))
      val child2 = testChild(id = 2, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild2,
        disability = TFCDisability(disabled = true, severelyDisabled = false))
      val child3 = testChild(id = 3, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild3,
        disability = TFCDisability(disabled = false, severelyDisabled = false))
      val tfc = TFCEligibilityInput(from = from, 8, location = "england", List(claimant), List(child1, child2, child3))
      val result = tfcEligibility.determineTFCPeriods(tfc)

      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
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

      val Period1OutputChild1 = TFCOutputChild(id = 1, qualifying = true, from = Some(startPeriod1), until = Some(child1EndDate), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period1OutputChild2 = TFCOutputChild(id = 2, qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period1OutputChild3 = TFCOutputChild(id = 3, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period2OutputChild1 = TFCOutputChild(id = 1, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period2OutputChild2 = TFCOutputChild(id = 2, qualifying = true, from = Some(startPeriod2), until = Some(untilPeriod2), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period2OutputChild3 = TFCOutputChild(id = 3, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period3OutputChild1 = TFCOutputChild(id = 1, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period3OutputChild2 = TFCOutputChild(id = 2, qualifying = true, from = Some(startPeriod3), until = Some(untilPeriod3), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period3OutputChild3 = TFCOutputChild(id = 3, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period4OutputChild1 = TFCOutputChild(id = 1, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period4OutputChild2 = TFCOutputChild(id = 2, qualifying = true, from = Some(startPeriod4), until = Some(untilPeriod4), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period4OutputChild3 = TFCOutputChild(id = 3, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period5OutputChild1 = TFCOutputChild(id = 1, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period5OutputChild2 = TFCOutputChild(id = 2, qualifying = true, from = Some(startPeriod5), until = Some(child2EndDate), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period5OutputChild3 = TFCOutputChild(id = 3, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period6OutputChild1 = TFCOutputChild(id = 1, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period6OutputChild2 = TFCOutputChild(id = 2, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period6OutputChild3 = TFCOutputChild(id = 3, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period7OutputChild1 = TFCOutputChild(id = 1, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period7OutputChild2 = TFCOutputChild(id = 2, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period7OutputChild3 = TFCOutputChild(id = 3, qualifying = true, from = Some(dateOfBirthChild3), until = Some(untilPeriod7), tfcRollout = dateOfBirthChild3.isBefore(LocalDate.now().plusWeeks(2)),
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period8OutputChild1 = TFCOutputChild(id = 1, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period8OutputChild2 = TFCOutputChild(id = 2, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period8OutputChild3 = TFCOutputChild(id = 3, qualifying = true, from = Some(startPeriod8), until = Some(untilPeriod8), tfcRollout = dateOfBirthChild3.isBefore(LocalDate.now().plusWeeks(2)),
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

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
      val claimant = testClaimant(hoursPerWeek = 16.50, isPartner = false,
        disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
      val child1 = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild1,
        disability = TFCDisability(disabled = false, severelyDisabled = false))
      val child2 = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild2,
        disability = TFCDisability(disabled = true, severelyDisabled = false))

      val tfc = TFCEligibilityInput(from = from, 4, location = "england", List(claimant), List(child1, child2))
      val result = tfcEligibility.determineTFCPeriods(tfc)

      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val startPeriod1 = LocalDate.parse("2016-05-30", formatter)
      val untilPeriod1 = LocalDate.parse("2016-08-30", formatter)
      val startPeriod2 = LocalDate.parse("2016-08-30", formatter)
      val untilPeriod2 = LocalDate.parse("2016-11-30", formatter)
      val startPeriod3 = LocalDate.parse("2016-11-30", formatter)
      val untilPeriod3 = LocalDate.parse("2017-02-28", formatter)
      val startPeriod4 = LocalDate.parse("2017-02-28", formatter)
      val untilPeriod4 = LocalDate.parse("2017-05-28", formatter)

      val child1EndDate = LocalDate.parse("2016-09-04", formatter)

      val Period1OutputChild1 = TFCOutputChild(id = 0, qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period1OutputChild2 = TFCOutputChild(id = 0, qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))

      val Period2OutputChild1 = TFCOutputChild(id = 0, qualifying = true, from = Some(startPeriod2), until = Some(child1EndDate), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period2OutputChild2 = TFCOutputChild(id = 0, qualifying = true, from = Some(startPeriod2), until = Some(untilPeriod2), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))

      val Period3OutputChild1 = TFCOutputChild(id = 0, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period3OutputChild2 = TFCOutputChild(id = 0, qualifying = true, from = Some(startPeriod3), until = Some(untilPeriod3), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))

      val Period4OutputChild1 = TFCOutputChild(id = 0, qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period4OutputChild2 = TFCOutputChild(id = 0, qualifying = true, from = Some(startPeriod4), until = Some(untilPeriod4), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period1OutputChild1, Period1OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod2,
          until = untilPeriod2,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period2OutputChild1, Period2OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod3,
          until = untilPeriod3,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period3OutputChild1, Period3OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod4,
          until = untilPeriod4,
          periodEligibility = true,
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
      val claimant = testClaimant(hoursPerWeek = 16.50, isPartner = false,
        disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
      val child1 = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild1,
        disability = TFCDisability(disabled = false, severelyDisabled = false))
      val child2 = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild2,
        disability = TFCDisability(disabled = true, severelyDisabled = false))
      val child3 = testChild(id = 0, childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild3,
        disability = TFCDisability(disabled = false, severelyDisabled = false))
      val tfc = TFCEligibilityInput(from = from, 6, location = "england", List(claimant), List(child1, child2, child3))
      val result = tfcEligibility.determineTFCPeriods(tfc)

      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
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

      val Period1OutputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period1OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period1OutputChild3 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period2OutputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period2OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod2), until = Some(untilPeriod2), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period2OutputChild3 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod2), until = Some(untilPeriod2), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period3OutputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period3OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod3), until = Some(untilPeriod3), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period3OutputChild3 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod3), until = Some(untilPeriod3), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period4OutputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period4OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod4), until = Some(untilPeriod4), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period4OutputChild3 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod4), until = Some(untilPeriod4), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period5OutputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period5OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod5), until = Some(untilPeriod5), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period5OutputChild3 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod5), until = Some(untilPeriod5), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period6OutputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period6OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod6), until = Some(untilPeriod6), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period6OutputChild3 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod6), until = Some(untilPeriod6), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

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
      val claimant = testClaimant(hoursPerWeek = 16.50, isPartner = false,
        disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
      val claimant1 = testClaimant(hoursPerWeek = 16.50, isPartner = false,
        disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)

      val child1 = testChild(id = 0,  childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild1,
        disability = TFCDisability(disabled = false, severelyDisabled = false))
      val child2 = testChild(id = 0,  childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild2,
        disability = TFCDisability(disabled = true, severelyDisabled = false))

      val tfc = TFCEligibilityInput(from = from, 3, location = "england", List(claimant, claimant1), List(child1, child2))
      val result = tfcEligibility.determineTFCPeriods(tfc)

      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val outputClaimant1 = TFCOutputClaimant(qualifying = true, isPartner = false)

      val startPeriod1 = LocalDate.parse("2017-06-30", formatter)
      val untilPeriod1 = LocalDate.parse("2017-09-30", formatter)
      val startPeriod2 = LocalDate.parse("2017-09-30", formatter)
      val untilPeriod2 = LocalDate.parse("2017-12-30", formatter)
      val startPeriod3 = LocalDate.parse("2017-12-30", formatter)
      val untilPeriod3 = LocalDate.parse("2018-03-30", formatter)

      val child1EndDate = LocalDate.parse("2017-09-03", formatter)

      val Period1OutputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(child1EndDate), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period1OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))

      val Period2OutputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period2OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod2), until = Some(untilPeriod2), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))

      val Period3OutputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period3OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod3), until = Some(untilPeriod3), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          periodEligibility = true,
          claimants = List(outputClaimant, outputClaimant1),
          children = List(Period1OutputChild1, Period1OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod2,
          until = untilPeriod2,
          periodEligibility = true,
          claimants = List(outputClaimant, outputClaimant1),
          children = List(Period2OutputChild1, Period2OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod3,
          until = untilPeriod3,
          periodEligibility = true,
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

      val claimant = testClaimant(hoursPerWeek = 16.50, isPartner = false,
        disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
      val claimant1 = testClaimant(hoursPerWeek = 16.50, isPartner = false,
        disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)

      val child1 = testChild(id = 0,  childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild1,
        disability = TFCDisability(disabled = true, severelyDisabled = false))
      val child2 = testChild(id = 0,  childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild2,
        disability = TFCDisability(disabled = false, severelyDisabled = false))
      val child3 = testChild(id = 0,  childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild3,
        disability = TFCDisability(disabled = false, severelyDisabled = false))
      val tfc = TFCEligibilityInput(from = from, 5, location = "england", List(claimant, claimant1), List(child1, child2, child3))
      val result = tfcEligibility.determineTFCPeriods(tfc)

      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val outputClaimant1 = TFCOutputClaimant(qualifying = true, isPartner = false)

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

      val Period1OutputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period1OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period1OutputChild3 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period2OutputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod2), until = Some(untilPeriod2), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period2OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod2), until = Some(untilPeriod2), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period2OutputChild3 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period3OutputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod3), until = Some(untilPeriod3), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period3OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod3), until = Some(untilPeriod3), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period3OutputChild3 = TFCOutputChild(id = 0,  qualifying = true, from = Some(dateOfBirthChild3), until = Some(untilPeriod3),
        tfcRollout = dateOfBirthChild3.isBefore(LocalDate.now().plusWeeks(2)),
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period4OutputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod4), until = Some(untilPeriod4), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period4OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod4), until = Some(untilPeriod4), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period4OutputChild3 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod4), until = Some(untilPeriod4),
        tfcRollout = dateOfBirthChild3.isBefore(LocalDate.now().plusWeeks(2)),
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period5OutputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod5), until = Some(untilPeriod5), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period5OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod5), until = Some(child2EndDate), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period5OutputChild3 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod5), until = Some(untilPeriod5),
        tfcRollout = dateOfBirthChild3.isBefore(LocalDate.now().plusWeeks(2)),
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

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
      val claimant = testClaimant(hoursPerWeek = 16.50, isPartner = false,
        disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
      val child1 = testChild(id = 0,  childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild1,
        disability = TFCDisability(disabled = true, severelyDisabled = false))
      val child2 = testChild(id = 0,  childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild2,
        disability = TFCDisability(disabled = false, severelyDisabled = false))
      val tfc = TFCEligibilityInput(from = from, 7, location = "england", List(claimant), List(child1, child2))
      val result = tfcEligibility.determineTFCPeriods(tfc)

      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
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

      val Period1OutputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period1OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period2OutputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod2), until = Some(untilPeriod2), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period2OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod2), until = Some(untilPeriod2), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period3OutputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod3), until = Some(untilPeriod3), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period3OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod3), until = Some(untilPeriod3), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period4OutputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod4), until = Some(untilPeriod4), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period4OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod4), until = Some(untilPeriod4), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period5OutputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod5), until = Some(untilPeriod5), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period5OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod5), until = Some(untilPeriod5), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period6OutputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod6), until = Some(untilPeriod6), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period6OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod6), until = Some(untilPeriod6), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period7OutputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod7), until = Some(untilPeriod7), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period7OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod7), until = Some(untilPeriod7), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period1OutputChild1, Period1OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod2,
          until = untilPeriod2,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period2OutputChild1, Period2OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod3,
          until = untilPeriod3,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period3OutputChild1, Period3OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod4,
          until = untilPeriod4,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period4OutputChild1, Period4OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod5,
          until = untilPeriod5,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period5OutputChild1, Period5OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod6,
          until = untilPeriod6,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period6OutputChild1, Period6OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod7,
          until = untilPeriod7,
          periodEligibility = true,
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
      val claimant = testClaimant(hoursPerWeek = 16.50, isPartner = false,
        disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
      val child1 = testChild(id = 0,  childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild1,
        disability = TFCDisability(disabled = true, severelyDisabled = false))
      val child2 = testChild(id = 0,  childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild2,
        disability = TFCDisability(disabled = true, severelyDisabled = false))
      val tfc = TFCEligibilityInput(from = from, 8, location = "england", List(claimant), List(child1, child2))
      val result = tfcEligibility.determineTFCPeriods(tfc)

      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
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

      val Period1OutputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period1OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))

      val Period2OutputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod2), until = Some(untilPeriod2), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period2OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod2), until = Some(untilPeriod2), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))

      val Period3OutputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod3), until = Some(untilPeriod3), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period3OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod3), until = Some(untilPeriod3), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))

      val Period4OutputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod4), until = Some(untilPeriod4), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period4OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod4), until = Some(untilPeriod4), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))

      val Period5OutputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod5), until = Some(untilPeriod5), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period5OutputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod5), until = Some(child2EndDate), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))

      val Period6OutputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod6), until = Some(untilPeriod6), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period6OutputChild2 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))

      val Period7OutputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod7), until = Some(untilPeriod7), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period7OutputChild2 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))

      val Period8OutputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod8), until = Some(untilPeriod8), tfcRollout = true,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period8OutputChild2 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          claimants = List(outputClaimant),
          periodEligibility = true,
          children = List(Period1OutputChild1, Period1OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod2,
          until = untilPeriod2,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period2OutputChild1, Period2OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod3,
          until = untilPeriod3,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period3OutputChild1, Period3OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod4,
          until = untilPeriod4,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period4OutputChild1, Period4OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod5,
          until = untilPeriod5,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period5OutputChild1, Period5OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod6,
          until = untilPeriod6,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period6OutputChild1, Period6OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod7,
          until = untilPeriod7,
          periodEligibility = true,
          claimants = List(outputClaimant),
          children = List(Period7OutputChild1, Period7OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod8,
          until = untilPeriod8,
          periodEligibility = true,
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
      val claimant = testClaimant(hoursPerWeek = 16.50, isPartner = false,
        disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
      val child1 = testChild(id = 0,  childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild1,
        disability = TFCDisability(disabled = false, severelyDisabled = false))
      val child2 = testChild(id = 0,  childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild2,
        disability = TFCDisability(disabled = false, severelyDisabled = false))

      val tfc = TFCEligibilityInput(from = from, 4, location = "england", List(claimant), List(child1, child2))
      val result = tfcEligibility.determineTFCPeriods(tfc)

      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val startPeriod1 = LocalDate.parse("2016-05-23", formatter)
      val untilPeriod1 = LocalDate.parse("2016-08-23", formatter)
      val startPeriod2 = LocalDate.parse("2016-08-23", formatter)
      val untilPeriod2 = LocalDate.parse("2016-11-23", formatter)
      val startPeriod3 = LocalDate.parse("2016-11-23", formatter)
      val untilPeriod3 = LocalDate.parse("2017-02-23", formatter)
      val startPeriod4 = LocalDate.parse("2017-02-23", formatter)
      val untilPeriod4 = LocalDate.parse("2017-05-23", formatter)

      val Period1OutputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period1OutputChild2 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period2OutputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period2OutputChild2 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period3OutputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period3OutputChild2 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      val Period4OutputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))
      val Period4OutputChild2 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(false,false))

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          periodEligibility = false,
          claimants = List(outputClaimant),
          children = List(Period1OutputChild1, Period1OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod2,
          until = untilPeriod2,
          periodEligibility = false,
          claimants = List(outputClaimant),
          children = List(Period2OutputChild1, Period2OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod3,
          until = untilPeriod3,
          periodEligibility = false,
          claimants = List(outputClaimant),
          children = List(Period3OutputChild1, Period3OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod4,
          until = untilPeriod4,
          periodEligibility = false,
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
      val claimant = testClaimant(hoursPerWeek = 16.50, isPartner = false,
        disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
      val child1 = testChild(id = 0,  childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild1,
        disability = TFCDisability(disabled = true, severelyDisabled = false))
      val child2 = testChild(id = 0,  childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirthChild2,
        disability = TFCDisability(disabled = true, severelyDisabled = false))

      val tfc = TFCEligibilityInput(from = from, 4, location = "england", List(claimant), List(child1, child2))
      val result = tfcEligibility.determineTFCPeriods(tfc)

      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val startPeriod1 = LocalDate.parse("2016-05-30", formatter)
      val untilPeriod1 = LocalDate.parse("2016-08-30", formatter)
      val startPeriod2 = LocalDate.parse("2016-08-30", formatter)
      val untilPeriod2 = LocalDate.parse("2016-11-30", formatter)
      val startPeriod3 = LocalDate.parse("2016-11-30", formatter)
      val untilPeriod3 = LocalDate.parse("2017-02-28", formatter)
      val startPeriod4 = LocalDate.parse("2017-02-28", formatter)
      val untilPeriod4 = LocalDate.parse("2017-05-28", formatter)

      val Period1OutputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period1OutputChild2 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))

      val Period2OutputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period2OutputChild2 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))

      val Period3OutputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period3OutputChild2 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))

      val Period4OutputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))
      val Period4OutputChild2 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability(true,false))

      result shouldBe List(
        TFCPeriod(
          from = startPeriod1,
          until = untilPeriod1,
          periodEligibility = false,
          claimants = List(outputClaimant),
          children = List(Period1OutputChild1, Period1OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod2,
          until = untilPeriod2,
          periodEligibility = false,
          claimants = List(outputClaimant),
          children = List(Period2OutputChild1, Period2OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod3,
          until = untilPeriod3,
          periodEligibility = false,
          claimants = List(outputClaimant),
          children = List(Period3OutputChild1, Period3OutputChild2)
        ),
        TFCPeriod(
          from = startPeriod4,
          until = untilPeriod4,
          periodEligibility = false,
          claimants = List(outputClaimant),
          children = List(Period4OutputChild1, Period4OutputChild2)
        )
      )
    }

    "determine period eligibility for 1 period - 1 qualifying child" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = TFCOutputClaimant(qualifying = false, isPartner = false)
      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val result = tfcEligibility.determinePeriodEligibility(List(outputClaimant), List(outputChild1))

      result shouldBe false
    }


    "determine period eligibility for 1 period - 1 qualifying child, partner exists " in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val outputClaimant1 = TFCOutputClaimant(qualifying = true, isPartner = true)

      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val result = tfcEligibility.determinePeriodEligibility(List(outputClaimant, outputClaimant1), List(outputChild1))

      result shouldBe true
    }

    "determine period eligibility for 1 period - 1 qualifying child, partner exists does not qualify " in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val outputClaimant1 = TFCOutputClaimant(qualifying = false, isPartner = true)

      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val result = tfcEligibility.determinePeriodEligibility(List(outputClaimant, outputClaimant1), List(outputChild1))

      result shouldBe false
    }

    "determine period eligibility for 1 period - 1 qualifying child, partner exists, claimant does not qualify " in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = TFCOutputClaimant(qualifying = false, isPartner = false)
      val outputClaimant1 = TFCOutputClaimant(qualifying = true, isPartner = true)

      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val result = tfcEligibility.determinePeriodEligibility(List(outputClaimant, outputClaimant1), List(outputChild1))

      result shouldBe false
    }

    "determine period eligibility for 1 period - 1 child, not qualifying, partner not qualifying, claimant does not qualify " in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = TFCOutputClaimant(qualifying = false, isPartner = false)
      val outputClaimant1 = TFCOutputClaimant(qualifying = false, isPartner = true)

      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val result = tfcEligibility.determinePeriodEligibility(List(outputClaimant, outputClaimant1), List(outputChild1))

      result shouldBe false
    }

    "determine period eligibility for 1 period - 1 child, not qualifying, 1 partner qualifying " in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = TFCOutputClaimant(qualifying = false, isPartner = false)
      val outputClaimant1 = TFCOutputClaimant(qualifying = true, isPartner = true)

      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val result = tfcEligibility.determinePeriodEligibility(List(outputClaimant, outputClaimant1), List(outputChild1))

      result shouldBe false
    }

    "determine period eligibility for 1 period - no qualifying child and non qual claimant" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = TFCOutputClaimant(qualifying = false, isPartner = false)
      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val result = tfcEligibility.determinePeriodEligibility(List(outputClaimant), List(outputChild1))

      result shouldBe false
    }


    "determine period eligibility for 1 period - 1 qualifying child and qual claimant" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val result = tfcEligibility.determinePeriodEligibility(List(outputClaimant), List(outputChild1))

      result shouldBe true
    }

    "determine period eligibility for 1 period - non qualifying child and qualifying claimant with weekly cost" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = TFCOutputChild(id = 0, childcareCostPeriod=Periods.Weekly, qualifying = false, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val result = tfcEligibility.determinePeriodEligibility(List(outputClaimant), List(outputChild1))

      result shouldBe false
    }

    "determine period eligibility for 1 period - non qualifying child and qualifying claimant" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val result = tfcEligibility.determinePeriodEligibility(List(outputClaimant), List(outputChild1))

      result shouldBe false
    }

    "determine period eligibility for 1 period - 2 children, both qualifying" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val outputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val result = tfcEligibility.determinePeriodEligibility(List(outputClaimant), List(outputChild1, outputChild2))

      result shouldBe true
    }

    "determine period eligibility for 1 period - 2 children, both non qualifying" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val outputChild2 = TFCOutputChild(id = 0,  qualifying = false, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val result = tfcEligibility.determinePeriodEligibility(List(outputClaimant), List(outputChild1, outputChild2))

      result shouldBe false
    }

    "determine period eligibility for 1 period - 2 children, one qualifying" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val outputChild2 = TFCOutputChild(id = 0,  qualifying = false, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val result = tfcEligibility.determinePeriodEligibility(List(outputClaimant), List(outputChild1, outputChild2))

      result shouldBe true
    }

    "determine period eligibility for 1 period - 2 children, one qualifying, one claimaint qualifying" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val outputClaimant1 = TFCOutputClaimant(qualifying = false, isPartner = false)

      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val outputChild2 = TFCOutputChild(id = 0,  qualifying = false, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val result = tfcEligibility.determinePeriodEligibility(List(outputClaimant, outputClaimant1), List(outputChild1, outputChild2))

      result shouldBe false
    }

    "determine period eligibility for 1 period - 2 children, one qualifying, both claimaints qualifying" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val outputClaimant1 = TFCOutputClaimant(qualifying = true, isPartner = false)

      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val outputChild2 = TFCOutputChild(id = 0,  qualifying = false, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val result = tfcEligibility.determinePeriodEligibility(List(outputClaimant, outputClaimant1), List(outputChild1, outputChild2))

      result shouldBe true
    }

    "determine period eligibility for 1 period - 3 children, all qualifying, both claimaints qualifying" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val outputClaimant1 = TFCOutputClaimant(qualifying = true, isPartner = false)

      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val outputChild2 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val outputChild3 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val result = tfcEligibility.determinePeriodEligibility(List(outputClaimant, outputClaimant1), List(outputChild1, outputChild2, outputChild3))

      result shouldBe true
    }

    "determine period eligibility for 1 period - 3 children, all non qualifying, both claimaints qualifying" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val outputClaimant1 = TFCOutputClaimant(qualifying = true, isPartner = false)

      val startPeriod1 = LocalDate.parse("2016-09-27", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-27", formatter)

      val outputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val outputChild2 = TFCOutputChild(id = 0,  qualifying = false, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val outputChild3 = TFCOutputChild(id = 0,  qualifying = false, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false)

      val result = tfcEligibility.determinePeriodEligibility(List(outputClaimant, outputClaimant1), List(outputChild1, outputChild2, outputChild3))

      result shouldBe false
    }

    "determine household if claimant fails eligibility" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-08-27", formatter)
      val from = LocalDate.parse("2016-03-30", formatter)
      val claimantIncome = Some(TFCIncome(Some(1199999.0),Some(100.0),Some(100.0)))
      val claimant = testClaimant(currentIncome = claimantIncome,
        hoursPerWeek = 16.50,
        disability = TFCDisability(), minimumEarnings = TFCMinimumEarnings(), age = None)
      val child = testChild(id = 0,  childCareCost = BigDecimal(200.00),
        childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability())
      val tfc = TFCEligibilityInput(from = from, numberOfPeriods = 1, location = "england", List(claimant), List(child))
      val result = await(tfcEligibility.eligibility(tfc))

      val outputClaimant = TFCOutputClaimant(qualifying = false, isPartner = false)
      val startPeriod = LocalDate.parse("2016-03-30", formatter)
      val untilPeriod = LocalDate.parse("2016-06-30", formatter)
      val PeriodOutputChild = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod), until = Some(untilPeriod), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability())
      val tfcPeriods = List(TFCPeriod(from = startPeriod, until = untilPeriod, periodEligibility = false, claimants = List(outputClaimant), children = List(PeriodOutputChild)))
      val tfcEligibilityModel = TFCEligibilityOutput(from = from, until = tfcPeriods.last.until, householdEligibility = false, periods = tfcPeriods, tfcRollout = false)
      result shouldBe tfcEligibilityModel
    }

    "determine household if claimant has eligibility - 2 periods" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-08-27", formatter)
      val from = LocalDate.parse("2016-06-30", formatter)
      val claimant = testClaimant(hoursPerWeek = 16.50,
        disability = TFCDisability(), minimumEarnings = TFCMinimumEarnings(), age = None)
      val child = testChild(id = 0,  childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability())
      val tfc = TFCEligibilityInput(from = from, numberOfPeriods = 2, location = "england", List(claimant), List(child))
      val result = await(tfcEligibility.eligibility(tfc))

      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val outputClaimant1 = TFCOutputClaimant(qualifying = true, isPartner = false)
      val startPeriod = LocalDate.parse("2016-06-30", formatter)
      val untilPeriod = LocalDate.parse("2016-09-30", formatter)
      val startPeriod1 = LocalDate.parse("2016-09-30", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-30", formatter)
      val septDate = LocalDate.parse("2016-09-04", formatter)
      val PeriodOutputChild = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod), until = Some(septDate), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability())
      val PeriodOutputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability())

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
      val tfcEligibilityModel = TFCEligibilityOutput(from = from, until = tfcPeriods.last.until, householdEligibility = true, periods = tfcPeriods, tfcRollout = false)
      result shouldBe tfcEligibilityModel
    }

    "determine household if claimant and child has eligibility - 2 periods" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2006-08-27", formatter)
      val from = LocalDate.parse("2016-06-30", formatter)
      val claimant = testClaimant(hoursPerWeek = 16.50,
        disability = TFCDisability(), minimumEarnings = TFCMinimumEarnings(), age = None)
      val child = testChild(id = 0,  childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability())
      val tfc = TFCEligibilityInput(from = from, numberOfPeriods = 2, location = "england", List(claimant), List(child))
      val result = await(tfcEligibility.eligibility(tfc))

      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val outputClaimant1 = TFCOutputClaimant(qualifying = true, isPartner = false)
      val startPeriod = LocalDate.parse("2016-06-30", formatter)
      val untilPeriod = LocalDate.parse("2016-09-30", formatter)
      val startPeriod1 = LocalDate.parse("2016-09-30", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-30", formatter)

      val PeriodOutputChild = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod), until = Some(untilPeriod), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability())
      val PeriodOutputChild1 = TFCOutputChild(id = 0,  qualifying = true, from = Some(startPeriod1), until = Some(untilPeriod1), tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability())

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
      val tfcEligibilityModel = TFCEligibilityOutput(from = from, until = tfcPeriods.last.until, householdEligibility = true, periods = tfcPeriods, tfcRollout = false)
      result shouldBe tfcEligibilityModel
    }

    "determine household if claimant has eligibility and child does not have eligibility- 3 periods" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2002-08-27", formatter)
      val from = LocalDate.parse("2016-06-30", formatter)
      val claimant = testClaimant(hoursPerWeek = 16.50,
        disability = TFCDisability(), minimumEarnings = TFCMinimumEarnings(amount = 120.0), maximumEarnings =  Some(false), age = None)
      val child = testChild(id = 0,  childCareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth,
        disability = TFCDisability())
      val tfc = TFCEligibilityInput(from = from, numberOfPeriods = 3, location = "england", List(claimant), List(child))
      val result = await(tfcEligibility.eligibility(tfc))

      val outputClaimant = TFCOutputClaimant(qualifying = true, isPartner = false)
      val startPeriod = LocalDate.parse("2016-06-30", formatter)
      val untilPeriod = LocalDate.parse("2016-09-30", formatter)
      val startPeriod1 = LocalDate.parse("2016-09-30", formatter)
      val untilPeriod1 = LocalDate.parse("2016-12-30", formatter)
      val startPeriod2 = LocalDate.parse("2016-12-30", formatter)
      val untilPeriod2 = LocalDate.parse("2017-03-30", formatter)

      val PeriodOutputChild = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability())
      val PeriodOutputChild1 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability())
      val PeriodOutputChild2 = TFCOutputChild(id = 0,  qualifying = false, from = None, until = None, tfcRollout = false,
        BigDecimal(200.00),childcareCostPeriod=Periods.Monthly, models.output.tfc.TFCDisability())

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
      val tfcEligibilityModel = TFCEligibilityOutput(from = from, until = tfcPeriods.last.until, householdEligibility = false, periods = tfcPeriods, tfcRollout = false)
      result shouldBe tfcEligibilityModel
    }

  }
}
