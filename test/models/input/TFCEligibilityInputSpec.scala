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

package models.input

import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader
import controllers.FakeCCEligibilityApplication
import models.input.tfc._
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}
import play.api.mvc.Request
import play.api.test.FakeRequest
import uk.gov.hmrc.http.HeaderCarrier
import utils.{CCConfigSpec, Periods, TFCConfig}

class TFCEligibilityInputSpec extends CCConfigSpec
  with FakeCCEligibilityApplication
  with MockitoSugar {

  implicit val req = FakeRequest()

  "TFCInputEligibility" should {

    val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
    val fromDate = LocalDate.parse("2000-08-27", formatter)

    "Read a valid JSON input and convert to a specific type" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/input/tfc/eligibility_input_test.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TFCEligibilityInput]
      result match {
        case JsSuccess(x, _) =>
          x shouldBe a[TFCEligibilityInput]
          x should not be null

          //TFC model
          x.from shouldBe a[LocalDate]
          x.numberOfPeriods.isInstanceOf[Short] shouldBe true
          x.claimants.isInstanceOf[List[TFCClaimant]] shouldBe true
          x.children.isInstanceOf[List[TFCChild]] shouldBe true

          //Claimant model
          x.claimants.head.totalIncome shouldBe a[BigDecimal]
          x.claimants.head.hoursPerWeek.isInstanceOf[Double] shouldBe true
          x.claimants.head.isPartner.isInstanceOf[Boolean] shouldBe true
          x.claimants.head.disability shouldBe a[TFCDisability]

          //Claimant Disability model
          x.claimants.head.disability.disabled.isInstanceOf[Boolean] shouldBe true
          x.claimants.head.disability.severelyDisabled.isInstanceOf[Boolean] shouldBe true

          //Other Support model
          x.claimants.head.carersAllowance.isInstanceOf[Boolean] shouldBe true

          //Child model
          x.children.head.id.isInstanceOf[Short] shouldBe true
          x.children.head.childcareCost shouldBe a[BigDecimal]
          x.children.head.childcareCostPeriod shouldBe a[Periods.Period]
          x.children.head.dob shouldBe a[LocalDate]
          x.children.head.disability shouldBe a[TFCDisability]

          //Child Disability model
          x.children.head.disability.disabled.isInstanceOf[Boolean] shouldBe true
          x.children.head.disability.severelyDisabled.isInstanceOf[Boolean] shouldBe true

        case JsError(e) => throw new IllegalArgumentException(e.toString)
      }
    }

    "Child" should {

      "Determine disability status if child is disabled" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
        val child = TFCChild(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability =
          TFCDisability(disabled = true))
        child.isDisabled shouldBe true
      }

      "Determine disability status if child is severely disabled" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
        val child = TFCChild(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability =
          TFCDisability(severelyDisabled = true))
        child.isDisabled shouldBe true
      }

      "Determine disability status if child is not disabled and not severely disabled" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
        val child = TFCChild(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability =
          TFCDisability())
        child.isDisabled shouldBe false
      }

      "Determine disability status if child is disabled and severely disabled" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
        val child = TFCChild(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability =
          TFCDisability(disabled = true, severelyDisabled = true))
        child.isDisabled shouldBe true
      }

      "determine childs Birthday(11th or 16th) where child is not disable" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2005-08-27", formatter)
        val current = LocalDate.parse("2017-08-01", formatter)
        val child = TFCChild(id = 0, childcareCost = BigDecimal(300.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability =
          TFCDisability())
        val child11Birthday = child.getChildBirthday(current, "england")
        LocalDate.fromDateFields(child11Birthday) shouldBe LocalDate.parse("2016-08-27", formatter)
      }

      "determine childs Birthday (11th or 16th) where child is disabled" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2000-09-27", formatter)
        val current = LocalDate.parse("2017-08-01", formatter)
        val child = TFCChild(id = 0, childcareCost = BigDecimal(300.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability =
          TFCDisability(disabled = true))
        val child11Birthday = child.getChildBirthday(current, "england")
        LocalDate.fromDateFields(child11Birthday) shouldBe LocalDate.parse("2016-09-27", formatter)
      }

      "determine end of Week following 1st September Date where child is not disabled" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2005-08-27", formatter)
        val current = LocalDate.parse("2017-08-01", formatter)
        val child = TFCChild(id = 0, childcareCost = BigDecimal(300.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability =
          TFCDisability())
        val endWeek1stOfSeptemberDate = child.endWeek1stOfSeptemberDate(current, "england")
        LocalDate.fromDateFields(endWeek1stOfSeptemberDate) shouldBe LocalDate.parse("2016-09-04", formatter)
      }

      "determine end of Week following 1st September Date where child is not disabled, dob after 1st Sept" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2005-09-12", formatter)
        val current = LocalDate.parse("2017-08-01", formatter)
        val child = TFCChild(id = 0, childcareCost = BigDecimal(300.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability =
          TFCDisability())
        val endWeek1stOfSeptemberDate = child.endWeek1stOfSeptemberDate(current, "england")
        LocalDate.fromDateFields(endWeek1stOfSeptemberDate) shouldBe LocalDate.parse("2017-09-03", formatter)
      }

      "determine end of Week following 1st September Date where child is disabled" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2005-08-27", formatter)
        val current = LocalDate.parse("2017-08-01", formatter)
        val child = TFCChild(id = 0, childcareCost = BigDecimal(300.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability =
          TFCDisability(disabled = true))
        val endWeek1stOfSeptemberDate = child.endWeek1stOfSeptemberDate(current, "england")
        LocalDate.fromDateFields(endWeek1stOfSeptemberDate) shouldBe LocalDate.parse("2021-09-05", formatter)
      }

      "determine end of Week following 1st September Date where child is disabled, dob after 1st Sept" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2005-09-12", formatter)
        val current = LocalDate.parse("2017-08-01", formatter)
        val child = TFCChild(id = 0, childcareCost = BigDecimal(300.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability =
          TFCDisability(disabled = true))
        val endWeek1stOfSeptemberDate = child.endWeek1stOfSeptemberDate(current, "england")
        LocalDate.fromDateFields(endWeek1stOfSeptemberDate) shouldBe LocalDate.parse("2022-09-04", formatter)
      }

    }

    "Claimant" should {

      "Check if claimant and partner eligible for minimum earnings rule (minimum 8 hours)" in {
        val claimant = TFCClaimant(hoursPerWeek = 16.01, disability = TFCDisability(), minimumEarnings =
          TFCMinimumEarnings(), age = None)
        val partner = TFCClaimant(hoursPerWeek = 18.01, isPartner = true, disability = TFCDisability(), minimumEarnings =
          TFCMinimumEarnings(), age = None)
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val fromDate = LocalDate.parse("2000-08-27", formatter)
        val toDate = LocalDate.parse("2000-08-27", formatter)

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())
        tfc.claimants.head.isWorkingAtLeast16HoursPerWeek(fromDate, tfc.location) shouldBe true
        tfc.claimants.last.isWorkingAtLeast16HoursPerWeek(fromDate, tfc.location) shouldBe true
      }

      "Check if claimant and partner eligible for maximum earnings rule (maximum £100000 earnings)" in {
        val claimant = TFCClaimant(hoursPerWeek = 16.50, disability = TFCDisability(), minimumEarnings =
          TFCMinimumEarnings(), age = None)
        val partner = TFCClaimant(hoursPerWeek = 16.50, isPartner = true, disability = TFCDisability(), minimumEarnings =
          TFCMinimumEarnings(), age = None)
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val fromDate = LocalDate.parse("2000-08-27", formatter)
        val toDate = LocalDate.parse("2000-08-27", formatter)

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())

        tfc.claimants.head.isTotalIncomeLessThan100000(fromDate, tfc.location) shouldBe true
        tfc.claimants.last.isTotalIncomeLessThan100000(fromDate, tfc.location) shouldBe true
      }

      "eligibility fails if claimant income > 100000" in {
        val claimant = TFCClaimant(currentIncome= Some(TFCIncome(Some(100000.01), None, None)), disability = TFCDisability(), minimumEarnings =
          TFCMinimumEarnings(), age = None)
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val fromDate = LocalDate.parse("2000-08-27", formatter)
        val toDate = LocalDate.parse("2000-08-27", formatter)

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant), List())

        tfc.claimants.head.isTotalIncomeLessThan100000(fromDate, tfc.location) shouldBe false
      }

      "eligibility fails if claimant income <= 100000 gets other income" in {
        val claimant = TFCClaimant(currentIncome= Some(TFCIncome(Some(100000.0), None, Some(900.0))), disability = TFCDisability(), minimumEarnings =
          TFCMinimumEarnings(), age = None)
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val fromDate = LocalDate.parse("2000-08-27", formatter)
        val toDate = LocalDate.parse("2000-08-27", formatter)

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant), List())

        tfc.claimants.head.isTotalIncomeLessThan100000(fromDate, tfc.location) shouldBe false
      }

      "eligibility passes if claimant income <= 100000" in {
        val claimant = TFCClaimant(currentIncome= Some(TFCIncome(Some(100000.0), None, None)), disability = TFCDisability(), minimumEarnings =
          TFCMinimumEarnings(), age = None)
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val fromDate = LocalDate.parse("2000-08-27", formatter)
        val toDate = LocalDate.parse("2000-08-27", formatter)

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant), List())

        tfc.claimants.head.isTotalIncomeLessThan100000(fromDate, tfc.location) shouldBe true
      }

      "Check if claimant and partner qualify for TFC" in {
        val claimant = TFCClaimant(hoursPerWeek = 16.50, disability = TFCDisability(), minimumEarnings =
          TFCMinimumEarnings(), age = None)
        val partner = TFCClaimant(hoursPerWeek = 17.50, isPartner = true, disability = TFCDisability(), minimumEarnings =
          TFCMinimumEarnings(), age = None)
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val fromDate = LocalDate.parse("2000-08-27", formatter)
        val toDate = LocalDate.parse("2000-08-27", formatter)

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())
        tfc.claimants.head.isQualifyingForTFC(fromDate, tfc.location) shouldBe true
        tfc.claimants.last.isQualifyingForTFC(fromDate, tfc.location) shouldBe true
      }

      "claimant and partner both not qualify (claimant fails maximum earnings rule)" in {
        val claimantIncome = Some(TFCIncome(Some(1199999.0), Some(100.0), Some(100.0)))
        val partnerIncome = Some(TFCIncome(Some(99999.0), Some(100.0), Some(100.0)))
        val claimant = TFCClaimant(currentIncome = claimantIncome,
          hoursPerWeek = 9.50,
          disability = TFCDisability(),
          minimumEarnings = TFCMinimumEarnings(),
          age = None)
        val partner = TFCClaimant(currentIncome = partnerIncome,
          hoursPerWeek = 4.50,
          isPartner = true,
          disability = TFCDisability(),
          minimumEarnings = TFCMinimumEarnings(),
          age = None)
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val fromDate = LocalDate.parse("2000-08-27", formatter)
        val toDate = LocalDate.parse("2000-08-27", formatter)

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())
        tfc.claimants.head.isQualifyingForTFC(fromDate, tfc.location) shouldBe false
        tfc.claimants.last.isQualifyingForTFC(fromDate, tfc.location) shouldBe true
      }

      "claimant qualify and partner not qualify (partner fails maximum earnings rule)" in {
        val claimantIncome = Some(TFCIncome(Some(99999.0), Some(100.0), Some(100.0)))
        val partnerIncome = Some(TFCIncome(Some(9999999.0), Some(100.0), Some(100.0)))
        val claimant = TFCClaimant(previousIncome = claimantIncome,
          hoursPerWeek = 16.50,
          disability = TFCDisability(),
          minimumEarnings = TFCMinimumEarnings(),
          age = None)
        val partner = TFCClaimant(previousIncome = partnerIncome,
          hoursPerWeek = 14.50,
          isPartner = true,
          disability = TFCDisability(),
          minimumEarnings = TFCMinimumEarnings(),
          age = None)
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val fromDate = LocalDate.parse("2000-08-27", formatter)
        val toDate = LocalDate.parse("2000-08-27", formatter)

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())
        tfc.claimants.head.isQualifyingForTFC(fromDate, tfc.location) shouldBe true
        tfc.claimants.last.isQualifyingForTFC(fromDate, tfc.location) shouldBe false
      }

      "claimant not qualify and partner qualify (claimant fails totalIncome)" in {
        val claimantIncome = Some(TFCIncome(Some(1199999.0), Some(100.0), Some(100.0)))
        val partnerIncome = Some(TFCIncome(Some(99999.0), Some(100.0), Some(100.0)))
        val claimant = TFCClaimant(currentIncome = claimantIncome,
          hoursPerWeek = 9.50,
          disability = TFCDisability(),
          minimumEarnings = TFCMinimumEarnings(),
          age = None)
        val partner = TFCClaimant(currentIncome = partnerIncome,
          hoursPerWeek = 16.50,
          isPartner = true,
          disability = TFCDisability(),
          minimumEarnings = TFCMinimumEarnings(),
          age = None)
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val fromDate = LocalDate.parse("2000-08-27", formatter)
        val toDate = LocalDate.parse("2000-08-27", formatter)

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())
        tfc.claimants.head.isQualifyingForTFC(fromDate, tfc.location) shouldBe false
        tfc.claimants.last.isQualifyingForTFC(fromDate, tfc.location) shouldBe true
      }

      "claimant qualify if carer's allowance is selected with zero hours worked" in {
        val claimant = TFCClaimant(hoursPerWeek = 0, disability = TFCDisability(), carersAllowance = true, minimumEarnings =
          TFCMinimumEarnings(), age = None)
        val partner = TFCClaimant(hoursPerWeek = 16.50, isPartner = true, disability = TFCDisability(), minimumEarnings =
          TFCMinimumEarnings(), age = None)
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val fromDate = LocalDate.parse("2000-08-27", formatter)
        val toDate = LocalDate.parse("2000-08-27", formatter)

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())
        tfc.claimants.head.isQualifyingForTFC(fromDate, tfc.location) shouldBe true
        tfc.claimants.last.isQualifyingForTFC(fromDate, tfc.location) shouldBe true
      }

    }
    "getNWMPerAge checks" should {

      "check if getNWMPerAge is correct for under-18 year olds" in {
        val claimant = TFCClaimant(disability = TFCDisability(), carersAllowance = true, minimumEarnings =
          TFCMinimumEarnings(), age = Some("under-18"))
        val taxYearConfig = TFCConfig.getConfig(LocalDate.parse("2000-08-27", formatter), "england")
        claimant.getNWMPerAge(taxYearConfig)._1 shouldBe taxYearConfig.nmwUnder18
      }
      "check if getNWMPerAge is correct for 18-20 year olds" in {
        val claimant = TFCClaimant(disability = TFCDisability(), carersAllowance = true, minimumEarnings =
          TFCMinimumEarnings(), age = Some("18-20"))
        val taxYearConfig = TFCConfig.getConfig(LocalDate.parse("2000-08-27", formatter), "england")
        claimant.getNWMPerAge(taxYearConfig)._1 shouldBe taxYearConfig.nmw18To20
      }
      "check if getNWMPerAge is correct for 21 -24 year olds" in {
        val claimant = TFCClaimant(disability = TFCDisability(), carersAllowance = true, minimumEarnings =
          TFCMinimumEarnings(), age = Some("21-24"))
        val taxYearConfig = TFCConfig.getConfig(LocalDate.parse("2000-08-27", formatter), "england")
        claimant.getNWMPerAge(taxYearConfig)._1 shouldBe taxYearConfig.nmw21To24
      }
      "check if getNWMPerAge is correct for over 25 year olds" in {
        val claimant = TFCClaimant(disability = TFCDisability(), carersAllowance = true, minimumEarnings =
          TFCMinimumEarnings(), age = Some("25 or over"))
        val taxYearConfig = TFCConfig.getConfig(LocalDate.parse("2000-08-27", formatter), "england")
        claimant.getNWMPerAge(taxYearConfig)._1 shouldBe taxYearConfig.nmw25Over
      }
    }

    "satisfyMinimumEarnings remaining checks" should {

      "with default values" in {

        val minimumEarnings = TFCMinimumEarnings(selection = false)
        val claimant = TFCClaimant(disability = TFCDisability(), carersAllowance = true, minimumEarnings =
          minimumEarnings, age = None)

        val partner = TFCClaimant(hoursPerWeek = 16.50, isPartner = true, disability = TFCDisability(), minimumEarnings =
          TFCMinimumEarnings(), age = None)

        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val fromDate = LocalDate.parse("2000-08-27", formatter)

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())
        tfc.claimants.head.satisfyMinimumEarnings(fromDate, true, "england")(req, hc) shouldBe false
        tfc.claimants.last.satisfyMinimumEarnings(fromDate, false, "england")(req, hc) shouldBe true
      }

      "with undefined ages, parent - TFCMinimumEarnings(selection = false) selfemployed, partner - apprentice" in {

        val minimumEarnings = TFCMinimumEarnings(selection = false)
        val claimant = TFCClaimant(disability = TFCDisability(), carersAllowance = true,
          minimumEarnings = minimumEarnings,
          age = None,
          selfEmployedSelection = Some(true),
          employmentStatus = Some("selfEmployed"))

        val minimumEarnings1 = TFCMinimumEarnings(selection = false)
        val partner = TFCClaimant(hoursPerWeek = 16.50, isPartner = true, disability = TFCDisability(),
          minimumEarnings = minimumEarnings1,
          age = None,
          selfEmployedSelection = Some(false),
          employmentStatus = Some("apprentice"))

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())
        tfc.claimants.head.satisfyMinimumEarnings(fromDate, true, "england")(req, hc) shouldBe true
        tfc.claimants.last.satisfyMinimumEarnings(fromDate, false, "england")(req, hc) shouldBe false
      }

      "with an 18-20 old, selfemployed and partner under 18, TFCMinimumEarnings(selection = false, amount = 0.0), apprentice" in {

        val claimant = TFCClaimant(disability = TFCDisability(),
          minimumEarnings = TFCMinimumEarnings(),
          age = Some("18-20"),
          selfEmployedSelection = Some(true),
          employmentStatus = Some("selfEmployed")
        )

        val minimumEarnings1 = TFCMinimumEarnings(selection = false, amount = 70.0)
        val partner = TFCClaimant(hoursPerWeek = 28.00, isPartner = true, disability = TFCDisability(),
          minimumEarnings = minimumEarnings1,
          age = Some("under-18"),
          selfEmployedSelection = Some(false),
          employmentStatus = Some("apprentice")
        )

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())
        tfc.claimants.head.satisfyMinimumEarnings(fromDate, true, "england")(req, hc) shouldBe true
        tfc.claimants.last.satisfyMinimumEarnings(fromDate, false, "england")(req, hc) shouldBe true
      }
    }

    "validHouseholdMinimumEarnings" should {

      val claimant = mock[TFCClaimant]
      val partner = mock[TFCClaimant]

      "if satisfyMinimumEarnings is false and partner.carersAllowance = false" in {

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())
        when(claimant.satisfyMinimumEarnings(any[LocalDate], any[Boolean], any[String])(any[Request[_]], any[HeaderCarrier])).thenReturn(true)
        when(claimant.carersAllowance).thenReturn(false)
        when(partner.satisfyMinimumEarnings(any[LocalDate], any[Boolean], any[String])(any[Request[_]], any[HeaderCarrier])).thenReturn(false)
        when(partner.carersAllowance).thenReturn(false)
        tfc.validHouseholdMinimumEarnings(req, hc) shouldBe false
      }

      "if parent.satisfyMinimumEarnings = true and partner.satisfyMinimumEarnings = true" in {

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())
        when(claimant.satisfyMinimumEarnings(any[LocalDate], any[Boolean], any[String])(any[Request[_]], any[HeaderCarrier])).thenReturn(true)
        when(claimant.carersAllowance).thenReturn(true)
        when(partner.satisfyMinimumEarnings(any[LocalDate], any[Boolean], any[String])(any[Request[_]], any[HeaderCarrier])).thenReturn(true)
        when(partner.carersAllowance).thenReturn(true)
        tfc.validHouseholdMinimumEarnings(req, hc) shouldBe true
      }

      "check if validHouseholdMinimumEarnings is correct if parent.satisfyMinimumEarnings is true and partner.satisfyMinimumEarnings = false" in {

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())
        when(claimant.satisfyMinimumEarnings(any[LocalDate], any[Boolean], any[String])(any[Request[_]], any[HeaderCarrier])).thenReturn(true)
        when(claimant.carersAllowance).thenReturn(true)
        when(partner.satisfyMinimumEarnings(any[LocalDate], any[Boolean], any[String])(any[Request[_]], any[HeaderCarrier])).thenReturn(false)
        when(partner.carersAllowance).thenReturn(true)
        tfc.validHouseholdMinimumEarnings(req, hc) shouldBe true
      }

      "check if validHouseholdMinimumEarnings is correct if parent.satisfyMinimumEarnings is false and partner.satisfyMinimumEarnings = true" in {

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())
        when(claimant.satisfyMinimumEarnings(any[LocalDate], any[Boolean], any[String])(any[Request[_]], any[HeaderCarrier])).thenReturn(false)
        when(claimant.carersAllowance).thenReturn(true)
        when(partner.satisfyMinimumEarnings(any[LocalDate], any[Boolean], any[String])(any[Request[_]], any[HeaderCarrier])).thenReturn(true)
        when(partner.carersAllowance).thenReturn(true)
        tfc.validHouseholdMinimumEarnings(req, hc) shouldBe true
      }

      "check if validHouseholdMinimumEarnings is correct if parent.satisfyMinimumEarnings is false and partner.satisfyMinimumEarnings = false" in {

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())
        when(claimant.satisfyMinimumEarnings(any[LocalDate], any[Boolean], any[String])(any[Request[_]], any[HeaderCarrier])).thenReturn(false)
        when(claimant.carersAllowance).thenReturn(true)
        when(partner.satisfyMinimumEarnings(any[LocalDate], any[Boolean], any[String])(any[Request[_]], any[HeaderCarrier])).thenReturn(false)
        when(partner.carersAllowance).thenReturn(true)
        tfc.validHouseholdMinimumEarnings(req, hc) shouldBe false
      }

      "check if validHouseholdMinimumEarnings is correct if parent.satisfyMinimumEarnings is false" in {

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant), List())
        when(claimant.satisfyMinimumEarnings(any[LocalDate], any[Boolean], any[String])(any[Request[_]], any[HeaderCarrier])).thenReturn(false)

        tfc.validHouseholdMinimumEarnings(req, hc) shouldBe false
      }

      "check if validHouseholdMinimumEarnings is correct if parent.satisfyMinimumEarnings is true" in {

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant), List())
        when(claimant.satisfyMinimumEarnings(any[LocalDate], any[Boolean], any[String])(any[Request[_]], any[HeaderCarrier])).thenReturn(true)

        tfc.validHouseholdMinimumEarnings(req, hc) shouldBe true
      }
    }

    "validMaxEarnings" should {

      val claimant = TFCClaimant(hoursPerWeek = 16.50, disability = TFCDisability(), minimumEarnings =
        TFCMinimumEarnings(), age = None)

      val partner = TFCClaimant(hoursPerWeek = 17.50, isPartner = true, disability = TFCDisability(), minimumEarnings =
        TFCMinimumEarnings(), age = None)


      "return true when its single parent journey when parent max earnings does not exist" in {
        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant), List())

        tfc.validMaxEarnings(req, hc) shouldBe true
      }

      "return true when its single parent journey and max earnings is false" in {
        val parent = claimant.copy(maximumEarnings = Some(false))
        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(parent), List())

        tfc.validMaxEarnings(req, hc) shouldBe true
      }

      //This test is for live service (old one)
      "return true when journey is with partner and max earnings for both parents does not exist" in {
        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())

        tfc.validMaxEarnings(req, hc) shouldBe true
      }

      "return true when journey is with partner and both parents have max earnings as false" in {
        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england",
          List(claimant.copy(maximumEarnings = Some(false)), partner.copy(maximumEarnings = Some(false))), List())

        tfc.validMaxEarnings(req, hc) shouldBe true
      }

      "return true when journey is with partner, parent has false for max earnings and partner max earnings does not exist" in {
        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england",
          List(claimant.copy(maximumEarnings = Some(false)), partner), List())

        tfc.validMaxEarnings(req, hc) shouldBe true
      }

      "return true when journey is with partner, partner has false for max earnings and parent max earnings does not exist" in {
        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england",
          List(claimant, partner.copy(maximumEarnings = Some(false))), List())

        tfc.validMaxEarnings(req, hc) shouldBe true
      }

      "return false when journey is with partner and both parents have max earnings as true" in {
        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england",
          List(claimant.copy(maximumEarnings = Some(true)), partner.copy(maximumEarnings = Some(true))), List())

        tfc.validMaxEarnings(req, hc) shouldBe false
      }

      "return true when journey is with partner, partner has true for max earnings and parent max earnings does not exist" in {
        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england",
          List(claimant, partner.copy(maximumEarnings = Some(true))), List())

        tfc.validMaxEarnings(req, hc) shouldBe false
      }

      "return true when journey is with partner, parent has true for max earnings and partner max earnings does not exist" in {
        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england",
          List(claimant.copy(maximumEarnings = Some(true)), partner), List())

        tfc.validMaxEarnings(req, hc) shouldBe false
      }

    }

  }
}
