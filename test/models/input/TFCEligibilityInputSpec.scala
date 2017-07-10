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

package models.input

import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader
import controllers.FakeCCEligibilityApplication
import models.input.tfc._
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}
import spec.CCConfigSpec
import utils.Periods

class TFCEligibilityInputSpec extends CCConfigSpec with FakeCCEligibilityApplication {

  "TFCInputEligibility" should {

    "Read a valid JSON input and convert to a specific type" in {
        val resource: JsonNode = JsonLoader.fromResource("/json/input/tfc/eligibility_input_test.json")
        val json: JsValue = Json.parse(resource.toString)
        val result = json.validate[TFCEligibilityInput]
        result match {
          case JsSuccess(x, _) => {
            x shouldBe a[TFCEligibilityInput]
            x should not be null

            //TFC model
            x.from shouldBe a[LocalDate]
            x.numberOfPeriods.isInstanceOf[Short] shouldBe true
            x.claimants.isInstanceOf[List[TFCClaimant]] shouldBe true
            x.children.isInstanceOf[List[TFCChild]] shouldBe true
            x.validHouseholdHours shouldBe true
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
          }
          case JsError(e) => throw new IllegalArgumentException(e.toString)
        }
      }

//    "Check for valid household hours true when hoursPerWeek is 16 or more" in {
//      val claimant = TFCClaimant(hoursPerWeek = 16.01, isPartner = false, disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
//      val partner = TFCClaimant(hoursPerWeek = 18.01, isPartner = false, disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
//      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
//      val fromDate = LocalDate.parse("2000-08-27", formatter)
//      val toDate = LocalDate.parse("2000-08-27", formatter)
//
//      val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())
//      tfc.validHouseholdHours shouldBe true
//    }
//
//    "Check for valid household hours false when hoursPerWeek is 16 or more" in {
//      val claimant = TFCClaimant(hoursPerWeek = 15.01, isPartner = false, disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
//      val partner = TFCClaimant(hoursPerWeek = 14.01, isPartner = false, disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
//      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
//      val fromDate = LocalDate.parse("2000-08-27", formatter)
//      val toDate = LocalDate.parse("2000-08-27", formatter)
//
//      val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())
//      tfc.validHouseholdHours shouldBe false
//    }
//
//    "Check for valid household hours partner's carerAllowance when hoursPerWeek for parent is 16 or more" in {
//      val claimant = TFCClaimant(hoursPerWeek = 16.01, isPartner = false, disability = TFCDisability(), carersAllowance = true, minimumEarnings = TFCMinimumEarnings(), age = None)
//      val partner = TFCClaimant(hoursPerWeek = 15.50, isPartner = false, disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
//      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
//      val fromDate = LocalDate.parse("2000-08-27", formatter)
//      val toDate = LocalDate.parse("2000-08-27", formatter)
//
//      val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())
//      tfc.validHouseholdHours shouldBe partner.carersAllowance
//    }
//
//    "Check for valid household hours parent's carerAllowance when hoursPerWeek for partner is 16 or more" in {
//      val claimant = TFCClaimant(hoursPerWeek = 15.50, isPartner = false, disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
//      val partner = TFCClaimant(hoursPerWeek = 16.01, isPartner = false, disability = TFCDisability(), carersAllowance = true, minimumEarnings = TFCMinimumEarnings(), age = None)
//      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
//      val fromDate = LocalDate.parse("2000-08-27", formatter)
//      val toDate = LocalDate.parse("2000-08-27", formatter)
//
//      val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())
//      tfc.validHouseholdHours shouldBe claimant.carersAllowance
//    }
  }

  "Child" should {

    "Determine disability status if child is disabled" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
      val child = TFCChild(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = TFCDisability(disabled = true, severelyDisabled = false))
      child.isDisabled shouldBe true
    }

    "Determine disability status if child is severely disabled" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
      val child = TFCChild(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = TFCDisability(disabled = false, severelyDisabled = true))
      child.isDisabled shouldBe true
    }

    "Determine disability status if child is not disabled and not severely disabled" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
      val child = TFCChild(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = TFCDisability(disabled = false, severelyDisabled = false))
      child.isDisabled shouldBe false
    }

    "Determine disability status if child is disabled and severely disabled" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
      val child = TFCChild(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = TFCDisability(disabled = true, severelyDisabled = true))
      child.isDisabled shouldBe true
    }

    "determine childs Birthday(11th or 16th) where child is not disable" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-08-27", formatter)
      val current = LocalDate.parse("2017-08-01", formatter)
      val child = TFCChild(id = 0, childcareCost = BigDecimal(300.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = TFCDisability(disabled = false, severelyDisabled = false))
      val child11Birthday = child.getChildBirthday(current, "england")
      LocalDate.fromDateFields(child11Birthday) shouldBe LocalDate.parse("2016-08-27", formatter)
    }

    "determine childs Birthday (11th or 16th) where child is disabled" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2000-09-27", formatter)
      val current = LocalDate.parse("2017-08-01", formatter)
      val child = TFCChild(id = 0, childcareCost = BigDecimal(300.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = TFCDisability(disabled = true, severelyDisabled = false))
      val child11Birthday = child.getChildBirthday(current, "england")
      LocalDate.fromDateFields(child11Birthday) shouldBe LocalDate.parse("2016-09-27", formatter)
    }

    "determine end of Week following 1st September Date where child is not disabled" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-08-27", formatter)
      val current = LocalDate.parse("2017-08-01", formatter)
      val child = TFCChild(id = 0, childcareCost = BigDecimal(300.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = TFCDisability(disabled = false, severelyDisabled = false))
      val endWeek1stOfSeptemberDate = child.endWeek1stOfSeptemberDate(current, "england")
      LocalDate.fromDateFields(endWeek1stOfSeptemberDate) shouldBe LocalDate.parse("2016-09-04", formatter)
    }

    "determine end of Week following 1st September Date where child is not disabled, dob after 1st Sept" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-09-12", formatter)
      val current = LocalDate.parse("2017-08-01", formatter)
      val child = TFCChild(id = 0, childcareCost = BigDecimal(300.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = TFCDisability(disabled = false, severelyDisabled = false))
      val endWeek1stOfSeptemberDate = child.endWeek1stOfSeptemberDate(current, "england")
      LocalDate.fromDateFields(endWeek1stOfSeptemberDate) shouldBe LocalDate.parse("2017-09-03", formatter)
    }

    "determine end of Week following 1st September Date where child is disabled" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-08-27", formatter)
      val current = LocalDate.parse("2017-08-01", formatter)
      val child = TFCChild(id = 0, childcareCost = BigDecimal(300.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = TFCDisability(disabled = true, severelyDisabled = false))
      val endWeek1stOfSeptemberDate = child.endWeek1stOfSeptemberDate(current, "england")
      LocalDate.fromDateFields(endWeek1stOfSeptemberDate) shouldBe LocalDate.parse("2021-09-05", formatter)
    }

    "determine end of Week following 1st September Date where child is disabled, dob after 1st Sept" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-09-12", formatter)
      val current = LocalDate.parse("2017-08-01", formatter)
      val child = TFCChild(id = 0, childcareCost = BigDecimal(300.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = TFCDisability(disabled = true, severelyDisabled = false))
      val endWeek1stOfSeptemberDate = child.endWeek1stOfSeptemberDate(current, "england")
      LocalDate.fromDateFields(endWeek1stOfSeptemberDate) shouldBe LocalDate.parse("2022-09-04", formatter)
    }

  }

    "Claimant" should {

      "Check if claimant and partner eligible for minimum earnings rule (minimum 8 hours)" in {
        val claimant = TFCClaimant(hoursPerWeek = 16.01, isPartner = false, disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
        val partner = TFCClaimant(hoursPerWeek = 18.01, isPartner = false, disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val fromDate = LocalDate.parse("2000-08-27", formatter)
        val toDate = LocalDate.parse("2000-08-27", formatter)

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())
        tfc.claimants.head.isWorkingAtLeast16HoursPerWeek(fromDate, tfc.location) shouldBe true
        tfc.claimants.last.isWorkingAtLeast16HoursPerWeek(fromDate, tfc.location) shouldBe true
      }

      "Check if claimant and partner eligible for maximum earnings rule (maximum £100000 earnings)" in {
        val claimant = TFCClaimant(hoursPerWeek = 16.50, isPartner = false, disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
        val partner = TFCClaimant(hoursPerWeek = 16.50, isPartner = false, disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val fromDate = LocalDate.parse("2000-08-27", formatter)
        val toDate = LocalDate.parse("2000-08-27", formatter)

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())

        tfc.claimants.head.isTotalIncomeLessThan100000(fromDate, tfc.location) shouldBe true
        tfc.claimants.last.isTotalIncomeLessThan100000(fromDate, tfc.location) shouldBe true
      }

      "Check if claimant and partner qualify for TFC" in {
        val claimant = TFCClaimant(hoursPerWeek = 16.50, isPartner = false, disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
        val partner = TFCClaimant(hoursPerWeek = 17.50, isPartner = false, disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val fromDate = LocalDate.parse("2000-08-27", formatter)
        val toDate = LocalDate.parse("2000-08-27", formatter)

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())
        tfc.claimants.head.isQualifyingForTFC(fromDate, tfc.location) shouldBe true
        tfc.claimants.last.isQualifyingForTFC(fromDate, tfc.location) shouldBe true
      }

      "claimant and partner both not qualify (claimant fails maximum earnings rule)" in {
        val claimantIncome = Some(TFCIncome(Some(1199999.0),Some(100.0),Some(100.0),None))
        val partnerIncome = Some(TFCIncome(Some(99999.0),Some(100.0),Some(100.0),None))
        val claimant = TFCClaimant(currentIncome = claimantIncome,
          hoursPerWeek = 9.50,
          isPartner = false,
          disability = TFCDisability(),
          carersAllowance = false,
          minimumEarnings = TFCMinimumEarnings(),
          age = None)
        val partner = TFCClaimant(currentIncome = partnerIncome,
          hoursPerWeek = 4.50,
          isPartner = true,
          disability = TFCDisability(),
          carersAllowance = false,
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
        val claimantIncome = Some(TFCIncome(Some(99999.0),Some(100.0),Some(100.0),None))
        val partnerIncome = Some(TFCIncome(Some(9999999.0),Some(100.0),Some(100.0),None))
        val claimant = TFCClaimant(previousIncome = claimantIncome,
          hoursPerWeek = 16.50,
          isPartner = false,
          disability = TFCDisability(),
          carersAllowance = false,
          minimumEarnings = TFCMinimumEarnings(),
          age = None)
        val partner = TFCClaimant(previousIncome = partnerIncome,
          hoursPerWeek = 14.50,
          isPartner = true,
          disability = TFCDisability(),
          carersAllowance = false,
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
        val claimantIncome = Some(TFCIncome(Some(1199999.0),Some(100.0),Some(100.0),None))
        val partnerIncome = Some(TFCIncome(Some(99999.0),Some(100.0),Some(100.0),None))
        val claimant = TFCClaimant(currentIncome = claimantIncome,
          hoursPerWeek = 9.50,
          isPartner = false,
          disability = TFCDisability(),
          carersAllowance = false,
          minimumEarnings = TFCMinimumEarnings(),
          age = None)
        val partner = TFCClaimant(currentIncome = partnerIncome,
          hoursPerWeek = 16.50,
          isPartner = true,
          disability = TFCDisability(),
          carersAllowance = false,
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
        val claimant = TFCClaimant(hoursPerWeek = 0, isPartner = false,  disability = TFCDisability(), carersAllowance = true, minimumEarnings = TFCMinimumEarnings(), age = None)
        val partner = TFCClaimant(hoursPerWeek = 16.50, isPartner = true,  disability = TFCDisability(), carersAllowance = false, minimumEarnings = TFCMinimumEarnings(), age = None)
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val fromDate = LocalDate.parse("2000-08-27", formatter)
        val toDate = LocalDate.parse("2000-08-27", formatter)

        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())
        tfc.claimants.head.isQualifyingForTFC(fromDate, tfc.location) shouldBe true
        tfc.claimants.last.isQualifyingForTFC(fromDate, tfc.location) shouldBe true
      }

    }

}
