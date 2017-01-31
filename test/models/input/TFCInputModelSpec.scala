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
import models.input.tfc.{OtherSupport, _}
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}
import spec.CCSpecConfig
import utils.Periods

class TFCInputModelSpec extends CCSpecConfig with FakeCCEligibilityApplication {

  "TFCInputEligibility" should {

    "Read a valid JSON input and convert to a specific type" in {
        val resource: JsonNode = JsonLoader.fromResource("/json/input/tfc/eligibility_input_test.json")
        val json: JsValue = Json.parse(resource.toString)
        val result = json.validate[Request]
        result match {
          case JsSuccess(x, _) => {
            x shouldBe a[Request]
            x.payload should not be null
            x.payload shouldBe a[Payload]
            x.payload.tfc shouldBe a[TFC]

            //TFC model
            x.payload.tfc.from shouldBe a[LocalDate]
            x.payload.tfc.numberOfPeriods.isInstanceOf[Short] shouldBe true
            x.payload.tfc.claimants.isInstanceOf[List[Claimant]] shouldBe true
            x.payload.tfc.children.isInstanceOf[List[Child]] shouldBe true

            //Claimant model
            x.payload.tfc.claimants.head.liveOrWork.isInstanceOf[Boolean] shouldBe true
            x.payload.tfc.claimants.head.totalIncome shouldBe a[BigDecimal]
            x.payload.tfc.claimants.head.hoursPerWeek.isInstanceOf[Double] shouldBe true
            x.payload.tfc.claimants.head.isPartner.isInstanceOf[Boolean] shouldBe true
            x.payload.tfc.claimants.head.disability shouldBe a[Disability]
            x.payload.tfc.claimants.head.schemesClaiming shouldBe a[SchemesClaiming]
            x.payload.tfc.claimants.head.otherSupport shouldBe a[OtherSupport]

            //Claimant Disability model
            x.payload.tfc.claimants.head.disability.disabled.isInstanceOf[Boolean] shouldBe true
            x.payload.tfc.claimants.head.disability.severelyDisabled.isInstanceOf[Boolean] shouldBe true

            //Schemes claiming model
            x.payload.tfc.claimants.head.schemesClaiming.esc.isInstanceOf[Boolean] shouldBe true
            x.payload.tfc.claimants.head.schemesClaiming.tc.isInstanceOf[Boolean] shouldBe true
            x.payload.tfc.claimants.head.schemesClaiming.uc.isInstanceOf[Boolean] shouldBe true
            x.payload.tfc.claimants.head.schemesClaiming.cg.isInstanceOf[Boolean] shouldBe true

            //Other Support model
            x.payload.tfc.claimants.head.otherSupport.disabilityBenefitsOrAllowances.isInstanceOf[Boolean] shouldBe true
            x.payload.tfc.claimants.head.otherSupport.severeDisabilityBenefitsOrAllowances.isInstanceOf[Boolean] shouldBe true
            x.payload.tfc.claimants.head.otherSupport.incomeBenefitsOrAllowances.isInstanceOf[Boolean] shouldBe true
            x.payload.tfc.claimants.head.otherSupport.carersAllowance.isInstanceOf[Boolean] shouldBe true

            //Child model
            x.payload.tfc.children.head.id.isInstanceOf[Short] shouldBe true
            x.payload.tfc.children.head.name.isInstanceOf[Option[String]] shouldBe true
            x.payload.tfc.children.head.childcareCost shouldBe a[BigDecimal]
            x.payload.tfc.children.head.childcareCostPeriod shouldBe a[Periods.Period]
            x.payload.tfc.children.head.dob shouldBe a[LocalDate]
            x.payload.tfc.children.head.disability shouldBe a[Disability]

            //Child Disability model
            x.payload.tfc.children.head.disability.disabled.isInstanceOf[Boolean] shouldBe true
            x.payload.tfc.children.head.disability.severelyDisabled.isInstanceOf[Boolean] shouldBe true
          }
          case JsError(e) => throw new IllegalArgumentException(e.toString)
        }
      }
  }


  "Child" should {

    "return child's name as a Some(String) if name defined" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2000-08-27", formatter)

      val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      child.name shouldBe Some("Child 1")
    }

    "return child's name as None if name not defined" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2000-08-27", formatter)

      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      child.name shouldBe None
    }

    "Determine disability status if child is disabled" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
      val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false))
      child.isDisabled shouldBe true
    }

    "Determine disability status if child is severely disabled" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = true))
      child.isDisabled shouldBe true
    }

    "Determine disability status if child is not disabled and not severely disabled" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      child.isDisabled shouldBe false
    }

    "Determine disability status if child is disabled and severely disabled" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
      val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = true))
      child.isDisabled shouldBe true
    }

    "determine childs Birthday(11th or 16th) where child is not disable" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-08-27", formatter)
      val current = LocalDate.parse("2017-08-01", formatter)
      val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(300.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val child11Birthday = child.getChildBirthday(current)
      LocalDate.fromDateFields(child11Birthday) shouldBe LocalDate.parse("2016-08-27", formatter)
    }

    "determine childs Birthday (11th or 16th) where child is disabled" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2000-09-27", formatter)
      val current = LocalDate.parse("2017-08-01", formatter)
      val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(300.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false))
      val child11Birthday = child.getChildBirthday(current)
      LocalDate.fromDateFields(child11Birthday) shouldBe LocalDate.parse("2016-09-27", formatter)
    }

    "determine end of Week following 1st September Date where child is not disabled" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-08-27", formatter)
      val current = LocalDate.parse("2017-08-01", formatter)
      val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(300.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val endWeek1stOfSeptemberDate = child.endWeek1stOfSeptemberDate(current)
      LocalDate.fromDateFields(endWeek1stOfSeptemberDate) shouldBe LocalDate.parse("2016-09-04", formatter)
    }

    "determine end of Week following 1st September Date where child is not disabled, dob after 1st Sept" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-09-12", formatter)
      val current = LocalDate.parse("2017-08-01", formatter)
      val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(300.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
      val endWeek1stOfSeptemberDate = child.endWeek1stOfSeptemberDate(current)
      LocalDate.fromDateFields(endWeek1stOfSeptemberDate) shouldBe LocalDate.parse("2017-09-03", formatter)
    }

    "determine end of Week following 1st September Date where child is disabled" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-08-27", formatter)
      val current = LocalDate.parse("2017-08-01", formatter)
      val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(300.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false))
      val endWeek1stOfSeptemberDate = child.endWeek1stOfSeptemberDate(current)
      LocalDate.fromDateFields(endWeek1stOfSeptemberDate) shouldBe LocalDate.parse("2021-09-05", formatter)
    }

    "determine end of Week following 1st September Date where child is disabled, dob after 1st Sept" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dateOfBirth = LocalDate.parse("2005-09-12", formatter)
      val current = LocalDate.parse("2017-08-01", formatter)
      val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(300.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false))
      val endWeek1stOfSeptemberDate = child.endWeek1stOfSeptemberDate(current)
      LocalDate.fromDateFields(endWeek1stOfSeptemberDate) shouldBe LocalDate.parse("2022-09-04", formatter)
    }

  }

    "Claimant" should {
  
      "Determine if claimant live or work in UK" in {
        val claimant = Claimant(liveOrWork = true, isPartner = false,  schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
        claimant.liveOrWork shouldBe true
      }

      "Determine if claimant claiming esc" in {
        val claimant = Claimant(liveOrWork = true, isPartner = false,  schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
        claimant.schemesClaiming.esc shouldBe true
      }

      "Check if claimant and partner eligible for minimum earnings rule (minimum 8 hours)" in {
        val claimant = Claimant(liveOrWork = true, hoursPerWeek = 16.01, isPartner = false,  schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
        val partner = Claimant(liveOrWork = true, hoursPerWeek = 18.01, isPartner = false,  schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val fromDate = LocalDate.parse("2000-08-27", formatter)
        val toDate = LocalDate.parse("2000-08-27", formatter)
        val tfc = TFC(from = fromDate, numberOfPeriods = 1, List(claimant, partner), List())
        tfc.claimants.head.isWorkingAtLeast16HoursPerWeek(fromDate) shouldBe true
        tfc.claimants.last.isWorkingAtLeast16HoursPerWeek(fromDate) shouldBe true
      }

      "Check if claimant and partner eligible for maximum earnings rule (maximum £100000 earnings)" in {
        val claimant = Claimant(liveOrWork = true, hoursPerWeek = 16.50, totalIncome = 99999.99, isPartner = false,  schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
        val partner = Claimant(liveOrWork = true, hoursPerWeek = 16.50, totalIncome = 99500.99, isPartner = false,  schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val fromDate = LocalDate.parse("2000-08-27", formatter)
        val toDate = LocalDate.parse("2000-08-27", formatter)
        val tfc = TFC(from = fromDate, numberOfPeriods = 1, List(claimant, partner), List())

        tfc.claimants.head.isTotalIncomeLessThan100000(fromDate) shouldBe true
        tfc.claimants.last.isTotalIncomeLessThan100000(fromDate) shouldBe true
      }

      "Check if claimant and partner qualify for TFC" in {
        val claimant = Claimant(liveOrWork = true, hoursPerWeek = 16.50, totalIncome = 12000, isPartner = false,  schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
        val partner = Claimant(liveOrWork = true, hoursPerWeek = 17.50, totalIncome = 52000, isPartner = false,  schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val fromDate = LocalDate.parse("2000-08-27", formatter)
        val toDate = LocalDate.parse("2000-08-27", formatter)
        val tfc = TFC(from = fromDate, numberOfPeriods = 1, List(claimant, partner), List())
        tfc.claimants.head.isQualifyingForTFC(fromDate) shouldBe true
        tfc.claimants.last.isQualifyingForTFC(fromDate) shouldBe true
      }

      "claimant and partner both not qualify (claimant fails maximum earnings rule)" in {
        val claimant = Claimant(liveOrWork = true, hoursPerWeek = 9.50, totalIncome = 111001, isPartner = false,  schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
        val partner = Claimant(liveOrWork = true, hoursPerWeek = 4.50, totalIncome = 52000, isPartner = true,  schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val fromDate = LocalDate.parse("2000-08-27", formatter)
        val toDate = LocalDate.parse("2000-08-27", formatter)
        val tfc = TFC(from = fromDate, numberOfPeriods = 1, List(claimant, partner), List())
        tfc.claimants.head.isQualifyingForTFC(fromDate) shouldBe false
        tfc.claimants.last.isQualifyingForTFC(fromDate) shouldBe true
      }

      "claimant qualify and partner not qualify (partner fails maximum earnings rule)" in {
        val claimant = Claimant(liveOrWork = true, hoursPerWeek = 16.50, totalIncome = 12000, isPartner = false,  schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
        val partner = Claimant(liveOrWork = true, hoursPerWeek = 14.50, totalIncome = 111001, isPartner = true,  schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val fromDate = LocalDate.parse("2000-08-27", formatter)
        val toDate = LocalDate.parse("2000-08-27", formatter)
        val tfc = TFC(from = fromDate, numberOfPeriods = 1, List(claimant, partner), List())
        tfc.claimants.head.isQualifyingForTFC(fromDate) shouldBe true
        tfc.claimants.last.isQualifyingForTFC(fromDate) shouldBe false
      }

      "claimant not qualify and partner qualify (claimant fails live or work)" in {
        val claimant = Claimant(liveOrWork = false, hoursPerWeek = 9.50, totalIncome = 12000, isPartner = false,  schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
        val partner = Claimant(liveOrWork = true, hoursPerWeek = 16.50, totalIncome = 59000, isPartner = true,  schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val fromDate = LocalDate.parse("2000-08-27", formatter)
        val toDate = LocalDate.parse("2000-08-27", formatter)
        val tfc = TFC(from = fromDate, numberOfPeriods = 1, List(claimant, partner), List())
        tfc.claimants.head.isQualifyingForTFC(fromDate) shouldBe false
        tfc.claimants.last.isQualifyingForTFC(fromDate) shouldBe true
      }

      "claimant qualify if carer's allowance is selected with zero hours worked" in {
        val claimant = Claimant(liveOrWork = true, hoursPerWeek = 0, totalIncome = 12000, isPartner = false,  schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport(carersAllowance=true))
        val partner = Claimant(liveOrWork = true, hoursPerWeek = 16.50, totalIncome = 59000, isPartner = true,  schemesClaiming = SchemesClaiming(esc = true), disability = Disability(), otherSupport = OtherSupport())
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val fromDate = LocalDate.parse("2000-08-27", formatter)
        val toDate = LocalDate.parse("2000-08-27", formatter)
        val tfc = TFC(from = fromDate, numberOfPeriods = 1, List(claimant, partner), List())
        tfc.claimants.head.isQualifyingForTFC(fromDate) shouldBe true
        tfc.claimants.last.isQualifyingForTFC(fromDate) shouldBe true
      }

    }

}
