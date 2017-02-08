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
import models.input.tc._
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}
import spec.CCSpecConfig
import utils.Periods

class TCInputModelSpec extends CCSpecConfig with FakeCCEligibilityApplication {

  "TCInputEligibility" should {

    "read a valid JSON input and convert to a specific type" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/input/tc/eligibility_input_test.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      result match {
        case JsSuccess(x, _) => {
          x shouldBe a[Request]
          x.payload should not be null
          x.payload.taxYears.head.from shouldBe a[LocalDate]
          x.payload.taxYears.head.until shouldBe a[LocalDate]
          x.payload.taxYears.head.claimants.isInstanceOf[List[Claimant]] shouldBe true
          x.payload.taxYears.head.children.isInstanceOf[List[Child]] shouldBe true

          //Claimant model
          x.payload.taxYears.head.claimants.head.hours.isInstanceOf[Double] shouldBe true
          x.payload.taxYears.head.claimants.head.liveOrWork.isInstanceOf[Boolean] shouldBe true
          x.payload.taxYears.head.claimants.head.isPartner.isInstanceOf[Boolean] shouldBe true
          x.payload.taxYears.head.claimants.head.disability shouldBe a[Disability]
          x.payload.taxYears.head.claimants.head.schemesClaiming shouldBe a[SchemesClaiming]

          //Disability model
          x.payload.taxYears.head.claimants.head.disability.disabled.isInstanceOf[Boolean] shouldBe true
          x.payload.taxYears.head.claimants.head.disability.severelyDisabled.isInstanceOf[Boolean] shouldBe true
          x.payload.taxYears.head.claimants.head.disability.incapacitated.isInstanceOf[Boolean] shouldBe true

          //Schemes claiming model
          x.payload.taxYears.head.claimants.head.schemesClaiming.tc.isInstanceOf[Boolean] shouldBe true
          x.payload.taxYears.head.claimants.head.schemesClaiming.tfc.isInstanceOf[Boolean] shouldBe true
          x.payload.taxYears.head.claimants.head.schemesClaiming.esc.isInstanceOf[Boolean] shouldBe true

          //Children model
          x.payload.taxYears.head.children.head.id.isInstanceOf[Short] shouldBe true
          x.payload.taxYears.head.children.head.name.isInstanceOf[Option[String]] shouldBe true
          x.payload.taxYears.head.children.head.childcareCost shouldBe a[BigDecimal]
          x.payload.taxYears.head.children.head.childcareCostPeriod shouldBe a[Periods.Period]
          x.payload.taxYears.head.children.head.dob shouldBe a[LocalDate]
          x.payload.taxYears.head.children.head.disability shouldBe a[Disability]
          x.payload.taxYears.head.children.head.education.isInstanceOf[Option[Education]] shouldBe true

          // Education model
          x.payload.taxYears.head.children.head.education.get.inEducation.isInstanceOf[Boolean] shouldBe true
          x.payload.taxYears.head.children.head.education.get.startDate shouldBe a[LocalDate]
        }
        case _ => throw new Exception
      }
    }

    "Child" should {

      "return child's name as a Some(String) if name defined" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2000-08-27", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.name shouldBe Some("Child 1")
      }

      "return None name defined not defined" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2000-08-27", formatter)

        val child = Child(id = 0, name = None, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.name shouldBe None
      }

      "(Non education < 16, turning 16) flag if the child is going to turn 16 before the 1st September" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
        val today = LocalDate.parse("2016-08-01", formatter)
        val endDate = LocalDate.parse("2017-04-06", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)

        val (x, y) = child.isTurning16Before1September(periodStart = today, endDate)
        x shouldBe true
        y shouldBe LocalDate.parse("2016-09-01", formatter)
      }

      "(Non education < 16, not turning 16) flag if the child is going to turn 16 before the 1st September" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2007-08-27", formatter)
        val today = LocalDate.parse("2016-07-31", formatter)
        val endDate = LocalDate.parse("2017-04-06", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)

        val (x, y) = child.isTurning16Before1September(periodStart = today, endDate)
        x shouldBe false
        y shouldBe LocalDate.parse("2016-09-01", formatter)
      }

      "(In education > 16) flag if the child is going to turn 16 before the 1st September" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("1998-08-27", formatter)
        val today = LocalDate.parse("2016-07-31", formatter)
        val educationStartDate = LocalDate.parse("2014-09-01", formatter)
        val endDate = LocalDate.parse("2017-04-06", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))

        val (x, y) = child.isTurning16Before1September(periodStart = today, endDate)
        x shouldBe false
        y shouldBe LocalDate.parse("2016-09-01", formatter)
      }

      "(Non education) flag if the child is going to turn 16 on the 1st September" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2000-08-31", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)
        val endDate = LocalDate.parse("2017-04-06", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)

        val (x, y) = child.isTurning16Before1September(periodStart = today, endDate)
        x shouldBe true
        y shouldBe LocalDate.parse("2016-09-01", formatter)
      }

      "(Non education < 15, turning 15) flag if the child is going to turn 15 before the 1st September" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-27", formatter)
        val today = LocalDate.parse("2016-08-01", formatter)
        val endDate = LocalDate.parse("2017-04-06", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)

        val (x, y) = child.isTurning15Before1September(periodStart = today, endDate)
        x shouldBe true
        y shouldBe LocalDate.parse("2016-09-01", formatter)
      }

      "(Non education < 15, not turning 15) flag if the child is going to turn 15 before the 1st September" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2008-08-27", formatter)
        val today = LocalDate.parse("2016-07-31", formatter)
        val endDate = LocalDate.parse("2017-04-06", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)

        val (x, y) = child.isTurning15Before1September(periodStart = today, endDate)
        x shouldBe false
        y shouldBe LocalDate.parse("2016-09-01", formatter)
      }

      "(In education > 15) flag if the child is going to turn 15 before the 1st September" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("1999-08-27", formatter)
        val today = LocalDate.parse("2016-07-31", formatter)
        val educationStartDate = LocalDate.parse("2015-09-01", formatter)
        val endDate = LocalDate.parse("2017-04-06", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))

        val (x, y) = child.isTurning15Before1September(periodStart = today, endDate)
        x shouldBe false
        y shouldBe LocalDate.parse("2016-09-01", formatter)
      }

      "(Non education) flag if the child is going to turn 15 on the 1st September" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)
        val endDate = LocalDate.parse("2017-04-06", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)

        val (x, y) = child.isTurning15Before1September(periodStart = today, endDate)
        x shouldBe true
        y shouldBe LocalDate.parse("2016-09-01", formatter)
      }

      "(child turning 20 in current tax year - after ty start date) determine if a tax year needs to split the day before the childs 20th birthday" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("1997-06-05", formatter)
        val taxYearStart = LocalDate.parse("2017-04-06", formatter)
        val taxYearEnd = LocalDate.parse("2018-04-06", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)

        val taxYear = TaxYear(from = taxYearStart, until = taxYearEnd, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List())
        val (x, y) = child.isBeingBornInTaxYear(taxYear = taxYear)
        x shouldBe false
        y shouldBe LocalDate.parse("1997-06-05", formatter)
      }

      "(child turning 20 in current tax year - before ty start date) determine if a tax year needs to split the day before the childs 20th birthday" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("1997-03-05", formatter)
        val taxYearStart = LocalDate.parse("2017-04-06", formatter)
        val taxYearEnd = LocalDate.parse("2018-04-06", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)

        val taxYear = TaxYear(from = taxYearStart, until = taxYearEnd, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List())
        val (x, y) = child.isBeingBornInTaxYear(taxYear = taxYear)
        x shouldBe false
        y shouldBe LocalDate.parse("1997-03-05", formatter)
      }

      "(child turning 20 in current tax year - on ty start date) determine if a tax year needs to split the day before the childs 20th birthday" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("1997-04-06", formatter)
        val taxYearStart = LocalDate.parse("2017-04-06", formatter)
        val taxYearEnd = LocalDate.parse("2018-04-06", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)

        val taxYear = TaxYear(from = taxYearStart, until = taxYearEnd, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List())

        val (x, y) = child.isBeingBornInTaxYear(taxYear = taxYear)
        x shouldBe false
        y shouldBe LocalDate.parse("1997-04-06", formatter)
      }

      "(child is not turning 20 in current tax year) determine if a tax year needs to split the day before the childs 20th birthday" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2006-06-05", formatter)
        val taxYearStart = LocalDate.parse("2017-04-06", formatter)
        val taxYearEnd = LocalDate.parse("2018-04-06", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)

        val taxYear = TaxYear(from = taxYearStart, until = taxYearEnd, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List())

        val (x, y) = child.isBeingBornInTaxYear(taxYear = taxYear)
        x shouldBe false
        y shouldBe LocalDate.parse("2006-06-05", formatter)
      }

      "(child is not turning 20 (September 1st) in current tax year) determine if a tax year needs to split the day before the childs 20th birthday" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2006-09-01", formatter)
        val taxYearStart = LocalDate.parse("2017-04-06", formatter)
        val taxYearEnd = LocalDate.parse("2018-04-06", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)

        val taxYear = TaxYear(from = taxYearStart, until = taxYearEnd, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List())

        val (x, y) = child.isBeingBornInTaxYear(taxYear = taxYear)
        x shouldBe false
        y shouldBe LocalDate.parse("2006-09-01", formatter)
      }

      "(child is being born in current tax year, before september 1st) determine if child is yet to be born" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2017-08-30", formatter)
        val taxYearStart = LocalDate.parse("2017-04-06", formatter)
        val taxYearEnd = LocalDate.parse("2018-04-06", formatter)
        val secondPeriodStart = LocalDate.parse("2018-02-06", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(0.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)

        val taxYear = TaxYear(from = taxYearStart, until = taxYearEnd, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List())

        val (x, y) = child.isBeingBornInTaxYear(taxYear = taxYear)
        x shouldBe true
        y shouldBe LocalDate.parse("2017-08-30", formatter)

        child.isChild(taxYearStart) shouldBe false
        // if a split occurred again then we would recheck the child for a later date
        child.isChild(secondPeriodStart) shouldBe true
      }

      "(child is being born in current tax year, after september 1st) determine if child is yet to be born" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2017-09-02", formatter)
        val taxYearStart = LocalDate.parse("2017-04-06", formatter)
        val taxYearEnd = LocalDate.parse("2018-04-06", formatter)
        val secondPeriodStart = LocalDate.parse("2018-02-06", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(0.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)

        val taxYear = TaxYear(from = taxYearStart, until = taxYearEnd, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List())

        val (x, y) = child.isBeingBornInTaxYear(taxYear = taxYear)
        x shouldBe true
        y shouldBe LocalDate.parse("2017-09-02", formatter)

        child.isChild(taxYearStart) shouldBe false
        // if a split occurred again then we would recheck the child for a later date
        child.isChild(secondPeriodStart) shouldBe true
      }

      "(child is born prior to tax year start date) determine if child is yet to be born" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2017-03-01", formatter)
        val taxYearStart = LocalDate.parse("2017-04-06", formatter)
        val taxYearEnd = LocalDate.parse("2018-04-06", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(0.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)

        val taxYear = TaxYear(from = taxYearStart, until = taxYearEnd, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List())

        val (x, y) = child.isBeingBornInTaxYear(taxYear = taxYear)
        x shouldBe false
        y shouldBe LocalDate.parse("2017-03-01", formatter)

        child.isChild(taxYearStart) shouldBe true
      }

      "(child is born on tax year start date) determine if child is yet to be born" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2017-04-06", formatter)
        val taxYearStart = LocalDate.parse("2017-04-06", formatter)
        val taxYearEnd = LocalDate.parse("2018-04-06", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(0.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)

        val taxYear = TaxYear(from = taxYearStart, until = taxYearEnd, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List())

        val (x, y) = child.isBeingBornInTaxYear(taxYear = taxYear)
        x shouldBe false
        y shouldBe LocalDate.parse("2017-04-06", formatter)

        child.isChild(taxYearStart) shouldBe true
      }

      "(child is born on tax year end date) determine if child is yet to be born" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2018-04-06", formatter)
        val taxYearStart = LocalDate.parse("2017-04-06", formatter)
        val taxYearEnd = LocalDate.parse("2018-04-06", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(0.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)

        val taxYear = TaxYear(from = taxYearStart, until = taxYearEnd, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List())

        val (x, y) = child.isBeingBornInTaxYear(taxYear = taxYear)
        x shouldBe false
        y shouldBe LocalDate.parse("2018-04-06", formatter)

        child.isChild(taxYearStart) shouldBe false
      }

      "(child is born after the tax year) determine if child is yet to be born" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2018-09-01", formatter)
        val taxYearStart = LocalDate.parse("2017-04-06", formatter)
        val taxYearEnd = LocalDate.parse("2018-04-06", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(0.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)

        val taxYear = TaxYear(from = taxYearStart, until = taxYearEnd, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List())

        val (x, y) = child.isBeingBornInTaxYear(taxYear = taxYear)
        x shouldBe false
        y shouldBe LocalDate.parse("2018-09-01", formatter)
        child.isChild(taxYearStart) shouldBe false
        child.isChild(taxYearEnd) shouldBe false
      }

      "(Child < 16) determine if child gets child element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2004-08-31", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.isChild(today) shouldBe true
      }

      "(Child > 16) determine if child gets child element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("1996-08-31", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.isChild(today) shouldBe false
      }

      "(Child is 16 and is 31august) determine if child gets child element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2000-08-31", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.isChild(today) shouldBe true
      }

      "(Child is 16 and is 1st September) determine if child gets child element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2000-08-31", formatter)
        val today = LocalDate.parse("2016-09-01", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.isChild(today) shouldBe false
      }

      "(Young adult < 16) determine if get young adult element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("20010-08-31", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.getsYoungAdultElement(today) shouldBe false
      }

      "(Young adult > 16 && < 20 and in education) determine if get young adult element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("1999-08-31", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("1999-09-05", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        child.getsYoungAdultElement(today) shouldBe true
      }

      "(Young adult > 16 && < 20 and not in education) determine if get young adult element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("1999-08-31", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.getsYoungAdultElement(today) shouldBe false
      }

      "(is inEducation) determine if the child is in education" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("1993-08-31", formatter)
        val educationStartDate = LocalDate.parse("1999-09-05", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        child.inEducation(today) shouldBe true
      }

      "(not inEducation - provided) determine if the child is in education" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("1993-08-31", formatter)
        val educationStartDate = LocalDate.parse("1999-09-05", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = Some(Education(inEducation = false, startDate = educationStartDate)))
        child.inEducation(today) shouldBe false
      }

      "(not inEducation - not provided) determine if the child is in education" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("1993-08-31", formatter)
        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        val today = LocalDate.parse("2016-08-31", formatter)

        child.inEducation(today) shouldBe false
      }

      "(birthday is the same day) determine the child's age" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("1993-08-31", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.age(now = today) shouldBe 23
      }

      "(birthday is after today) determine the child's age" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("1993-08-31", formatter)
        val today = LocalDate.parse("2016-07-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.age(now = today) shouldBe 22
      }

      "(birthday is before today) determine the child's age" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("1993-08-31", formatter)
        val today = LocalDate.parse("2016-09-27", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.age(now = today) shouldBe 23
      }

      "(birthday is after today - not born yet) determine the child's age" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2017-05-20", formatter)
        val today = LocalDate.parse("2016-09-27", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.age(now = today) shouldBe -1
      }

      "(birthday is the same day - child is being born on the same day) determine the child's age" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2017-05-20", formatter)
        val today = LocalDate.parse("2017-05-20", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.age(now = today) shouldBe 0
      }

      "(birthday is after today - same month) determine the child's age" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-09-27", formatter)
        val today = LocalDate.parse("2016-09-20", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.age(now = today) shouldBe 14
      }

      "(20 years old) determine child's birthday for an age" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-09-27", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.childsBirthdayDateForAge(years = 20) shouldBe LocalDate.parse("2021-09-27", formatter).toDate
      }

      "(15 years old) determine child's birthday for an age" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-09-27", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.childsBirthdayDateForAge(years = 15) shouldBe LocalDate.parse("2016-09-27", formatter).toDate
      }

      "(5 years old) determine child's birthday for an age" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-09-27", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.childsBirthdayDateForAge(years = 5) shouldBe LocalDate.parse("2006-09-27", formatter).toDate
      }

      "(Child < 0) determine if child gets child element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2017-08-31", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.isChild(today) shouldBe false
      }


      "(Child is 16 and is 31st August) determine if child gets child element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2000-08-31", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.isChild(today) shouldBe true
      }

      "(Young adult > 16 && < 20 and in education - started education before 19th birthday) determine if get young adult element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2020-08-31", formatter)
        val educationStartDate = LocalDate.parse("2019-09-05", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false),education = Some(Education(inEducation = true, startDate = educationStartDate)))
        child.getsYoungAdultElement(today) shouldBe true
      }

      "(Young adult 19 years old and in education - started education before 19th birthday) determine if get young adult element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2020-08-20", formatter)
        val educationStartDate = LocalDate.parse("2019-09-05", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false),education = Some(Education(inEducation = true, startDate = educationStartDate)))
        child.getsYoungAdultElement(today) shouldBe true
      }

      "(Young adult turning 20 years old and in education - started education before 19th birthday) determine if get young adult element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2021-08-30", formatter)
        val educationStartDate = LocalDate.parse("2019-09-05", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false),education = Some(Education(inEducation = true, startDate = educationStartDate)))
        child.getsYoungAdultElement(today) shouldBe true
      }

      "(Young adult 19 years old and in education - started education on 19th birthday) determine if get young adult element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2020-08-31", formatter)
        val educationStartDate = LocalDate.parse("2020-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false),education = Some(Education(inEducation = true, startDate = educationStartDate)))
        child.getsYoungAdultElement(today) shouldBe false
      }

      "(Young adult > 16 && < 20 and in education - started education after 19th birthday) determine if get young adult element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2020-09-10", formatter)
        val educationStartDate = LocalDate.parse("2020-09-05", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false),education = Some(Education(inEducation = true, startDate = educationStartDate)))
        child.getsYoungAdultElement(today) shouldBe false
      }

      "(Young adult > 16 && > 20) determine if get young adult element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("1993-08-31", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.getsYoungAdultElement(today) shouldBe false
      }

      "(Child < 16, disabled) determine if gets disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2004-08-31", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        child.getsDisabilityElement(today) shouldBe true
      }

      "(Child < 16, non disabled) determine if gets disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2004-08-31", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.getsDisabilityElement(today) shouldBe false
      }

      "(Child < 0, disabled) determine if gets disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2017-08-31", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        child.getsDisabilityElement(today) shouldBe false
      }

      "(Child < 0, non disabled) determine if gets disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2017-08-31", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.getsDisabilityElement(today) shouldBe false
      }

      "(Young adult > 16 && < 20, in education started before 19th birthday, is disabled) determine if gets disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2020-08-31", formatter)
        val educationStartDate = LocalDate.parse("2019-09-05", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false),education = Some(Education(inEducation = true, startDate = educationStartDate)))
        child.getsDisabilityElement(today) shouldBe true
      }

      "(Young adult > 16 && < 20, in education started before 19th birthday, is not disabled) determine if gets disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2020-08-31", formatter)
        val educationStartDate = LocalDate.parse("2019-09-05", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false),education = Some(Education(inEducation = true, startDate = educationStartDate)))
        child.getsDisabilityElement(today) shouldBe false
      }

      "(Young adult > 16 && < 20, in education started after 19th birthday, is disabled) determine if gets disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2020-09-10", formatter)
        val educationStartDate = LocalDate.parse("2020-09-05", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false),education = Some(Education(inEducation = true, startDate = educationStartDate)))
        child.getsDisabilityElement(today) shouldBe false
      }

      "(Young adult > 16 && < 20, in education started after 19th birthday, is not disabled) determine if gets disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2020-09-10", formatter)
        val educationStartDate = LocalDate.parse("2020-09-05", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false),education = Some(Education(inEducation = true, startDate = educationStartDate)))
        child.getsDisabilityElement(today) shouldBe false
      }

      "(Young adult > 16 && < 20, not in education, is disabled) determine if gets disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2020-09-10", formatter)
        val educationStartDate = LocalDate.parse("2020-09-05", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false),education = Some(Education(inEducation = true, startDate = educationStartDate)))
        child.getsDisabilityElement(today) shouldBe false
      }

      "(Young adult > 16 && < 20, not in education, is not disabled) determine if gets disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2020-09-10", formatter)
        val educationStartDate = LocalDate.parse("2020-09-05", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false),education = Some(Education(inEducation = true, startDate = educationStartDate)))
        child.getsDisabilityElement(today) shouldBe false
      }

      "(Child < 16, severe disabled) determine if gets severe disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2004-08-31", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = true), education = None)
        child.getsSevereDisabilityElement(today) shouldBe true
      }

      "(Child < 16, severe disabled, non disabled) determine if gets severe disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2004-08-31", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = true), education = None)
        child.getsSevereDisabilityElement(today) shouldBe true
      }

      "(Child < 16, non severe disabled) determine if gets severe disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2004-08-31", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.getsSevereDisabilityElement(today) shouldBe false
      }

      "(Child < 0, severe disabled) determine if gets severe disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2017-08-31", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = true), education = None)
        child.getsSevereDisabilityElement(today) shouldBe false
      }

      "(Child < 0, non severe disabled) determine if gets severe disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2017-08-31", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        child.getsSevereDisabilityElement(today) shouldBe false
      }

      "(Young adult > 16 && < 20, in education started before 19th birthday, is severe disabled) determine if gets severe disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2020-08-31", formatter)
        val educationStartDate = LocalDate.parse("2019-09-05", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = true),education = Some(Education(inEducation = true, startDate = educationStartDate)))
        child.getsSevereDisabilityElement(today) shouldBe true
      }

      "(Young adult > 16 && < 20, in education started before 19th birthday, is severe disabled, is non disabled) determine if gets severe disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2020-08-31", formatter)
        val educationStartDate = LocalDate.parse("2019-09-05", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = true),education = Some(Education(inEducation = true, startDate = educationStartDate)))
        child.getsSevereDisabilityElement(today) shouldBe true
      }

      "(Young adult > 16 && < 20, in education started before 19th birthday, is not severe disabled) determine if gets severe disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2020-08-31", formatter)
        val educationStartDate = LocalDate.parse("2019-09-05", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false),education = Some(Education(inEducation = true, startDate = educationStartDate)))
        child.getsSevereDisabilityElement(today) shouldBe false
      }

      "(Young adult > 16 && < 20, in education started after 19th birthday, is severe disabled) determine if gets severe disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2020-09-10", formatter)
        val educationStartDate = LocalDate.parse("2020-09-05", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = true),education = Some(Education(inEducation = true, startDate = educationStartDate)))
        child.getsSevereDisabilityElement(today) shouldBe false
      }

      "(Young adult > 16 && < 20, in education started after 19th birthday, is not severe disabled) determine if gets severe disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2020-09-10", formatter)
        val educationStartDate = LocalDate.parse("2020-09-05", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false),education = Some(Education(inEducation = true, startDate = educationStartDate)))
        child.getsSevereDisabilityElement(today) shouldBe false
      }

      "(Young adult > 16 && < 20, not in education, is severe disabled) determine if gets severe disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2020-09-10", formatter)
        val educationStartDate = LocalDate.parse("2020-09-05", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = true),education = Some(Education(inEducation = true, startDate = educationStartDate)))
        child.getsSevereDisabilityElement(today) shouldBe false
      }

      "(Young adult > 16 && < 20, not in education, is not severe disabled) determine if gets severe disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2020-09-10", formatter)
        val educationStartDate = LocalDate.parse("2020-09-05", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false),education = Some(Education(inEducation = true, startDate = educationStartDate)))
        child.getsSevereDisabilityElement(today) shouldBe false
      }

      "(Child < 15, non disabled) determine if gets childcare element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2015-09-10", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false),education = None)
        child.getsChildcareElement(today) shouldBe true
      }

      "(Child < 15, disabled) determine if gets childcare element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2015-09-10", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false),education = None)
        child.getsChildcareElement(today) shouldBe true
      }

      "(Child > 15 && < 16, disabled) determine if gets childcare element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2016-09-10", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false),education = None)
        child.getsChildcareElement(today) shouldBe true
      }

      "(Child > 15 && < 16, non disabled) determine if gets childcare element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2016-09-10", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false),education = None)
        child.getsChildcareElement(today) shouldBe false
      }

      "(Child > 16) determine if gets childcare element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2018-09-10", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false),education = None)
        child.getsChildcareElement(today) shouldBe false
      }

      "(Child, no childcare cost) determine if gets childcare element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2013-09-10", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(0.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false),education = None)
        child.getsChildcareElement(today) shouldBe false
      }

      "(Child, 15 on same day as period start - 31st August) determine if gets childcare element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(0.10), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false),education = None)
        child.getsChildcareElement(today) shouldBe true
      }

      "(Child, 15 on same day as period start - 1st September) determine if gets childcare element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-08-31", formatter)
        val today = LocalDate.parse("2016-09-01", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(0.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false),education = None)
        child.getsChildcareElement(today) shouldBe false
      }

      "(Child < 0) determine if gets childcare element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2016-08-31", formatter)
        val today = LocalDate.parse("2015-09-10", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false),education = None)
        child.getsChildcareElement(today) shouldBe false
      }
    }

    "TaxYear" should {

      "(claimant with a child < 15, non disabled) determine if household has a child or young person" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2006-08-31", formatter)
        val periodStart = LocalDate.parse("2021-08-30", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List(claimant))
        taxYear.householdHasChildOrYoungPerson(periodStart) shouldBe true
      }

      "(claimant with a child < 16, disabled) determine if household has a child or young person" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2005-08-31", formatter)
        val periodStart = LocalDate.parse("2021-08-30", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00,disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List(claimant))
        taxYear.householdHasChildOrYoungPerson(periodStart) shouldBe true
      }

      "(claimant with a young person > 16, non-disabled) determine if household has a child or young person" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2004-08-31", formatter)
        val periodStart = LocalDate.parse("2021-08-30", formatter)
        val educationStartDate = LocalDate.parse("2019-09-05", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List(claimant))
        taxYear.householdHasChildOrYoungPerson(periodStart) shouldBe true
      }

      "(claimant with a person > 16, disabled, doesn't qualify) determine if household has a child or young person" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2003-08-31", formatter)
        val periodStart = LocalDate.parse("2021-08-30", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List(claimant))
        taxYear.householdHasChildOrYoungPerson(periodStart) shouldBe false
      }

      "(claimant with a person < 0, disabled, doesn't qualify) determine if household has a child or young person" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2022-08-31", formatter)
        val periodStart = LocalDate.parse("2021-08-30", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List(claimant))
        taxYear.householdHasChildOrYoungPerson(periodStart) shouldBe false
      }

      "(two claimants) determine total household working hours" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 20.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 20.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.getTotalHouseholdWorkingHours shouldBe 40.00
      }

      "(two claimants) (one doesn't work) determine total household working hours" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 20.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.getTotalHouseholdWorkingHours shouldBe 20.00
      }

      "(two claimants) (both doesn't work) determine total household working hours" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.getTotalHouseholdWorkingHours shouldBe 0.00
      }

      "(one claimant) (doesn't work) determine total household working hours" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1))

        taxYear.getTotalHouseholdWorkingHours shouldBe 0.00
      }

      "(one claimant) (works) determine total household working hours" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 0.02, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1))

        taxYear.getTotalHouseholdWorkingHours shouldBe 0.02
      }

      "(two claimants) determine if we have a couple" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.isCouple shouldBe true
      }

      "(single claimant) determine if we have a couple" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1))

        taxYear.isCouple shouldBe false
      }

      "(single claimant, < 16 hours, disabled)(non qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 10.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe false
      }

      "(single claimant, < 16 hours, non disabled)(non qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 10.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe false
      }

      "(single claimant, < 16 hours, incapacitated)(non qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 10.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe false
      }

      "(single claimant, > 16 hours, disabled)(qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 50.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe true
      }

      "(single claimant, > 16 hours, non disabled)(qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 50.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe true
      }

      "(single claimant, > 16 hours, incapacitated)(non qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 50.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe false
      }

      "(two claimants, (> 16 hours, < 16 hours)(disabled, non disabled))(both qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 18.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 14.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe false
      }

      "(two claimants, (< 16 hours, < 16 hours)(disabled, disabled))(both non - qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 14.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 1.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe false
      }

      "(two claimants, (< 16 hours, < 16 hours)(incapacitated, incapacitated))(both non - qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 0.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 0.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe false
      }

      "(two claimants, (< 16 hours, < 16 hours)(non disabled, non disabled))(both non qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 14.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 14.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe false
      }

      "(two claimants, (16 hours, < 16 hours)(incapaciated, disabled))(both non - qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 0.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe false
      }

      "(two claimants, (< 16 hours, > 16 hours)(non disabled, non disabled))(one qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 0.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe false
      }

      "(two claimants, (< 16 hours, > 16 hours)(incapacitated, non disabled))(one qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 0.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe true
      }

      "(two claimants, (< 16 hours, > 16 hours)(disabled, incapacitated))(both non qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 0.00, disability = Disability(disabled = true, severelyDisabled = false, incapacitated = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe false
      }

      "(two claimants, (> 16 hours, > 16 hours)(non disabled, disabled))(one qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe true
      }

      "(two claimants, (> 16 hours, > 16 hours)(non disabled, incapacitated))(one qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe true
      }

      "(two claimants, (> 16 hours, > 16 hours)(disabled, incapacitated))(one qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe true
      }

      "(two claimants, (> 16 hours, > 16 hours)(disabled, disabled))(one qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe true
      }

      "(two claimants, (> 16 hours, > 16 hours)(incapacitated, incapacitated))(none qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe false
      }

      "(two claimants, (> 16 hours, > 16 hours)(non disabled, non disabled))(one qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe true
      }

      "(two claimants, (< 16 hours, < 16 hours)(disabled, disabled))(one qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 15.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 15.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe false
      }

      "(two claimants, (< 16 hours, < 16 hours)(disabled, incapacitated))(none qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 15.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 15.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe false
      }

      "(two claimants, (> 16 hours, non live or work)(non disabled, non disabled))(one qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = false, isPartner = true, hours = 20.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe false
      }

      "(two claimants, (> 16 hours, non live or work)(non disabled, incapacitated))(one qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = false, isPartner = true, hours = 20.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe false
      }

      "(two claimants, (> 16 hours, non live or work)(incapacitated, non disabled))(one qualifying) determine if household claimants qualify for childcare element" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = false, isPartner = true, hours = 20.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant1, claimant2))

        taxYear.claimantsGetChildcareElement(taxYear.from) shouldBe false
      }

      "(qualifying claimant and qualifying child) determine if household gets childcare element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2004-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List(claimant))
        taxYear.householdGetsChildcareElement(periodStart) shouldBe true
      }

      "(qualifying claimant and qualifying child) (two children, 1 qualifying, 1 not qualifying) determine if household gets childcare element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2004-08-31", formatter)
        val dateOfBirth2 = LocalDate.parse("1998-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val child2 = Child(id = 0, name = Some("Child 2"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth2, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1, child2), claimants = List(claimant))
        taxYear.householdGetsChildcareElement(periodStart) shouldBe true
      }

      "(non qualifying claimant and non qualifying child) determine if household gets childcare element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("1991-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 15.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List(claimant))
        taxYear.householdGetsChildcareElement(periodStart) shouldBe false
      }

      "(qualifying claimant and non qualifying child) determine if household gets childcare element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("1991-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 19.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List(claimant))
        taxYear.householdGetsChildcareElement(periodStart) shouldBe false
      }

      "(non qualifying claimant and qualifying child) determine if household gets childcare element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2014-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 1.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List(claimant))
        taxYear.householdGetsChildcareElement(periodStart) shouldBe false
      }

      "(Scenario 25 - C1 Incapacitated, C2 Working > 16 hours) determine if household gets childcare element" in {
        val resource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_25.json")
        val json: JsValue = Json.parse(resource.toString)
        val result = json.validate[Request]

        result match {
          case JsSuccess(x, _) =>
            x shouldBe a[Request]

            val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
            val today = LocalDate.parse("2016-09-10", formatter)

            x.payload.taxYears.head.householdGetsChildcareElement(today) shouldBe true

          case JsError(e) =>
            throw new RuntimeException(e.toList.toString())
        }
      }

      "(Scenario 26 - C1 Incapacitated, C2 Incapacitated) determine if household gets childcare element" in {
        val resource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_26.json")
        val json: JsValue = Json.parse(resource.toString)
        val result = json.validate[Request]

        result match {
          case JsSuccess(x, _) =>
            x shouldBe a[Request]

            val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
            val today = LocalDate.parse("2016-09-10", formatter)

            x.payload.taxYears.head.householdGetsChildcareElement(today) shouldBe false

          case JsError(e) =>
            throw new RuntimeException(e.toList.toString())
        }
      }

      "(Scenario 27 - C1 working < 16 hours, C2 Incapacitated) determine if household gets childcare element" in {
        val resource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_27.json")
        val json: JsValue = Json.parse(resource.toString)
        val result = json.validate[Request]

        result match {
          case JsSuccess(x, _) =>
            x shouldBe a[Request]

            val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
            val today = LocalDate.parse("2016-09-10", formatter)

            x.payload.taxYears.head.householdGetsChildcareElement(today) shouldBe false

          case JsError(e) =>
            throw new RuntimeException(e.toList.toString())
        }
      }

      "(Scenario 28 - C1 working < 16 hours, C2 Incapacitated + working > 16 hours) determine if household gets childcare element" in {
        val resource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_28.json")
        val json: JsValue = Json.parse(resource.toString)
        val result = json.validate[Request]

        result match {
          case JsSuccess(x, _) =>
            x shouldBe a[Request]

            val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
            val today = LocalDate.parse("2016-09-10", formatter)

            x.payload.taxYears.head.householdGetsChildcareElement(today) shouldBe false

          case JsError(e) =>
            throw new RuntimeException(e.toList.toString())
        }
      }

      "(Scenario 29 - (TY 1 - C1 working > 16 hours)(TY 2 - C1 Incapacitated) determine if household gets childcare element" in {
        val resource: JsonNode = JsonLoader.fromResource("/json/input/tc/scenario_29.json")
        val json: JsValue = Json.parse(resource.toString)
        val result = json.validate[Request]

        result match {
          case JsSuccess(x, _) =>
            x shouldBe a[Request]

            val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
            val today = LocalDate.parse("2016-09-10", formatter)

            x.payload.taxYears.head.householdGetsChildcareElement(today) shouldBe true
            x.payload.taxYears.tail.head.householdGetsChildcareElement(today) shouldBe false

          case JsError(e) =>
            throw new RuntimeException(e.toList.toString())
        }
      }

      ///////////////////////////////////BASIC ELEMENT///////////////////////////////////////////////////////////////////

      "(single claimant with child, claimant is 16, working 16 hours) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2014-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List(claimant))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(single claimant with non qualifying child, claimant is 16, working 16 hours) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("1993-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List(claimant))
        taxYear.getBasicElement(periodStart) shouldBe false
      }

      "(single claimant with 2 children, claimant is 16, working 16 hours, one child qualifying) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("1993-08-31", formatter)
        val dateOfBirth2 = LocalDate.parse("2003-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val child2 = Child(id = 1, name = Some("Child 2"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth2, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1, child2), claimants = List(claimant))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(single claimant with qualifying child, claimant is 16, working less than 16 hours) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2011-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 15, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List(claimant))
        taxYear.getBasicElement(periodStart) shouldBe false
      }

      "(single claimant, incapacitated) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2011-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child), claimants = List(claimant))
        taxYear.getBasicElement(periodStart) shouldBe false
      }

      "(joint claimants with child, both claimants are 16, 1 claimant working 16 hours, 1 claimant working 15 hours) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 15, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(joint claimants with child, both claimants are 16, 1 claimant working 24 hours, 1 claimant is not working)(non-disabled) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 24, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(joint claimants with child, both claimants are 16, 1 claimant working 16 hours, 1 claimant working 8 hours)(non-disabled) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 8, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(joint claimants with child, both claimants are 16, 1 claimant working 16 hours, 1 claimant working 4 hours)(non-disabled) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 4, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe false
      }

      "(joint claimants with child, both claimants are 16, 1 claimant working 16 hours, 1 claimant not working at all)(non-disabled) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe false
      }

      "(joint claimants with child, both claimants are 16, 1 claimant working 16 hours, 1 partner not working + carers allowance)(non-disabled) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, disability = Disability(), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(true))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(joint claimants with child, both claimants are 16, 1 claimant working 16 hours + carers allowance, 1 partner not working)(non-disabled) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(true))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, disability = Disability(), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(joint claimants with child, both claimants are 16, 1 claimant not working + carers allowance, 1 partner working 16 hours)(non-disabled) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, disability = Disability(), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(true))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(joint claimants with child, both claimants are 16, 1 claimant not working, 1 partner working 16 hours + carers allowance)(non-disabled) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, disability = Disability(), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(true))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(joint claimants with child, both claimants are 16, 1 claimant not working + carers allowance, 1 partner working 15 hours)(non-disabled) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, disability = Disability(), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(true))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 15.00, disability = Disability(), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe false
      }

      "(joint claimants with child, both claimants are 16, 1 claimant not working, 1 partner working 15 hours + carers allowance)(non-disabled) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, disability = Disability(), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 15.00, disability = Disability(), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(true))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe false
      }

      "(joint claimants with child, both claimants are 16, 1 claimant working 8 hours, 1 claimant working 4 hours) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 4, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 8, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe false
      }

      "(joint claimants with child, both claimants are 16, 1 claimant working 15 hours, 1 claimant working 15 hours) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 15, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 15, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe false
      }

      "(joint claimants with child, both claimants are 16, 1 claimant working 16 hours, another one is disabled) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(joint claimants with child, both claimants are 16, 1 claimant working 16 hours, 1 claimant is severely disabled) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, disability = Disability(disabled = true, severelyDisabled = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(joint claimants with child, both claimants are 16, 1 claimant working 16 hours, 1 claimant gets disability element and is working at least 16 hours) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(joint claimants with young adult, both claimants are 16, 1 claimant working 0 hours, 1 claimant gets disability element and is working at least 16 hours) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("1998-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 0, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(joint claimants with young adult, both claimants are 16, 1 claimant working 16 hours and is disabled, 1 claimant does not work) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("1998-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 0, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(joint claimants with young adult, both claimants are 16, 1 claimant working 16 hours and is disabled and severely disabled, 1 claimant does not work (non disabled)) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("1998-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 0, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(joint claimants with young adult, both claimants are 16, 1 claimant working 16 hours and is just severely disabled, 1 claimant does not work (non-disabled)) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("1998-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 0, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(joint claimants with non qualifying young adult > 20, both claimants are 16, 1 claimant working 16 hours, 1 claimant gets disability element and is working at least 16 hours) determine if get basic element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("1984-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe false
      }

      "(joint claimants)(claimant is working + incapacitated)(partner is working + incapacitated) determine if get basic element" in {
        // fail
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe false
      }

      "(joint claimants)(claimant is working + incapacitated)(partner is working) determine if get basic element" in {
        // fail
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(joint claimants)(partner is working + incapacitated)(claimant is working) determine if get basic element" in {
        // fail
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(joint claimants)(claimant is not working)(partner is not working + incapacitated) determine if get basic element" in {
        // fail
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe false
      }

      "(joint claimants)(claimant is working < 16 hours)(partner is not working + incapacitated) determine if get basic element" in {
        // fail
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 14, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe false
      }

      "(joint claimants)(claimant is not working + incapacitated)(partner is working < 16 hours) determine if get basic element" in {
        // fail
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 14, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe false
      }

      "(joint claimants)(claimant is not working + incapacitated)(partner is working < 16 hours + disabled) determine if get basic element" in {
        // fail
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 14, disability = Disability(disabled = true, severelyDisabled = false, incapacitated = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe false
      }

      "(joint claimants)(partner is not working + incapacitated)(claimant is working < 16 hours + disabled) determine if get basic element" in {
        // fail
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 14, disability = Disability(disabled = true, severelyDisabled = false, incapacitated = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe false
      }

      "(joint claimants)(claimant is working)(partner is not working + incapacitated) determine if get basic element" in {
        // pass
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe true

      }

      "(joint claimants)(claimant is not working + incapacitated)(partner is working) determine if get basic element" in {
        // pass
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(joint claimants)(claimant is working + disabled)(partner is not working + incapacitated) determine if get basic element" in {
        // pass
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false, incapacitated = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(joint claimants)(claimant is not working + incapacitated)(partner is working + disabled) determine if get basic element" in {
        // pass
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, disability = Disability(disabled = false, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false, incapacitated = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(joint claimants)(claimant is working + disabled)(partner is not working + incapacitated + disabled) determine if get basic element" in {
        // pass
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false, incapacitated = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, disability = Disability(disabled = true, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(joint claimants)(claimant is not working + incapacitated + disabled)(partner is working + disabled) determine if get basic element" in {
        // pass
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2013-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, disability = Disability(disabled = true, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false, incapacitated = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe true
      }

      "(joint claimants)(claimant is not working + incapacitated + disabled)(partner is working + disabled)(child yet to be born) determine if get basic element" in {
        // pass
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2021-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, disability = Disability(disabled = true, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false, incapacitated = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe false
      }

      "(joint claimants)(claimant is not working + incapacitated + disabled)(partner is working + disabled)(child too old) determine if get basic element" in {
        // pass
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("1992-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, disability = Disability(disabled = true, severelyDisabled = false, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false, incapacitated = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.getBasicElement(periodStart) shouldBe false
      }


      /////////////////////////////////////2nd Adult Element///////////////////////////////////////////////////////////////////

      "(joint claimants with a child) determine if get second adult element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2014-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.gets2ndAdultElement(periodStart) shouldBe true
      }

      "(joint claimants with a child, doesn't qualify for basic) determine if get second adult element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("1966-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant2))
        taxYear.gets2ndAdultElement(periodStart) shouldBe false
      }

      "(joint claimants without a child)(not covered in our scenarios) determine if get second adult element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant, claimant2))
        taxYear.gets2ndAdultElement(periodStart) shouldBe false //because it won't get the basic element
      }

      "(single claimant with a child, qualifies for basic) determine if get second adult element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2014-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant))
        taxYear.gets2ndAdultElement(periodStart) shouldBe false
      }

      "(single claimant with a child, doesn't qualify for basic) determine if get second adult element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("1987-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant))
        taxYear.gets2ndAdultElement(periodStart) shouldBe false
      }

      "(single claimant without a child)(not covered in our scenarios) determine if get second adult element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant))
        taxYear.gets2ndAdultElement(periodStart) shouldBe false
      }

      /////////////////////////////////////Lone Parent Element///////////////////////////////////////////////////////////////////

      "(couple with qualifying child) determine if get lone parent element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2015-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant1 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant1))
        taxYear.getsLoneParentElement(periodStart) shouldBe false
      }

      "(single claimant with qualifying child) determine if get lone parent element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2015-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant))
        taxYear.getsLoneParentElement(periodStart) shouldBe true
      }

      "(single claimant with non qualifying child (yet to be born) ) determine if get lone parent element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2017-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant))
        taxYear.getsLoneParentElement(periodStart) shouldBe false
      }

      "(single claimant with non qualifying child) determine if get lone parent element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("1992-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant))
        taxYear.getsLoneParentElement(periodStart) shouldBe false
      }

      "(single claimant with qualifying young adult) determine if get lone parent element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("1999-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2015-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant))
        taxYear.getsLoneParentElement(periodStart) shouldBe true
      }

      "(single claimant with 2 non qualifying children) determine if get lone parent element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("1994-08-31", formatter)
        val dateOfBirth2 = LocalDate.parse("2017-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2015-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val child2 = Child(id = 1, name = Some("Child 2"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth2, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1, child2), claimants = List(claimant))
        taxYear.getsLoneParentElement(periodStart) shouldBe false
      }

      "(single claimant with not qualifying young adult) determine if get lone parent element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("1996-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant))
        taxYear.getsLoneParentElement(periodStart) shouldBe false
      }

      /////////////////////////////////////30 Hours Element///////////////////////////////////////////////////////////////////

      "(single claimant with qualifying child, working >= 30h) determine if get 30 hours element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2006-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 30, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant))
        taxYear.gets30HoursElement(periodStart) shouldBe true
      }

      "(single claimant with qualifying child, working < 30h) determine if get 30 hours element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2006-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 29.99, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant))
        taxYear.gets30HoursElement(periodStart) shouldBe false
      }

      "(couple with qualifying child, one working >= 30h, another not working) determine if get 30 hours element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2006-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 30, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant1 = Claimant(liveOrWork = true, isPartner = true, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant1))
        taxYear.gets30HoursElement(periodStart) shouldBe true
      }

      "(couple with qualifying child, one working 16h, another < 16) determine if get 30 hours element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2006-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant1 = Claimant(liveOrWork = true, isPartner = true, hours = 14, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant1))
        taxYear.gets30HoursElement(periodStart) shouldBe true
      }

      "(couple with qualifying child, one working 15h, another 15h) determine if get 30 hours element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2006-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 15, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant1 = Claimant(liveOrWork = true, isPartner = true, hours = 15, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant1))
        taxYear.gets30HoursElement(periodStart) shouldBe false
      }

      "(couple with qualifying child, one working 16h, another 12h) determine if get 30 hours element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2006-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant1 = Claimant(liveOrWork = true, isPartner = true, hours = 12, disability = Disability(disabled = true, severelyDisabled = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant1))
        taxYear.gets30HoursElement(periodStart) shouldBe false
      }

      "(couple with non qualifying child, one working 16h, another 16h) determine if get 30 hours element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("1996-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant1 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant, claimant1))
        taxYear.gets30HoursElement(periodStart) shouldBe false
      }

      "(couple, one doesn't work in UK, another does) determine if claimants qualify for TC scheme " in {
        val claimant = Claimant(liveOrWork = false, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant1 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant, claimant1))
        taxYear.isCoupleQualifyingForTC shouldBe false
      }

      "(couple, both don't work in UK) determine if claimants qualify for TC scheme " in {
        val claimant = Claimant(liveOrWork = false, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant1 = Claimant(liveOrWork = false, isPartner = true, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant, claimant1))
        taxYear.isCoupleQualifyingForTC shouldBe false
      }

      "(couple, both work in UK) determine if claimants qualify for TC scheme " in {
        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant1 = Claimant(liveOrWork = true, isPartner = true, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(), claimants = List(claimant, claimant1))
        taxYear.isCoupleQualifyingForTC shouldBe true
      }

      /////////////////////////////////////Family Element///////////////////////////////////////////////////////////////////

      "(single claimant (non-qualifying), 1 eligible child, < 15) determine if gets family element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2014-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val claimant = Claimant(liveOrWork = false, isPartner = false, hours = 0, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant))
        taxYear.getsFamilyElement(periodStart) shouldBe false
      }

      "(single claimant, 1 eligible child, < 15) determine if gets family element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2014-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 0, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant))
        taxYear.getsFamilyElement(periodStart) shouldBe true
      }

      "(single claimant, 1 eligible young person, > 16, in Education) determine if gets family element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("2014-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2015-09-05", formatter)


        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 30, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant))
        taxYear.getsFamilyElement(periodStart) shouldBe true
      }

      "(single claimant, 1 non eligible child, > 16, not in education) determine if gets family element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("1996-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)

        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 1, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant))
        taxYear.getsFamilyElement(periodStart) shouldBe false
      }

      "(single claimant, 1 non eligible young person, > 19, in education) determine if gets family element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("1992-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1), claimants = List(claimant))
        taxYear.getsFamilyElement(periodStart) shouldBe false
      }

      "(couple, 1 eligible child, 1 non eligible young person) determine if gets family element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("1992-08-31", formatter)
        val dateOfBirth2 = LocalDate.parse("2014-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val child2 = Child(id = 1, name = Some("Child 2"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth2, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 2, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1, child2), claimants = List(claimant1, claimant2))
        taxYear.getsFamilyElement(periodStart) shouldBe true
      }

      "(couple (non qualifying), 1 eligible child, 1 non eligible young person) determine if gets family element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("1992-08-31", formatter)
        val dateOfBirth2 = LocalDate.parse("2014-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val child2 = Child(id = 1, name = Some("Child 2"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth2, disability = Disability(disabled = false, severelyDisabled = false), education = None)
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = false, isPartner = true, hours = 2, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1, child2), claimants = List(claimant1, claimant2))
        taxYear.getsFamilyElement(periodStart) shouldBe false
      }

      "(couple, 1 non eligible child, 1 non eligible young person) determine if gets family element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth1 = LocalDate.parse("1992-08-31", formatter)
        val dateOfBirth2 = LocalDate.parse("1999-08-31", formatter)
        val periodStart = LocalDate.parse("2016-08-31", formatter)
        val educationStartDate = LocalDate.parse("2006-09-05", formatter)

        val child1 = Child(id = 0, name = Some("Child 1"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth1, disability = Disability(disabled = true, severelyDisabled = false), education = Some(Education(inEducation = true, startDate = educationStartDate)))
        val child2 = Child(id = 1, name = Some("Child 2"), childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth2, disability = Disability(disabled = true, severelyDisabled = false), education = None)
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = true, hours = 2, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), children = List(child1, child2), claimants = List(claimant1, claimant2))
        taxYear.getsFamilyElement(periodStart) shouldBe false
      }

      "determine if at least one claimant in the household working 16h (1 claimant, <16h)" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 15.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), claimants = List(claimant1), children = List())

        taxYear.isOneOfClaimantsWorking16h(taxYear.from) shouldBe false
      }

      "determine if at least one claimant in the household working 16h (1 claimant, =16h)" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), claimants = List(claimant1), children = List())

        taxYear.isOneOfClaimantsWorking16h(taxYear.from) shouldBe true
      }

      "determine if at least one claimant in the household working 16h (2 claimant, <16h, >16h)" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = false, hours = 0.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), claimants = List(claimant1, claimant2), children = List())

        taxYear.isOneOfClaimantsWorking16h(taxYear.from) shouldBe true
      }

      "determine if at least one claimant in the household working 16h (2 claimant, <16h, <16h)" in {
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 15.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant2 = Claimant(liveOrWork = true, isPartner = false, hours = 15.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), claimants = List(claimant1, claimant2), children = List())

        taxYear.isOneOfClaimantsWorking16h(taxYear.from) shouldBe false
      }

      "determine if at least one claimant in the household working 16h (2 claimant, >16h, >16h)" in {
        val claimant2 = Claimant(liveOrWork = true, isPartner = false, hours = 17.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val claimant1 = Claimant(liveOrWork = true, isPartner = false, hours = 40.00, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        val taxYear = TaxYear(from = LocalDate.now, until = LocalDate.now, totalIncome = BigDecimal(0), previousTotalIncome = BigDecimal(0), claimants = List(claimant1, claimant2), children = List())

        taxYear.isOneOfClaimantsWorking16h(taxYear.from) shouldBe true
      }

    }

    "Claimant" should {

      "(qualifying) determine if claimant is qualifying for Tax Credits" in {
        val claimant = Claimant(liveOrWork = true, isPartner = false, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        claimant.isQualifyingForTC shouldBe true
      }

      "(not qualifying) determine if claimant is qualifying for Tax Credits" in {
        val claimant = Claimant(liveOrWork = false, isPartner = false, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        claimant.isQualifyingForTC shouldBe false
      }

      "(lives in uk, is disabled) determine if claimant gets disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val periodStart = LocalDate.parse("2017-08-31", formatter)
        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        claimant.getDisabilityElement(periodStart) shouldBe true
      }

      "(lives in uk, is disabled, doesn't work) determine if claimant gets disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val periodStart = LocalDate.parse("2017-08-31", formatter)
        val claimant = Claimant(liveOrWork = true, isPartner = false, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        claimant.getDisabilityElement(periodStart) shouldBe false
      }

      "(lives in uk, is not disabled) determine if claimant gets disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val periodStart = LocalDate.parse("2017-08-31", formatter)
        val claimant = Claimant(liveOrWork = true, isPartner = false, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        claimant.getDisabilityElement(periodStart) shouldBe false
      }

      "(lives outside uk, is disabled) determine if claimant gets disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val periodStart = LocalDate.parse("2017-08-31", formatter)
        val claimant = Claimant(liveOrWork = false, isPartner = false, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        claimant.getDisabilityElement(periodStart) shouldBe false
      }

      "(lives outside uk, is not disabled) determine if claimant gets disability element" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val periodStart = LocalDate.parse("2017-08-31", formatter)
        val claimant = Claimant(liveOrWork = false, isPartner = false, disability = Disability(disabled = false, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        claimant.getDisabilityElement(periodStart) shouldBe false
      }

      "(lives in uk, is severely disabled) determine if claimant qualifies for severe disability element" in {
        val claimant = Claimant(liveOrWork = true, isPartner = false, disability = Disability(disabled = true, severelyDisabled = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        claimant.isClaimantQualifyingForSevereDisabilityElement shouldBe true
      }

      "(lives in uk, is not severely disabled) determine if claimant qualifies for severe disability element" in {
        val claimant = Claimant(liveOrWork = true, isPartner = false, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        claimant.isClaimantQualifyingForSevereDisabilityElement shouldBe false
      }

      "(lives in uk, is severely disabled, but not disabled) determine if claimant qualifies for severe disability element" in {
        val claimant = Claimant(liveOrWork = true, isPartner = false, disability = Disability(disabled = false, severelyDisabled = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        claimant.isClaimantQualifyingForSevereDisabilityElement shouldBe true
      }

      "(lives outside uk, is severely disabled) determine if claimant qualifies for severe disability element" in {
        val claimant = Claimant(liveOrWork = false, isPartner = false, disability = Disability(disabled = true, severelyDisabled = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        claimant.isClaimantQualifyingForSevereDisabilityElement shouldBe false
      }

      "(lives outside uk, is not severely disabled) determine if claimant qualifies for severe disability element" in {
        val claimant = Claimant(liveOrWork = false, isPartner = false, disability = Disability(disabled = true, severelyDisabled = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        claimant.isClaimantQualifyingForSevereDisabilityElement shouldBe false
      }

      "(> 16 hours) determine if working at least 16 hours per week" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val periodStart = LocalDate.parse("2017-08-31", formatter)
        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 20.00, disability = Disability(disabled = false, severelyDisabled = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        claimant.isWorkingAtLeast16HoursPerWeek(periodStart) shouldBe true
      }

      "(< 16 hours) determine if working at least 16 hours per week" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val periodStart = LocalDate.parse("2017-08-31", formatter)
        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 15.00, disability = Disability(disabled = false, severelyDisabled = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        claimant.isWorkingAtLeast16HoursPerWeek(periodStart) shouldBe false
      }

      "(is 16 hours) determine if working at least 16 hours per week" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val periodStart = LocalDate.parse("2017-08-31", formatter)
        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 16.00, disability = Disability(disabled = false, severelyDisabled = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        claimant.isWorkingAtLeast16HoursPerWeek(periodStart) shouldBe true
      }

      "(> 24 hours) determine if working at least 24 hours per week" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val periodStart = LocalDate.parse("2017-08-31", formatter)
        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 25.00, disability = Disability(disabled = false, severelyDisabled = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        claimant.isWorkingAtLeast24HoursPerWeek(periodStart) shouldBe true
      }

      "(< 24 hours) determine if working at least 24 hours per week" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val periodStart = LocalDate.parse("2017-08-31", formatter)
        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 15.00, disability = Disability(disabled = false, severelyDisabled = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        claimant.isWorkingAtLeast24HoursPerWeek(periodStart) shouldBe false
      }

      "(is 24 hours) determine if working at least 24 hours per week" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val periodStart = LocalDate.parse("2017-08-31", formatter)
        val claimant = Claimant(liveOrWork = true, isPartner = false, hours = 24.00, disability = Disability(disabled = false, severelyDisabled = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        claimant.isWorkingAtLeast24HoursPerWeek(periodStart) shouldBe true
      }

      "(is incapacitated) determine if a claimant is incapacitated" in {
        val claimant = Claimant(liveOrWork = true, isPartner = false, disability = Disability(disabled = false, severelyDisabled = true, incapacitated = true), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        claimant.isIncapacitated shouldBe true
      }

      "(is not incapacitated) determine if a claimant is incapacitated" in {
        val claimant = Claimant(liveOrWork = true, isPartner = false, disability = Disability(disabled = false, severelyDisabled = true, incapacitated = false), schemesClaiming = SchemesClaiming(), otherSupport = OtherSupport(false))
        claimant.isIncapacitated shouldBe false
      }
    }
    
  }
}
