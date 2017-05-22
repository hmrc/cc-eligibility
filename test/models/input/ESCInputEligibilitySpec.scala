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
import models.input.esc._
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.libs.json.{JsSuccess, JsValue, Json}
import spec.CCSpecConfig

class ESCInputEligibilitySpec extends CCSpecConfig with FakeCCEligibilityApplication {

  "ESCInputEligibility" should {

    "read a valid JSON input and convert to a specific type" in {
        val resource: JsonNode = JsonLoader.fromResource("/json/input/esc/eligibility_input_test.json")
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
            x.payload.taxYears.head.claimants.head.isPartner.isInstanceOf[Boolean] shouldBe true
            x.payload.taxYears.head.claimants.head.employerProvidesESC.isInstanceOf[Boolean] shouldBe true
            x.payload.taxYears.head.claimants.head.elements shouldBe a[ClaimantsElements]

            //Children model
            x.payload.taxYears.head.children.head.id.isInstanceOf[Short] shouldBe true
            x.payload.taxYears.head.children.head.name.isInstanceOf[Option[String]] shouldBe true
            x.payload.taxYears.head.children.head.dob shouldBe a[LocalDate]
            x.payload.taxYears.head.children.head.disability shouldBe a[Disability]

            //Disability model
            x.payload.taxYears.head.children.head.disability.disabled.isInstanceOf[Boolean] shouldBe true
            x.payload.taxYears.head.children.head.disability.severelyDisabled.isInstanceOf[Boolean] shouldBe true
          }
          case _ => throw new Exception
        }
      }
  }
  "Child" should {
  
      "(ESC)(< 0) determine if the child is qualifying for esc" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2016-08-31", formatter)
        val today = LocalDate.parse("2015-09-10", formatter)
  
        val child = Child(id = 0, name = Some("Child 1"), dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
        child.qualifiesForESC(now = today) shouldBe false
      }
  
      "(ESC)(< 15) determine if the child is qualifying for esc" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2004-08-31", formatter)
        val today = LocalDate.parse("2015-09-10", formatter)
  
        val child = Child(id = 0, name = Some("Child 1"), dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
        child.qualifiesForESC(now = today) shouldBe true
      }
  
      "(ESC)(15)(before september following their birthday) determine if the child is qualifying for esc" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-06-27", formatter)
        val today = LocalDate.parse("2016-08-10", formatter)
  
        val child = Child(id = 0, name = Some("Child 1"), dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
        child.qualifiesForESC(now = today) shouldBe true
      }
  
      "(ESC)(15)(after september following their birthday) determine if the child is qualifying for esc" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2000-06-27", formatter)
        val today = LocalDate.parse("2016-09-10", formatter)
  
        val child = Child(id = 0, name = Some("Child 1"), dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
        child.qualifiesForESC(now = today) shouldBe false
      }
  
      "(ESC)(15)(before september following their birthday, 1st September) determine if the child is qualifying for esc" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2001-09-01", formatter)
        val today = LocalDate.parse("2016-09-10", formatter)
  
        val child = Child(id = 0, name = Some("Child 1"), dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
        child.qualifiesForESC(now = today) shouldBe true
      }
  
      "(ESC)(16)(non disabled) determine if the child is qualifying for esc" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2000-06-27", formatter)
        val today = LocalDate.parse("2016-08-10", formatter)
  
        val child = Child(id = 0, name = Some("Child 1"), dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
        child.qualifiesForESC(now = today) shouldBe false
      }
  
      "(ESC)(16)(disabled)(before september following their birthday) determine if the child is qualifying for esc" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2000-06-27", formatter)
        val today = LocalDate.parse("2016-08-10", formatter)
  
        val child = Child(id = 0, name = Some("Child 1"), dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false))
        child.qualifiesForESC(now = today) shouldBe true
      }
  
      "(ESC)(16)(disabled)(before september following their birthday, 1st September) determine if the child is qualifying for esc" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2000-09-01", formatter)
        val today = LocalDate.parse("2016-09-10", formatter)
  
        val child = Child(id = 0, name = Some("Child 1"), dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false))
        child.qualifiesForESC(now = today) shouldBe true
      }
  
      "(ESC)(16)(non disabled)(before september following their birthday, 1st September) determine if the child is qualifying for esc" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2000-09-01", formatter)
        val today = LocalDate.parse("2016-09-10", formatter)
  
        val child = Child(id = 0, name = Some("Child 1"), dob = dateOfBirth, disability = Disability(disabled = false, severelyDisabled = false))
        child.qualifiesForESC(now = today) shouldBe false
      }
  
      "(ESC)(16)(disabled)(after september following birthday) determine if the child is qualifying for esc" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2000-06-27", formatter)
        val today = LocalDate.parse("2016-09-10", formatter)
  
        val child = Child(id = 0, name = Some("Child 1"), dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false))
        child.qualifiesForESC(now = today) shouldBe false
      }
  
      "(ESC)(> 16) determine if the child is qualifying for esc" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("1992-08-31", formatter)
        val today = LocalDate.parse("2015-09-10", formatter)
  
        val child = Child(id = 0, name = Some("Child 1"), dob = dateOfBirth, disability = Disability(disabled = true, severelyDisabled = false))
        child.qualifiesForESC(now = today) shouldBe false
      }
  }

    "Claimant" should {
  
      "(Employer providing vouchers, claimant receiving vouchers) determine if claimants qualifies for esc (receives vouchers)" in {
        val claimant = Claimant(isPartner = false, elements = ClaimantsElements(vouchers = true), employerProvidesESC = true)
        claimant.isClaimantQualifyingForESC shouldBe true
      }

      "(Employer not providing vouchers, claimant not receiving vouchers) determine if claimants qualifies for esc (receives vouchers)" in {
        val claimant = Claimant(isPartner = false, elements = ClaimantsElements(vouchers = false), employerProvidesESC = false)
        claimant.isClaimantQualifyingForESC shouldBe false
      }
  
      "(Employer not providing vouchers, claimant not receiving vouchers, claimant not working in UK) determine if claimants qualifies for esc (receives vouchers)" in {
        val claimant = Claimant(isPartner = false, elements = ClaimantsElements(vouchers = false), employerProvidesESC = false)
        claimant.isClaimantQualifyingForESC shouldBe false
      }
  
      "(Employer providing vouchers, claimant not receiving vouchers) determine if claimants qualifies for esc (receives vouchers)" in {
        val claimant = Claimant(isPartner = false, elements = ClaimantsElements(vouchers = false), employerProvidesESC = true)
        claimant.isClaimantQualifyingForESC shouldBe false
      }
  
      "(Employer not providing vouchers - ClaimantsElements() case) determine if claimants qualifies for esc (receives vouchers)" in {
        val claimant = Claimant(isPartner = false, elements = ClaimantsElements())
        claimant.isClaimantQualifyingForESC shouldBe false
      }

    }
  
}
