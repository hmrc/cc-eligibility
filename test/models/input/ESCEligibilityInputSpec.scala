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

package models.input

import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader
import controllers.FakeCCEligibilityApplication
import fixtures.ESCChildren
import models.input.esc._
import java.time.LocalDate
import play.api.libs.json.{JsValue, Json}
import utils.{CCConfig, ESCConfig}

class ESCEligibilityInputSpec extends FakeCCEligibilityApplication with ESCChildren {

  override def eSCConfig: Option[ESCConfig] = Some(app.injector.instanceOf[ESCConfig])
  override def ccConfig: Option[CCConfig]   = Some(app.injector.instanceOf[CCConfig])

  "ESCInputEligibility" must {

    "read a valid JSON input and convert to a specific type" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/input/esc/eligibility_input_test.json")
      val json: JsValue      = Json.parse(resource.toString)
      val result             = json.validate[ESCEligibilityInput]
      val taxYeaar           = result.get.escTaxYears.head

      taxYeaar.from shouldBe a[LocalDate]
      taxYeaar.until shouldBe a[LocalDate]
      taxYeaar.claimants.isInstanceOf[List[ESCClaimant]] shouldBe true
      taxYeaar.children.isInstanceOf[List[ESCChild]] shouldBe true

      // Claimant model
      val claimant = taxYeaar.claimants.head
      claimant.isPartner.isInstanceOf[Boolean] shouldBe true
      claimant.employerProvidesESC.isInstanceOf[Boolean] shouldBe true

      // Children model
      val child = taxYeaar.children.head
      child.id.isInstanceOf[Short] shouldBe true
      child.dob shouldBe a[LocalDate]
      child.disability shouldBe a[ESCDisability]

      // Disability model
      child.disability.disabled.isInstanceOf[Boolean] shouldBe true
      child.disability.severelyDisabled.isInstanceOf[Boolean] shouldBe true
    }
  }

  "Child" must {

    "(ESC)(< 0) determine if the child is qualifying for esc" in {
      val dateOfBirth = LocalDate.parse("2016-08-31", formatter)
      val today       = LocalDate.parse("2015-09-10", formatter)
      val child       = buildChild(dob = dateOfBirth)
      child.qualifiesForESC(now = today) shouldBe false
    }

    "(ESC)(< 15) determine if the child is qualifying for esc" in {
      val dateOfBirth = LocalDate.parse("2004-08-31", formatter)
      val today       = LocalDate.parse("2015-09-10", formatter)
      val child       = buildChild(dob = dateOfBirth)
      child.qualifiesForESC(now = today) shouldBe true
    }

    "(ESC)(15)(before september following their birthday) determine if the child is qualifying for esc" in {
      val dateOfBirth = LocalDate.parse("2001-06-27", formatter)
      val today       = LocalDate.parse("2016-08-10", formatter)
      val child       = buildChild(dob = dateOfBirth)
      child.qualifiesForESC(now = today) shouldBe true
    }

    "(ESC)(15)(after september following their birthday) determine if the child is qualifying for esc" in {
      val dateOfBirth = LocalDate.parse("2000-06-27", formatter)
      val today       = LocalDate.parse("2016-09-10", formatter)
      val child       = buildChild(dob = dateOfBirth)
      child.qualifiesForESC(now = today) shouldBe false
    }

    "(ESC)(15)(before september following their birthday, 1st September) determine if the child is qualifying for esc" in {
      val dateOfBirth = LocalDate.parse("2001-09-01", formatter)
      val today       = LocalDate.parse("2016-09-10", formatter)
      val child       = buildChild(dob = dateOfBirth)
      child.qualifiesForESC(now = today) shouldBe true
    }

    "(ESC)(16)(non disabled) determine if the child is qualifying for esc" in {
      val dateOfBirth = LocalDate.parse("2000-06-27", formatter)
      val today       = LocalDate.parse("2016-08-10", formatter)
      val child       = buildChild(dob = dateOfBirth)
      child.qualifiesForESC(now = today) shouldBe false
    }

    "(ESC)(16)(disabled)(before september following their birthday) determine if the child is qualifying for esc" in {
      val dateOfBirth = LocalDate.parse("2000-06-27", formatter)
      val today       = LocalDate.parse("2016-08-10", formatter)
      val child       = buildChild(dob = dateOfBirth, disabled = true)
      child.qualifiesForESC(now = today) shouldBe true
    }

    "(ESC)(16)(disabled)(before september following their birthday, 1st September) determine if the child is qualifying for esc" in {
      val dateOfBirth = LocalDate.parse("2000-09-01", formatter)
      val today       = LocalDate.parse("2016-09-10", formatter)
      val child       = buildChild(dob = dateOfBirth, disabled = true)
      child.qualifiesForESC(now = today) shouldBe true
    }

    "(ESC)(16)(non disabled)(before september following their birthday, 1st September) determine if the child is qualifying for esc" in {
      val dateOfBirth = LocalDate.parse("2000-09-01", formatter)
      val today       = LocalDate.parse("2016-09-10", formatter)
      val child       = buildChild(dob = dateOfBirth)
      child.qualifiesForESC(now = today) shouldBe false
    }

    "(ESC)(16)(disabled)(after september following birthday) determine if the child is qualifying for esc" in {
      val dateOfBirth = LocalDate.parse("2000-06-27", formatter)
      val today       = LocalDate.parse("2016-09-10", formatter)
      val child       = buildChild(dob = dateOfBirth, disabled = true)
      child.qualifiesForESC(now = today) shouldBe false
    }

    "(ESC)(> 16) determine if the child is qualifying for esc" in {
      val dateOfBirth = LocalDate.parse("1992-08-31", formatter)
      val today       = LocalDate.parse("2015-09-10", formatter)
      val child       = buildChild(dob = dateOfBirth, disabled = true)
      child.qualifiesForESC(now = today) shouldBe false
    }
  }

  "Claimant" must {

    "Employer providing vouchers determine if claimants qualifies for esc (receives vouchers)" in {
      val claimant = ESCClaimant(isPartner = false, employerProvidesESC = true)
      claimant.isClaimantQualifyingForESC shouldBe true
    }

    "Employer not providing vouchers determine if claimants qualifies for esc (receives vouchers)" in {
      val claimant = ESCClaimant(isPartner = false, employerProvidesESC = false)
      claimant.isClaimantQualifyingForESC shouldBe false
    }

  }

}
