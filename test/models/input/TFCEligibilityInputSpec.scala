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

package models.input

import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader
import controllers.FakeCCEligibilityApplication
import models.input.tfc._
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.scalatestplus.mockito.MockitoSugar
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}
import play.api.test.FakeRequest
import utils.{CCConfigSpec, Periods, TFCConfig}

class TFCEligibilityInputSpec extends CCConfigSpec with FakeCCEligibilityApplication with MockitoSugar {

  implicit val req = FakeRequest()

  "TFCEInputEligibility" must {

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

    "Child" must {

      "Determine disability status if child is disabled" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
        val child = new TFCChild(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability =
          TFCDisability(disabled = true))(None)
        child.isDisabled shouldBe true
      }

      "Determine disability status if child is severely disabled" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
        val child = new TFCChild(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability =
          TFCDisability(severelyDisabled = true))(None)
        child.isDisabled shouldBe true
      }

      "Determine disability status if child is not disabled and not severely disabled" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
        val child = new TFCChild(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability =
          TFCDisability())(None)
        child.isDisabled shouldBe false
      }

      "Determine disability status if child is disabled and severely disabled" in {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
        val dateOfBirth = LocalDate.parse("2000-08-27", formatter)
        val child = new TFCChild(id = 0, childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, dob = dateOfBirth, disability =
          TFCDisability(disabled = true, severelyDisabled = true))(None)
        child.isDisabled shouldBe true
      }
    }


    "getNWMPerAge checks" must {

      val tfcConfig = app.injector.instanceOf[TFCConfig]

      "check if getNWMPerAge is correct for under-18 year olds" in {
        val claimant = TFCClaimant(disability = TFCDisability(), carersAllowance = true, minimumEarnings =
          TFCMinimumEarnings(), age = Some("under-18"))
        val taxYearConfig = tfcConfig.getConfig(LocalDate.parse("2000-08-27", formatter), "england")
        claimant.getNWMPerAge(taxYearConfig)._1 shouldBe taxYearConfig.nmwUnder18
      }
      "check if getNWMPerAge is correct for 18-20 year olds" in {
        val claimant = TFCClaimant(disability = TFCDisability(), carersAllowance = true, minimumEarnings =
          TFCMinimumEarnings(), age = Some("18-20"))
        val taxYearConfig = tfcConfig.getConfig(LocalDate.parse("2000-08-27", formatter), "england")
        claimant.getNWMPerAge(taxYearConfig)._1 shouldBe taxYearConfig.nmw18To20
      }
      "check if getNWMPerAge is correct for 21 -24 year olds" in {
        val claimant = TFCClaimant(disability = TFCDisability(), carersAllowance = true, minimumEarnings =
          TFCMinimumEarnings(), age = Some("21-24"))
        val taxYearConfig = tfcConfig.getConfig(LocalDate.parse("2000-08-27", formatter), "england")
        claimant.getNWMPerAge(taxYearConfig)._1 shouldBe taxYearConfig.nmw21To24
      }
      "check if getNWMPerAge is correct for over 25 year olds" in {
        val claimant = TFCClaimant(disability = TFCDisability(), carersAllowance = true, minimumEarnings =
          TFCMinimumEarnings(), age = Some("25 or over"))
        val taxYearConfig = tfcConfig.getConfig(LocalDate.parse("2000-08-27", formatter), "england")
        claimant.getNWMPerAge(taxYearConfig)._1 shouldBe taxYearConfig.nmw25Over
      }
    }

    "validMaxEarnings" must {

      val claimant = TFCClaimant(hoursPerWeek = 16.50, disability = TFCDisability(), minimumEarnings =
        TFCMinimumEarnings(), age = None)

      val partner = TFCClaimant(hoursPerWeek = 17.50, isPartner = true, disability = TFCDisability(), minimumEarnings =
        TFCMinimumEarnings(), age = None)

      "return true when its single parent journey when parent max earnings does not exist" in {
        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant), List())

        tfc.validMaxEarnings() shouldBe true
      }

      "return true when its single parent journey and max earnings is false" in {
        val parent = claimant.copy(maximumEarnings = Some(false))
        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(parent), List())

        tfc.validMaxEarnings() shouldBe true
      }

      //This test is for live service (old one)
      "return true when journey is with partner and max earnings for both parents does not exist" in {
        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england", List(claimant, partner), List())

        tfc.validMaxEarnings() shouldBe true
      }

      "return true when journey is with partner and both parents have max earnings as false" in {
        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england",
          List(claimant.copy(maximumEarnings = Some(false)), partner.copy(maximumEarnings = Some(false))), List())

        tfc.validMaxEarnings() shouldBe true
      }

      "return true when journey is with partner, parent has false for max earnings and partner max earnings does not exist" in {
        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england",
          List(claimant.copy(maximumEarnings = Some(false)), partner), List())

        tfc.validMaxEarnings() shouldBe true
      }

      "return true when journey is with partner, partner has false for max earnings and parent max earnings does not exist" in {
        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england",
          List(claimant, partner.copy(maximumEarnings = Some(false))), List())

        tfc.validMaxEarnings() shouldBe true
      }

      "return false when journey is with partner and both parents have max earnings as true" in {
        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england",
          List(claimant.copy(maximumEarnings = Some(true)), partner.copy(maximumEarnings = Some(true))), List())

        tfc.validMaxEarnings() shouldBe false
      }

      "return true when journey is with partner, partner has true for max earnings and parent max earnings does not exist" in {
        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england",
          List(claimant, partner.copy(maximumEarnings = Some(true))), List())

        tfc.validMaxEarnings() shouldBe false
      }

      "return true when journey is with partner, parent has true for max earnings and partner max earnings does not exist" in {
        val tfc = TFCEligibilityInput(from = fromDate, numberOfPeriods = 1, location = "england",
          List(claimant.copy(maximumEarnings = Some(true)), partner), List())

        tfc.validMaxEarnings() shouldBe false
      }
    }

  }
}
