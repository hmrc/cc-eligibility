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

package mapping

import java.time.Month

import controllers.FakeCCEligibilityApplication
import models._
import models.input.esc._
import models.mappings.HHToESCEligibilityInput
import org.joda.time.LocalDate
import spec.CCConfigSpec
import utils.Periods

class HHToESCEligibilityInputSpec extends CCConfigSpec with FakeCCEligibilityApplication{

val SUT = HHToESCEligibilityInput

  "HHToESCEligibilityInput" should {

    "accept a valid Household model and return a valid ESCEligibilityInput model" in {

      val hhChild1 = Child(
        id = 0,
        name = "child1",
        dob = Some(LocalDate.parse("2016-08-31", formatter)),
        disability = Some(Disability(disabled = true, severelyDisabled = false, blind = true)),
        childcareCost = Some(ChildCareCost(amount = Some(350), Some(PeriodEnum.MONTHLY))),
        education = Some(Education(inEducation = false, startDate = Some(LocalDate.now())))
      )
      val hhChild2 = Child(
        id = 1,
        name = "child2",
        dob = Some(LocalDate.parse("2016-08-31", formatter)),
        disability = Some(Disability(disabled = false, severelyDisabled = false, blind = true)),
        childcareCost = Some(ChildCareCost(amount = Some(1000), Some(PeriodEnum.MONTHLY))),
        education = Some(Education(inEducation = false, startDate = Some(LocalDate.now())))
      )
      val parent = Claimant(
        ageRange = Some(AgeRangeEnum.EIGHTEENTOTWENTY),
        benefits = Some(Benefits(
          disabilityBenefits = false,
          highRateDisabilityBenefits = false,
          incomeBenefits = false,
          carersAllowance = false
        )),
        lastYearlyIncome = None,
        currentYearlyIncome = Some(Income(employmentIncome = Some(12212),
          pension = Some(47674),
          otherIncome = Some(647864),
          benefits = Some(546),
          statutoryIncome = None
        )),
        hours = Some(4567),
        minimumEarnings = None,
        escVouchers = Some(YesNoUnsureBothEnum.YES)
      )
      val partner = Claimant(
        ageRange = Some(AgeRangeEnum.EIGHTEENTOTWENTY),
        benefits = Some(Benefits(
          disabilityBenefits = false,
          highRateDisabilityBenefits = false,
          incomeBenefits = false,
          carersAllowance = false
        )),
        lastYearlyIncome = None,
        currentYearlyIncome = Some(Income(employmentIncome = Some(12212),
          pension = Some(47674),
          otherIncome = Some(647864),
          benefits = Some(546),
          statutoryIncome = None
        )),
        hours = Some(4567),
        minimumEarnings = None,
        escVouchers = Some(YesNoUnsureBothEnum.NOTSURE)
      )

      val hhModel = Household(None, Some(LocationEnum.ENGLAND), true, List(hhChild1, hhChild2), parent, Some(partner))

      val expectedOutput = ESCEligibilityInput(List(
        ESCTaxYear(LocalDate.parse("2017-08-02", formatter), LocalDate.parse("2018-04-06", formatter),
      List(ESCClaimant(false,true), ESCClaimant(true,true)),List(ESCChild(1,LocalDate.parse("2016-08-31", formatter),350,Periods.Monthly,ESCDisability(true,false)),
          ESCChild(1,LocalDate.parse("2016-08-31", formatter),1000,Periods.Monthly,ESCDisability(true,false)))),
        ESCTaxYear(LocalDate.parse("2018-04-06", formatter),LocalDate.parse("2018-08-02", formatter),
      List(ESCClaimant(false,true), ESCClaimant(true,true)),List(ESCChild(1,LocalDate.parse("2016-08-31", formatter),350,Periods.Monthly,ESCDisability(true,false)),
            ESCChild(1,LocalDate.parse("2016-08-31", formatter),1000,Periods.Monthly,ESCDisability(true,false))))))

      val output = SUT.convert(hhModel)
      output.isInstanceOf[ESCEligibilityInput] shouldBe true
      output shouldBe expectedOutput
    }
  }
}
