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

package models.mapping

import controllers.FakeCCEligibilityApplication
import models._
import models.input.tfc._
import models.mappings._
import org.joda.time.LocalDate
import org.mockito.Mockito.when
import org.scalatest.mock.MockitoSugar
import utils.{CCConfigSpec, Periods, TFCConfig}

class HHToTFCEligibilityInputSpec extends CCConfigSpec with FakeCCEligibilityApplication with MockitoSugar {

  val SUT = new HHToTFCEligibilityInput {
    override val tFCConfig = mock[TFCConfig]
  }

  "HHToTFCEligibilityInput" should {

    "have reference to TFCConfig" in {
      HHToTFCEligibilityInput.tFCConfig.isInstanceOf[TFCConfig] shouldBe true
    }
    "return 4 as a default if tfcNoOfPeriods is not specified" in {
      HHToTFCEligibilityInput.tFCConfig.tfcNoOfPeriods shouldBe 4
    }

    "convert Household to TFC Eligibility Input" when {
      "a household with parent, no partner and 2 children" in {
        val currentDate = LocalDate.now
        val dob = LocalDate.now().minusYears(2)

        val hhChild1 = Child(
          id = 0,
          name = "child1",
          dob = Some(dob),
          disability = Some(Disability(disabled = true, severelyDisabled = false, blind = false)),
          childcareCost = Some(ChildCareCost(amount = Some(350), Some(PeriodEnum.MONTHLY)))
        )
        val hhChild2 = Child(
          id = 1,
          name = "child2",
          dob = Some(dob),
          disability = Some(Disability(disabled = false, severelyDisabled = true, blind = false)),
          childcareCost = Some(ChildCareCost(amount = Some(100), Some(PeriodEnum.MONTHLY)))
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
          currentYearlyIncome = Some(Income(employmentIncome = Some(25000),
            pension = Some(1200),
            otherIncome = Some(6000),
            benefits = None,
            statutoryIncome = None
          )),
          hours = Some(32),
          minimumEarnings = Some(MinimumEarnings(BigDecimal(3900), Some(EmploymentStatusEnum.SELFEMPLOYED), Some(true))),
          escVouchers = Some(YesNoUnsureBothEnum.YES)
        )

        val hhModel = Household(None, Some(LocationEnum.ENGLAND), false, List(hhChild1, hhChild2), parent, None)

        val expectedOutput = TFCEligibilityInput(currentDate, 4, "england",
          List(TFCClaimant(None, Some(TFCIncome(Some(25000), Some(1200), Some(6000))), 32.0, false, TFCDisability(false, false), false,
            TFCMinimumEarnings(true, 3900), Some(AgeRangeEnum.EIGHTEENTOTWENTY.toString), Some(Some(EmploymentStatusEnum.SELFEMPLOYED).toString), Some(true))),
          List(TFCChild(0, 350, Periods.Monthly, dob, TFCDisability(true, false)),
            TFCChild(1, 100, Periods.Monthly, dob, TFCDisability(false, true))))

        when(SUT.tFCConfig.tfcNoOfPeriods).thenReturn(4.toShort)
        when(SUT.tFCConfig.StartDate).thenReturn(currentDate)

        val output = SUT.convert(hhModel)
        output.isInstanceOf[TFCEligibilityInput] shouldBe true
        output shouldBe expectedOutput
      }

      "a household with parent, partner and 2 children" in {
        val currentDate = LocalDate.now
        val dob = LocalDate.now().minusYears(2)

        val hhChild1 = Child(
          id = 0,
          name = "child1",
          dob = Some(dob),
          disability = Some(Disability(disabled = true, severelyDisabled = false, blind = true)),
          childcareCost = Some(ChildCareCost(amount = None, None))
        )
        val hhChild2 = Child(
          id = 1,
          name = "child2",
          dob = Some(dob),
          disability = Some(Disability(disabled = false, severelyDisabled = false, blind = true)),
          childcareCost = Some(ChildCareCost(amount = Some(1000), Some(PeriodEnum.MONTHLY)))
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
          hours = Some(34),
          minimumEarnings = Some(MinimumEarnings(BigDecimal(2313), Some(EmploymentStatusEnum.SELFEMPLOYED), Some(true))),
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
          hours = Some(21),
          minimumEarnings = None,
          escVouchers = Some(YesNoUnsureBothEnum.NOTSURE)
        )

        val hhModel = Household(None, Some(LocationEnum.ENGLAND), true, List(hhChild1, hhChild2), parent, Some(partner))

        val expectedOutput = TFCEligibilityInput(currentDate, 4, "england",
          List(TFCClaimant(None, Some(TFCIncome(Some(12212), Some(47674), Some(647864))), 34.0, false, TFCDisability(false, false), false,
            TFCMinimumEarnings(true, 2313), Some(AgeRangeEnum.EIGHTEENTOTWENTY.toString), Some(Some(EmploymentStatusEnum.SELFEMPLOYED).toString), Some(true)),
            TFCClaimant(None, Some(TFCIncome(Some(12212), Some(47674), Some(647864))), 21.0, true, TFCDisability(false, false), false,
              TFCMinimumEarnings(false, 0.0), Some(AgeRangeEnum.EIGHTEENTOTWENTY.toString), None, None)),
          List(TFCChild(0, 0, Periods.Monthly, dob, TFCDisability(true, false)),
            TFCChild(1, 1000, Periods.Monthly, dob, TFCDisability(true, false))))

        when(SUT.tFCConfig.tfcNoOfPeriods).thenReturn(4.toShort)
        when(SUT.tFCConfig.StartDate).thenReturn(currentDate)

        val output = SUT.convert(hhModel)
        output.isInstanceOf[TFCEligibilityInput] shouldBe true
        output shouldBe expectedOutput
      }
    }
  }
}
