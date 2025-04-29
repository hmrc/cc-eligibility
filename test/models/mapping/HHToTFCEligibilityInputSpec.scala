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

package models.mapping

import controllers.FakeCCEligibilityApplication
import models.ParentsBenefits.IncapacityBenefit
import models._
import models.input.tfc._
import models.mappings._

import java.time.LocalDate
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import utils.{CCConfig, Periods, TFCConfig}

class HHToTFCEligibilityInputSpec extends FakeCCEligibilityApplication with MockitoSugar {

  val mockTFC = mock[TFCConfig]
  val SUT     = new HHToTFCEligibilityInput(mockTFC, mock[CCConfig])

  "HHToTFCEligibilityInput" must {

    "convert Household to TFC Eligibility Input" when {
      "a household with parent, no partner and 2 children" in {
        val currentDate = LocalDate.now
        val dob         = LocalDate.now().minusYears(2)

        val hhChild1 = Child(
          id = 0,
          name = "child1",
          dob = Some(dob),
          disability = Some(Disability(disabled = true, severelyDisabled = false, blind = false)),
          childcareCost = Some(ChildCareCost(amount = Some(350), period = Some(PeriodEnum.MONTHLY)))
        )
        val hhChild2 = Child(
          id = 1,
          name = "child2",
          dob = Some(dob),
          disability = Some(Disability(disabled = false, severelyDisabled = true, blind = false)),
          childcareCost = Some(ChildCareCost(amount = Some(100), period = Some(PeriodEnum.MONTHLY)))
        )
        val parent = Claimant(
          ageRange = Some(AgeRangeEnum.EIGHTEENTOTWENTY),
          benefits = None,
          currentYearlyIncome = Some(
            Income(
              employmentIncome = Some(25000),
              pension = Some(1200),
              otherIncome = Some(6000),
              benefits = None
            )
          ),
          minimumEarnings = Some(
            MinimumEarnings(
              amount = BigDecimal(3900),
              employmentStatus = Some(EmploymentStatusEnum.SELFEMPLOYED),
              selfEmployedIn12Months = Some(true)
            )
          ),
          escVouchers = Some(YesNoUnsureEnum.YES),
          maximumEarnings = Some(false)
        )

        val hhModel = Household(None, Some(LocationEnum.ENGLAND), List(hhChild1, hhChild2), parent, None)

        val expectedOutput = TFCEligibilityInput(
          from = currentDate,
          numberOfPeriods = 4,
          location = "england",
          claimants = List(
            TFCClaimant(
              currentIncome = Some(
                TFCIncome(employmentIncome = Some(25000), pension = Some(1200), otherIncome = Some(6000))
              ),
              isPartner = false,
              disability = TFCDisability(),
              carersAllowance = false,
              minimumEarnings = TFCMinimumEarnings(amount = 3900),
              age = Some(AgeRangeEnum.EIGHTEENTOTWENTY.toString),
              employmentStatus = Some(EmploymentStatusEnum.SELFEMPLOYED.toString),
              selfEmployedSelection = Some(true),
              maximumEarnings = Some(false)
            )
          ),
          children = List(
            TFCChild(
              id = 0,
              childcareCost = 350,
              childcareCostPeriod = Periods.Monthly,
              dob = dob,
              disability = TFCDisability(disabled = true)
            ),
            TFCChild(
              id = 1,
              childcareCost = 100,
              childcareCostPeriod = Periods.Monthly,
              dob = dob,
              disability = TFCDisability(severelyDisabled = true)
            )
          )
        )

        val mockCC = mock[CCConfig]

        when(mockTFC.tfcNoOfPeriods).thenReturn(4.toShort)
        when(mockTFC.config).thenReturn(mockCC)
        when(mockCC.startDate).thenReturn(currentDate)

        val output = SUT.convert(hhModel)
        output.isInstanceOf[TFCEligibilityInput] shouldBe true
        output shouldBe expectedOutput
      }

      "a household with parent, partner and 2 children" in {
        val currentDate = LocalDate.now
        val dob         = LocalDate.now().minusYears(2)

        val hhChild1 = Child(
          id = 0,
          name = "child1",
          dob = Some(dob),
          disability = Some(Disability(disabled = true, severelyDisabled = false, blind = true)),
          childcareCost = Some(ChildCareCost(amount = None, period = None))
        )
        val hhChild2 = Child(
          id = 1,
          name = "child2",
          dob = Some(dob),
          disability = Some(Disability(disabled = false, severelyDisabled = false, blind = true)),
          childcareCost = Some(ChildCareCost(amount = Some(1000), period = Some(PeriodEnum.MONTHLY)))
        )
        val parent = Claimant(
          ageRange = Some(AgeRangeEnum.EIGHTEENTOTWENTY),
          benefits = Some(Set(IncapacityBenefit)),
          currentYearlyIncome = Some(
            Income(
              employmentIncome = Some(12212),
              pension = Some(47674),
              otherIncome = Some(647864),
              benefits = Some(546)
            )
          ),
          minimumEarnings = Some(MinimumEarnings(BigDecimal(0), Some(EmploymentStatusEnum.SELFEMPLOYED), Some(true))),
          escVouchers = Some(YesNoUnsureEnum.YES)
        )
        val partner = Claimant(
          ageRange = Some(AgeRangeEnum.EIGHTEENTOTWENTY),
          benefits = None,
          currentYearlyIncome = Some(
            Income(
              employmentIncome = Some(12212),
              pension = Some(47674),
              otherIncome = Some(647864),
              benefits = Some(546)
            )
          ),
          minimumEarnings = None,
          escVouchers = Some(YesNoUnsureEnum.NOTSURE)
        )

        val hhModel = Household(None, Some(LocationEnum.ENGLAND), List(hhChild1, hhChild2), parent, Some(partner))

        val expectedOutput = TFCEligibilityInput(
          from = currentDate,
          numberOfPeriods = 4,
          location = "england",
          claimants = List(
            TFCClaimant(
              currentIncome = Some(TFCIncome(Some(12212), Some(47674), Some(647864))),
              isPartner = false,
              disability = TFCDisability(),
              carersAllowance = true,
              minimumEarnings = TFCMinimumEarnings(selection = false, amount = 0.0),
              age = Some(AgeRangeEnum.EIGHTEENTOTWENTY.toString),
              employmentStatus = Some(EmploymentStatusEnum.SELFEMPLOYED.toString),
              selfEmployedSelection = Some(true)
            ),
            TFCClaimant(
              currentIncome = Some(TFCIncome(Some(12212), Some(47674), Some(647864))),
              isPartner = true,
              disability = TFCDisability(),
              carersAllowance = false,
              minimumEarnings = TFCMinimumEarnings(selection = true, amount = 0.0),
              age = Some(AgeRangeEnum.EIGHTEENTOTWENTY.toString),
              employmentStatus = None,
              selfEmployedSelection = None
            )
          ),
          children = List(
            TFCChild(
              id = 0,
              childcareCost = 0,
              childcareCostPeriod = Periods.Monthly,
              dob = dob,
              disability = TFCDisability(disabled = true)
            ),
            TFCChild(
              id = 1,
              childcareCost = 1000,
              childcareCostPeriod = Periods.Monthly,
              dob = dob,
              disability = TFCDisability(disabled = true)
            )
          )
        )

        when(mockTFC.tfcNoOfPeriods).thenReturn(4.toShort)
        when(mockTFC.config.startDate).thenReturn(currentDate)

        val output = SUT.convert(hhModel)
        output.isInstanceOf[TFCEligibilityInput] shouldBe true
        output shouldBe expectedOutput
      }
    }
  }

}
