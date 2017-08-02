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
import org.mockito.Mockito.when
import org.scalatest.mock.MockitoSugar
import models.input.esc._
import models.mappings._
import org.joda.time.LocalDate
import spec.CCConfigSpec
import uk.gov.hmrc.play.test.UnitSpec
import utils.{CCConfig, Periods}

class HHToESCEligibilityInputSpec extends UnitSpec
  with MockitoSugar
  with FakeCCEligibilityApplication
  with CCConfigSpec {

  val SUT = HHToESCEligibilityInput

  "ESCMapping" should {

    val mockObject = new HHToESCEligibilityInput {
      override val cCConfig: CCConfig = mock[CCConfig]
    }

    "have reference to CCConfig" in {
      HHToESCEligibilityInput.cCConfig.isInstanceOf[CCConfig] shouldBe true
    }

    "convert periodEnum to Periods for ESCEligibilityInput" when {
      "periodEnum is weekly"in {
        PeriodEnumToPeriod.convert(PeriodEnum.WEEKLY) shouldBe Periods.Weekly
      }

      "periodEnum is fortnightly"in {
        PeriodEnumToPeriod.convert(PeriodEnum.FORTNIGHTLY) shouldBe Periods.Fortnightly
      }

      "periodEnum is yearly"in {
        PeriodEnumToPeriod.convert(PeriodEnum.YEARLY) shouldBe Periods.Yearly
      }

      "periodEnum is quarterly"in {
        PeriodEnumToPeriod.convert(PeriodEnum.QUARTERLY) shouldBe Periods.Quarterly
      }

      "periodEnum is invalid"in {
        PeriodEnumToPeriod.convert(PeriodEnum.INVALID) shouldBe Periods.INVALID
      }
    }

    "convert Household into ESCEligibilityInput" when {
      "given a household with parent and no partner no children" in {

        val household = Household( children = Nil, hasPartner = false,
          parent = Claimant(escVouchers = Some(YesNoUnsureBothEnum.YES)),
          partner = None)

        val res = ESCEligibilityInput(List(
          ESCTaxYear(LocalDate.now(),
            LocalDate.parse("2018-04-06"),
            List(ESCClaimant(false,true)),List()),
          ESCTaxYear(LocalDate.parse("2018-04-06"),
            LocalDate.now().plusYears(1),
            List(ESCClaimant(false,true)),List())))

        when(mockObject.cCConfig.StartDate).thenReturn(LocalDate.now())

        mockObject.convert(household) shouldBe res
      }

      "given a household with parent and a partner with no children" in {

        val household = Household(children = Nil, hasPartner = true,
          parent = Claimant(escVouchers = Some(YesNoUnsureBothEnum.NOTSURE)),
          partner = Some(Claimant(escVouchers = Some(YesNoUnsureBothEnum.NO))))

        val res = ESCEligibilityInput(List(
          ESCTaxYear(LocalDate.now(),
            LocalDate.parse("2018-04-06"),
            List(ESCClaimant(false,true), ESCClaimant(true,false)),List()),
          ESCTaxYear(LocalDate.parse("2018-04-06"),
            LocalDate.now().plusYears(1),
            List(ESCClaimant(false,true), ESCClaimant(true,false)),List())))

        when(mockObject.cCConfig.StartDate).thenReturn(LocalDate.now())

        mockObject.convert(household) shouldBe res
      }

      "given a household with parent, a partner and children with startDate after April 6" in {
        val currentDate = LocalDate.parse("2017-08-01")
        val dob = LocalDate.now().minusYears(2)

        val children = List(
          Child(
            1,
            "Child1",
            Some(dob),
            Some(Disability(true, false, false)),
            Some(ChildCareCost(period = Some(PeriodEnum.MONTHLY)))
          )
        )

        val household = Household(
          children = children,
          hasPartner = true,
          parent = Claimant(
            escVouchers = Some(YesNoUnsureBothEnum.YES)
          ),
          partner = Some(
            Claimant(
              escVouchers = Some(YesNoUnsureBothEnum.NO)
            )
          )
        )

        val res = ESCEligibilityInput(
          List(
            ESCTaxYear(
              currentDate,
              LocalDate.parse("2018-04-06"),
              List(
                ESCClaimant(false,true),
                ESCClaimant(true,false)
              ),
              List(
                ESCChild(
                  1,
                  dob,
                  0,
                  Periods.Monthly,
                  ESCDisability(true,false)
                )
              )
            ),
            ESCTaxYear(
              LocalDate.parse("2018-04-06"),
              currentDate.plusYears(1),
              List(
                ESCClaimant(false,true),
                ESCClaimant(true,false)
              ),
              List(
                ESCChild(
                  1,
                  dob,
                  0,
                  Periods.Monthly,
                  ESCDisability(true,false)
                )
              )
            )
          )
        )

        when(mockObject.cCConfig.StartDate).thenReturn(currentDate)

        mockObject.convert(household) shouldBe res
      }

      "given a household with parent, a partner and children with startDate as before April 6" in {
        val currentDate = LocalDate.parse("2017-03-06")
        val dob = LocalDate.now().minusYears(2)

        val children = List(
          Child(
            1,
            "Child1",
            Some(dob),
            Some(Disability(true, false, false)),
            Some(ChildCareCost(period = None))
          )
        )

        val household = Household(
          children = children,
          hasPartner = true,
          parent = Claimant(escVouchers = Some(YesNoUnsureBothEnum.YES)),
          partner = Some(
            Claimant(escVouchers = Some(YesNoUnsureBothEnum.NO))
          )
        )

        val res = ESCEligibilityInput(
          List(
            ESCTaxYear(
              currentDate,
              LocalDate.parse("2017-04-06"),
              List(ESCClaimant(false,true),ESCClaimant(true,false)),
              List(ESCChild(1, dob, 0, Periods.Monthly, ESCDisability(true,false)))
            ),
            ESCTaxYear(
              LocalDate.parse("2017-04-06"),
              currentDate.plusYears(1),
              List(ESCClaimant(false,true), ESCClaimant(true,false)),
              List(ESCChild(1, dob, 0, Periods.Monthly, ESCDisability(true,false))))
          )
        )

        when(mockObject.cCConfig.StartDate).thenReturn(currentDate)

        mockObject.convert(household) shouldBe res
      }
    }

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
          List(ESCClaimant(false,true), ESCClaimant(true,true)),List(ESCChild(0,LocalDate.parse("2016-08-31", formatter),350,Periods.Monthly,ESCDisability(true,false)),
            ESCChild(1,LocalDate.parse("2016-08-31", formatter),1000,Periods.Monthly,ESCDisability(true,false)))),
        ESCTaxYear(LocalDate.parse("2018-04-06", formatter),LocalDate.parse("2018-08-02", formatter),
          List(ESCClaimant(false,true), ESCClaimant(true,true)),List(ESCChild(0,LocalDate.parse("2016-08-31", formatter),350,Periods.Monthly,ESCDisability(true,false)),
            ESCChild(1,LocalDate.parse("2016-08-31", formatter),1000,Periods.Monthly,ESCDisability(true,false))))))

      val output = SUT.convert(hhModel)
      output.isInstanceOf[ESCEligibilityInput] shouldBe true
      output shouldBe expectedOutput
    }
  }
}
