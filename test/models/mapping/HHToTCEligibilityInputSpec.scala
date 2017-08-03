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
import models.input.tc._
import models.mappings._
import org.joda.time.LocalDate
import org.mockito.Mockito.when
import org.scalatest.mock.MockitoSugar
import spec.CCConfigSpec
import uk.gov.hmrc.play.test.UnitSpec
import utils.{CCConfig, Periods}

class HHToTCEligibilityInputSpec extends UnitSpec
  with MockitoSugar
  with FakeCCEligibilityApplication
  with CCConfigSpec {

  val SUT = new HHToTCEligibilityInput {
    override val cCConfig: CCConfig = mock[CCConfig]
  }

  "HHToTCEligibilityInput" should {

    "have reference to CCConfig" in {
      SUT.cCConfig.isInstanceOf[CCConfig] shouldBe true
    }

    "convert Household into TCEligibilityInput" when {
      "given a household with parent and no partner no children" in {

        val benefits: Benefits = Benefits(disabilityBenefits = false, highRateDisabilityBenefits =   false, incomeBenefits =   false, carersAllowance = true)

        val household = Household( children = Nil, hasPartner = false, parent = Claimant(benefits = Some(benefits)), partner = None)

        val res = TCEligibilityInput(List(
          TCTaxYear(from = LocalDate.now(),
                    until = LocalDate.parse("2018-04-06"),
                    claimants = List(TCClaimant(0, false, TCDisability(false,false), carersAllowance = true)),
                    children = List[TCChild]()
                   ),
          TCTaxYear(from = LocalDate.parse("2018-04-06"),
                    until = LocalDate.now().plusYears(1),
                    claimants = List(TCClaimant(0, false, TCDisability(false,false),carersAllowance = true)),
                    children = List()))
                    )

        when(SUT.cCConfig.StartDate).thenReturn(LocalDate.now())

        SUT.convert(household) shouldBe res
      }

      "given a household with parent and a partner with no children" in {
        val benefits: Benefits = Benefits(disabilityBenefits = false, highRateDisabilityBenefits =   false, incomeBenefits =   false, carersAllowance = true)

        val household = Household(children = Nil, hasPartner = true,
          parent = Claimant(benefits = Some(benefits)),
          partner = Some(Claimant(benefits = Some(benefits))))

        val res = TCEligibilityInput(List(
          TCTaxYear(LocalDate.now(),
            LocalDate.parse("2018-04-06"),
            List(TCClaimant(0, false, TCDisability(false,false), carersAllowance = true),
              TCClaimant(0, true, TCDisability(false,false), carersAllowance = true)),List()),
          TCTaxYear(LocalDate.parse("2018-04-06"),
            LocalDate.now().plusYears(1),
            List(TCClaimant(0, false, TCDisability(false,false), carersAllowance = true),
              TCClaimant(0, true, TCDisability(false,false), carersAllowance = true)),List())))

        when(SUT.cCConfig.StartDate).thenReturn(LocalDate.now())

        SUT.convert(household) shouldBe res
      }

      "given a household with parent, a partner and children with startDate after April 6" in {
        val currentDate = LocalDate.parse("2017-08-01")
        val dob = LocalDate.now().minusYears(2)
        val benefits: Benefits = Benefits(disabilityBenefits = false, highRateDisabilityBenefits =   false, incomeBenefits =   false, carersAllowance = true)
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
          parent = Claimant(benefits = Some(benefits)),
          partner = Some(Claimant(benefits = Some(benefits)))
        )

        val res = TCEligibilityInput(
          List(
            TCTaxYear(
              currentDate,
              LocalDate.parse("2018-04-06"),
              List(
                TCClaimant(0, false, TCDisability(false,false), carersAllowance = true),
                TCClaimant(0, true, TCDisability(false,false), carersAllowance = true)
              ),
              List(
                TCChild(
                  1,
                  0,
                  Periods.Monthly,
                  dob,
                  TCDisability(true,false),
                  Some(TCEducation(false,LocalDate.now()))
                )
              )
            ),
            TCTaxYear(
              LocalDate.parse("2018-04-06"),
              currentDate.plusYears(1),
              List(
                TCClaimant(0, false, TCDisability(false,false), carersAllowance = true),
                TCClaimant(0, true, TCDisability(false,false), carersAllowance = true)
              ),
              List(
                TCChild(
                  1,
                  0,
                  Periods.Monthly,
                  dob,
                  TCDisability(true,false),
                  Some(TCEducation(false,LocalDate.now()))
                )
              )
            )
          )
        )

        when(SUT.cCConfig.StartDate).thenReturn(currentDate)

        SUT.convert(household) shouldBe res
      }

      "given a household with parent, a partner and children with startDate as before April 6" in {
        val currentDate = LocalDate.parse("2017-03-06")
        val dob = LocalDate.now().minusYears(2)

        val benefits: Benefits = Benefits(disabilityBenefits = false, highRateDisabilityBenefits =   false, incomeBenefits =   false, carersAllowance = true)
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
          parent = Claimant(benefits = Some(benefits)),
          partner = Some(Claimant(benefits = Some(benefits)))
        )

        val res = TCEligibilityInput(
          List(
            TCTaxYear(
              currentDate,
              LocalDate.parse("2017-04-06"),
              List(TCClaimant(0, false, TCDisability(false,false), carersAllowance = true),
                TCClaimant(0, true, TCDisability(false,false), carersAllowance = true)),
              List(TCChild(
                1,
                0.0,
                Periods.INVALID,
                dob,
                TCDisability(true,false),
                Some(TCEducation(false,LocalDate.now()))
              ))
            ),
            TCTaxYear(
              LocalDate.parse("2017-04-06"),
              currentDate.plusYears(1),
              List(TCClaimant(0, false, TCDisability(false,false), carersAllowance = true),
                TCClaimant(0, true, TCDisability(false,false), carersAllowance = true)),
              List(TCChild(
                1,
                0.0,
                Periods.INVALID,
                dob,
                TCDisability(true,false),
                Some(TCEducation(false,LocalDate.now()))
              )))
          )
        )

        when(SUT.cCConfig.StartDate).thenReturn(currentDate)

        SUT.convert(household) shouldBe res
      }
    }

    "accept a valid Household model and return a valid TCEligibilityInput model" in {

      val currentDate = LocalDate.parse("2017-08-01")
      val dob = LocalDate.now().minusYears(2)

      val hhChild1 = Child(
        id = 0,
        name = "child1",
        dob = Some(dob),
        disability = Some(Disability(disabled = true, severelyDisabled = false, blind = true)),
        childcareCost = Some(ChildCareCost(amount = Some(350), Some(PeriodEnum.MONTHLY))),
        education = Some(Education(inEducation = false, startDate = Some(LocalDate.now())))
      )
      val hhChild2 = Child(
        id = 1,
        name = "child2",
        dob = Some(dob),
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
          carersAllowance = true
        )),
        lastYearlyIncome = None,
        currentYearlyIncome = Some(Income(employmentIncome = Some(12212),
          pension = Some(47674),
          otherIncome = Some(647864),
          benefits = Some(546),
          statutoryIncome = None
        )),
        hours = Some(40),
        minimumEarnings = None,
        escVouchers = Some(YesNoUnsureBothEnum.YES)
      )
      val partner = Claimant(
        ageRange = Some(AgeRangeEnum.EIGHTEENTOTWENTY),
        benefits = Some(Benefits(
          disabilityBenefits = false,
          highRateDisabilityBenefits = false,
          incomeBenefits = false,
          carersAllowance = true
        )),
        lastYearlyIncome = None,
        currentYearlyIncome = Some(Income(employmentIncome = Some(12212),
          pension = Some(47674),
          otherIncome = Some(647864),
          benefits = Some(546),
          statutoryIncome = None
        )),
        hours = Some(40),
        minimumEarnings = None,
        escVouchers = Some(YesNoUnsureBothEnum.NOTSURE)
      )

      val hhModel = Household(None, Some(LocationEnum.ENGLAND), true, List(hhChild1, hhChild2), parent, Some(partner))

      val expectedOutput = TCEligibilityInput(List(
        TCTaxYear(currentDate,
          LocalDate.parse("2018-04-06"),
          List(TCClaimant(40, false, TCDisability(false,false), carersAllowance = true),
            TCClaimant(40, true, TCDisability(false,false), carersAllowance = true)),
          List(TCChild(
            0,
            350,
            Periods.Monthly,
            dob,
            TCDisability(true,false),
            Some(TCEducation(false,LocalDate.now()))
          ),
            TCChild(
              1,
              1000,
              Periods.Monthly,
              dob,
              TCDisability(true,false),
              Some(TCEducation(false,LocalDate.now()))
            ))),
        TCTaxYear(LocalDate.parse("2018-04-06"),
          currentDate.plusYears(1),
          List(TCClaimant(40, false, TCDisability(false,false), carersAllowance = true),
            TCClaimant(40, true, TCDisability(false,false), carersAllowance = true)),
          List(TCChild(
            0,
            350,
            Periods.Monthly,
            dob,
            TCDisability(true,false),
            Some(TCEducation(false,LocalDate.now()))
          ),
            TCChild(
              1,
              1000,
              Periods.Monthly,
              dob,
              TCDisability(true,false),
              Some(TCEducation(false,LocalDate.now()))
            )))))

      when(SUT.cCConfig.StartDate).thenReturn(currentDate)

      val output = SUT.convert(hhModel)
      output.isInstanceOf[TCEligibilityInput] shouldBe true
      output shouldBe expectedOutput
    }
  }
}
