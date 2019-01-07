/*
 * Copyright 2019 HM Revenue & Customs
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
import org.scalatest.mockito.MockitoSugar
import uk.gov.hmrc.play.test.UnitSpec
import utils.{CCConfig, CCConfigSpec, Periods}

class HHToTCEligibilityInputSpec extends UnitSpec
  with MockitoSugar
  with FakeCCEligibilityApplication
  with CCConfigSpec {

  val SUT = new HHToTCEligibilityInput {
    override val cCConfig: CCConfig = mock[CCConfig]
  }

  val testYear = LocalDate.now().plusYears(1).getYear

  "HHToTCEligibilityInput" should {

    "have reference to CCConfig" in {
      HHToTCEligibilityInput.cCConfig.isInstanceOf[CCConfig] shouldBe true
    }

    "convert Household into TCEligibilityInput" when {
      "given a household with parent and no partner no children" in {

        val currentDate = LocalDate.now
        val fourthApril = LocalDate.parse(s"$testYear-04-06")

        val benefits: Benefits = Benefits(
          disabilityBenefits = false,
          highRateDisabilityBenefits = false,
          incomeBenefits = false,
          carersAllowance = true
        )

        val household = Household(
          children = List.empty,
          parent = Claimant(
            benefits = Some(benefits),
            lastYearlyIncome = Some(Income(employmentIncome = Some(12212),
              pension = Some(47674),
              otherIncome = Some(647864),
              benefits = Some(546),
              statutoryIncome = Some(
                StatutoryIncome(
                  statutoryWeeks = 5,
                  statutoryAmount = 10
                )
              )
            )),
            currentYearlyIncome = None
          ),
          partner = None
        )

        when(SUT.cCConfig.StartDate).thenReturn(currentDate)

        SUT.convert(household) shouldBe TCEligibilityInput(
          List(
            TCTaxYear(
              from = currentDate,
              until = fourthApril,
              previousHouseholdIncome = Some(
                TCIncome(
                  employment = Some(List(12212)),
                  pension = Some(List(47674)),
                  other = Some(List(647864)),
                  benefits = Some(List(546)),
                  statutory = Some(
                    List(
                      TCStatutoryIncome(
                        weeks = 5,
                        amount = 10
                      )
                    )
                  )
                )
              ),
              currentHouseholdIncome = Some(TCIncome()),
              claimants = List(
                TCClaimant(0, false, TCDisability(false, false), carersAllowance = true,incomeBenefits =  false)
              ),
              children = List.empty
            ),
            TCTaxYear(
              from = fourthApril,
              until = currentDate.plusYears(1),
              previousHouseholdIncome = Some(TCIncome()),
              currentHouseholdIncome = Some(TCIncome()),
              claimants = List(
                TCClaimant(0, false, TCDisability(false, false), carersAllowance = true,incomeBenefits =  false)
              ),
              children = List.empty
            )
          )
        )
      }

      "given a household with parent and a partner with no children" in {
        val currentDate = LocalDate.now
        val fourthApril = LocalDate.parse(s"$testYear-04-06")

        val benefits: Benefits = Benefits(
          disabilityBenefits = false,
          highRateDisabilityBenefits = false,
          incomeBenefits = false,
          carersAllowance = true
        )

        val household = Household(
          children = List.empty,
          parent = Claimant(benefits = Some(benefits)),
          partner = Some(Claimant(benefits = Some(benefits)))
        )

        when(SUT.cCConfig.StartDate).thenReturn(currentDate)

        SUT.convert(household) shouldBe TCEligibilityInput(
          List(
            TCTaxYear(
              from = currentDate,
              until = fourthApril,
              previousHouseholdIncome = Some(TCIncome()),
              currentHouseholdIncome = Some(TCIncome()),
              claimants = List(
                TCClaimant(0, false, TCDisability(false, false), carersAllowance = true,incomeBenefits =  false),
                TCClaimant(0, true, TCDisability(false, false), carersAllowance = true,incomeBenefits =  false)
              ),
              children = List()
            ),
            TCTaxYear(
              from = fourthApril,
              until = currentDate.plusYears(1),
              previousHouseholdIncome = Some(TCIncome()),
              currentHouseholdIncome = Some(TCIncome()),
              claimants = List(
                TCClaimant(0, false, TCDisability(false, false), carersAllowance = true,incomeBenefits =  false),
                TCClaimant(0, true, TCDisability(false, false), carersAllowance = true,incomeBenefits =  false)
              ),
              children = List()
            )
          )
        )
      }

      "given a household with parent, a partner and children with startDate after April 6" in {
        val currentDate = LocalDate.parse("2017-08-01")
        val fourthApril = LocalDate.parse("2018-04-06")

        val dob = currentDate.minusYears(2)
        val benefits: Benefits = Benefits(
          disabilityBenefits = true,
          highRateDisabilityBenefits = true,
          incomeBenefits = false,
          carersAllowance = true
        )
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
          parent = Claimant(benefits = Some(benefits)),
          partner = Some(Claimant(benefits = Some(benefits)))
        )

        val res = TCEligibilityInput(
          List(
            TCTaxYear(
              currentDate,
              LocalDate.parse("2018-04-06"),
              Some(TCIncome()),
              Some(TCIncome()),
              List(
                TCClaimant(0, false, TCDisability(true, true), carersAllowance = true,incomeBenefits =  false),
                TCClaimant(0, true, TCDisability(true, true), carersAllowance = true,incomeBenefits =  false)
              ),
              List(
                TCChild(
                  1,
                  0,
                  Periods.Monthly,
                  dob,
                  TCDisability(true, false),
                  Some(TCEducation(false, LocalDate.now()))
                )
              )
            ),
            TCTaxYear(
              LocalDate.parse("2018-04-06"),
              currentDate.plusYears(1),
              Some(TCIncome()),
              Some(TCIncome()),
              List(
                TCClaimant(0, false, TCDisability(true, true), carersAllowance = true,incomeBenefits =  false),
                TCClaimant(0, true, TCDisability(true, true), carersAllowance = true,incomeBenefits =  false)
              ),
              List(
                TCChild(
                  1,
                  0,
                  Periods.Monthly,
                  dob,
                  TCDisability(true, false),
                  Some(TCEducation(false, LocalDate.now()))
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
        val dob = currentDate.minusYears(2)

        val benefits: Benefits = Benefits(
          disabilityBenefits = false,
          highRateDisabilityBenefits = false,
          incomeBenefits = false,
          carersAllowance = true
        )

        val children = List(
          Child(
            0,
            "Child1",
            Some(dob),
            Some(Disability(true, false, false)),
            Some(ChildCareCost(period = None))
          )
        )

        val household = Household(
          children = children,
          parent = Claimant(benefits = Some(benefits)),
          partner = Some(Claimant(benefits = Some(benefits)))
        )

        val res = TCEligibilityInput(
          List(
            TCTaxYear(
              currentDate,
              LocalDate.parse("2017-04-06"),
              Some(TCIncome()),
              Some(TCIncome()),
              List(
                TCClaimant(0, false, TCDisability(false, false), carersAllowance = true,incomeBenefits =  false),
                TCClaimant(0, true, TCDisability(false, false), carersAllowance = true,incomeBenefits =  false)
              ),
              List(
                TCChild(
                  0,
                  0.0,
                  Periods.Monthly,
                  dob,
                  TCDisability(true, false),
                  Some(TCEducation(false, LocalDate.now()))
                )
              )
            ),
            TCTaxYear(
              LocalDate.parse("2017-04-06"),
              currentDate.plusYears(1),
              Some(TCIncome()),
              Some(TCIncome()),
              List(
                TCClaimant(0, false, TCDisability(false, false), carersAllowance = true,incomeBenefits =  false),
                TCClaimant(0, true, TCDisability(false, false), carersAllowance = true,incomeBenefits =  false)
              ),
              List(
                TCChild(
                  0,
                  0.0,
                  Periods.Monthly,
                  dob,
                  TCDisability(true, false),
                  Some(TCEducation(false, LocalDate.now()))
                )
              )
            )
          )
        )

        when(SUT.cCConfig.StartDate).thenReturn(currentDate)

        SUT.convert(household) shouldBe res
      }
    }


    "given a household with parent, a partner and children (no childcare costs) with startDate after April 6 " in {
      val currentDate = LocalDate.parse("2017-08-01")
      val dob = LocalDate.now().minusYears(2)
      val benefits: Benefits = Benefits(disabilityBenefits = true, highRateDisabilityBenefits = true, incomeBenefits = false, carersAllowance = true)
      val children = List(
        Child(
          1,
          "Child1",
          Some(dob),
          None,
          None
        )
      )

      val household = Household(
        children = children,
        parent = Claimant(benefits = Some(benefits)),
        partner = Some(Claimant(benefits = Some(benefits)))
      )

      val res = TCEligibilityInput(
        List(
          TCTaxYear(
            currentDate,
            LocalDate.parse("2018-04-06"),
            Some(TCIncome()),
            Some(TCIncome()),
            List(
              TCClaimant(0, false, TCDisability(true, true), carersAllowance = true,incomeBenefits =  false),
              TCClaimant(0, true, TCDisability(true, true), carersAllowance = true,incomeBenefits =  false)
            ),
            List(
              TCChild(
                1,
                0.0,
                Periods.Monthly,
                dob,
                TCDisability(false, false),
                Some(TCEducation(false, LocalDate.now()))
              )
            )
          ),
          TCTaxYear(
            LocalDate.parse("2018-04-06"),
            currentDate.plusYears(1),
            Some(TCIncome()),
            Some(TCIncome()),
            List(
              TCClaimant(0, false, TCDisability(true, true), carersAllowance = true,incomeBenefits =  false),
              TCClaimant(0, true, TCDisability(true, true), carersAllowance = true,incomeBenefits =  false)
            ),
            List(
              TCChild(
                1,
                0.0,
                Periods.Monthly,
                dob,
                TCDisability(false, false),
                Some(TCEducation(false, LocalDate.now()))
              )
            )
          )
        )
      )

      when(SUT.cCConfig.StartDate).thenReturn(currentDate)

      SUT.convert(household) shouldBe res
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
        education = Some(Education(inEducation = true, startDate = Some(LocalDate.now())))
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
          statutoryIncome = Some(
            StatutoryIncome(
              statutoryWeeks = 5,
              statutoryAmount = 10
            )
          )
        )),
        hours = Some(40),
        minimumEarnings = None,
        escVouchers = Some(YesNoUnsureEnum.YES)
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
          statutoryIncome = Some(
            StatutoryIncome(
              statutoryWeeks = 5,
              statutoryAmount = 10
            )
          )
        )),
        hours = Some(40),
        minimumEarnings = None,
        escVouchers = Some(YesNoUnsureEnum.NOTSURE)
      )

      val hhModel = Household(None, Some(LocationEnum.ENGLAND), List(hhChild1, hhChild2), parent, Some(partner))

      val expectedOutput = TCEligibilityInput(List(
        TCTaxYear(currentDate,
          LocalDate.parse("2018-04-06"),
          Some(TCIncome()),
          Some(
            TCIncome(
              employment = Some(List(12212, 12212)),
              pension = Some(List(47674, 47674)),
              other = Some(List(647864, 647864)),
              benefits = Some(List(546, 546)),
              statutory = Some(
                List(
                  TCStatutoryIncome(
                    weeks = 5,
                    amount = 10
                  ),
                  TCStatutoryIncome(
                    weeks = 5,
                    amount = 10
                  )
                )
              )
            )
          ),
          List(
            TCClaimant(40, false, TCDisability(false, false), carersAllowance = true,incomeBenefits =  false),
            TCClaimant(40, true, TCDisability(false, false), carersAllowance = true,incomeBenefits =  false)
          ),
          List(TCChild(
            0,
            350,
            Periods.Monthly,
            dob,
            TCDisability(true, false),
            Some(TCEducation(true, LocalDate.now()))
          ),
            TCChild(
              1,
              1000,
              Periods.Monthly,
              dob,
              TCDisability(true, false),
              Some(TCEducation(false, LocalDate.now()))
            ))),
        TCTaxYear(LocalDate.parse("2018-04-06"),
          currentDate.plusYears(1),
          Some(
            TCIncome(
              employment = Some(List(12212, 12212)),
              pension = Some(List(47674, 47674)),
              other = Some(List(647864, 647864)),
              benefits = Some(List(546, 546)),
              statutory = Some(
                List(
                  TCStatutoryIncome(
                    weeks = 5,
                    amount = 10
                  ),
                  TCStatutoryIncome(
                    weeks = 5,
                    amount = 10
                  )
                )
              )
            )
          ),
          Some(
            TCIncome(
              employment = Some(List(12212, 12212)),
              pension = Some(List(47674, 47674)),
              other = Some(List(647864, 647864)),
              benefits = Some(List(546, 546)),
              Some(
                List(
                  TCStatutoryIncome(
                    weeks = 5,
                    amount = 10
                  ),
                  TCStatutoryIncome(
                    weeks = 5,
                    amount = 10
                  )
                )
              )
            )
          ),
          List(TCClaimant(40, false, TCDisability(false, false), carersAllowance = true,incomeBenefits =  false),
            TCClaimant(40, true, TCDisability(false, false), carersAllowance = true,incomeBenefits =  false)),
          List(TCChild(
            0,
            350,
            Periods.Monthly,
            dob,
            TCDisability(true, false),
            Some(TCEducation(true, LocalDate.now()))
          ),
            TCChild(
              1,
              1000,
              Periods.Monthly,
              dob,
              TCDisability(true, false),
              Some(TCEducation(false, LocalDate.now()))
            )))))

      when(SUT.cCConfig.StartDate).thenReturn(currentDate)

      val output = SUT.convert(hhModel)
      output.isInstanceOf[TCEligibilityInput] shouldBe true
      output shouldBe expectedOutput
    }
  }
}
