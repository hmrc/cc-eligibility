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
import models.input.esc._
import models.mappings._
import org.joda.time.LocalDate
import org.mockito.Mockito.when
import org.scalatest.mock.MockitoSugar
import spec.CCConfigSpec
import uk.gov.hmrc.play.test.UnitSpec
import utils.{CCConfig, Periods}

class HHToESCEligibilityInputSpec extends UnitSpec
  with MockitoSugar
  with FakeCCEligibilityApplication
  with CCConfigSpec {

  val SUT =  new HHToESCEligibilityInput {
    override val cCConfig: CCConfig = mock[CCConfig]
  }

  "HHToESCEligibilityInput" should {

    "have reference to CCConfig" in {
      HHToESCEligibilityInput.cCConfig.isInstanceOf[CCConfig] shouldBe true
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

        when(SUT.cCConfig.StartDate).thenReturn(LocalDate.now())

        SUT.convert(household) shouldBe res
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

        when(SUT.cCConfig.StartDate).thenReturn(LocalDate.now())

        SUT.convert(household) shouldBe res
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

        when(SUT.cCConfig.StartDate).thenReturn(currentDate)

        SUT.convert(household) shouldBe res
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

        when(SUT.cCConfig.StartDate).thenReturn(currentDate)

        SUT.convert(household) shouldBe res
      }
    }

  }
}
