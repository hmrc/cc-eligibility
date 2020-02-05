/*
 * Copyright 2020 HM Revenue & Customs
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
import models.input.freeEntitlement.FreeEntitlementEligibilityInput
import models.mappings.HHToFree30hoursEligibilityInput
import org.joda.time.LocalDate
import uk.gov.hmrc.play.test.UnitSpec

class HHToFree30hoursEligibilityInputSpec extends UnitSpec with FakeCCEligibilityApplication {

  val SUT = new HHToFree30hoursEligibilityInput {}

  "HHToFree30hoursEligibilityInput" should {

    "given a household with location and children having DOB" in {
      val dob1 = LocalDate.parse("2017-08-01")
      val dob2 = LocalDate.parse("2016-08-01")

      val children = List(
        Child(
          1,
          "Child1",
          Some(dob1),
          Some(Disability(true, false, false)),
          Some(ChildCareCost(period = Some(PeriodEnum.MONTHLY)))
        ),
        Child(
          2,
          "Child2",
          Some(dob2),
          Some(Disability(true, false, false)),
          Some(ChildCareCost(period = Some(PeriodEnum.MONTHLY)))
        )
      )

      val household = Household(
        children = children,
        location = Some(LocationEnum.ENGLAND),
        parent = Claimant(
          escVouchers = Some(YesNoUnsureEnum.YES)
        ),
        partner = Some(
          Claimant(
            escVouchers = Some(YesNoUnsureEnum.NO)
          )
        )
      )

      val childDOBList = List(dob1, dob2)

      SUT.convert(household) shouldBe FreeEntitlementEligibilityInput("england", childDOBList)
    }

    "given a household with default location and no children" in {

      val children = List(
        Child(
          1,
          "Child1",
          None,
          Some(Disability(true, false, false)),
          Some(ChildCareCost(period = Some(PeriodEnum.MONTHLY)))
        ),
        Child(
          2,
          "Child2",
          None,
          Some(Disability(true, false, false)),
          Some(ChildCareCost(period = Some(PeriodEnum.MONTHLY)))
        )
      )

      val household = Household(
        children = List(),
        parent = Claimant(
          escVouchers = Some(YesNoUnsureEnum.YES)
        ),
        partner = Some(
          Claimant(
            escVouchers = Some(YesNoUnsureEnum.NO)
          )
        )
      )

      SUT.convert(household) shouldBe FreeEntitlementEligibilityInput("england", Nil)
    }
  }
}
