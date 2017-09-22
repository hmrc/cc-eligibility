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
import models.LocationEnum.LocationEnum
import models._
import models.input.freeEntitlement.FreeEntitlementEligibilityInput
import models.mappings.HHToFree30hoursEligibilityInput
import uk.gov.hmrc.play.test.UnitSpec

class HHToFree30hoursEligibilityInputSpec extends UnitSpec with FakeCCEligibilityApplication {

  val SUT = new HHToFree30hoursEligibilityInput {
  }

  "HHToFree30hoursEligibilityInput" should {

    def buildHousehold(child3Or4: Option[Boolean] = None, theLocation: Option[LocationEnum] = None): Household = {
      Household(
        children = Nil,
        location = theLocation,
        childAgedThreeOrFour = child3Or4,
        parent = Claimant()
      )
    }

    "convert Household into FreeEntitlementEligibilityInput" when {

      "a household in england and having 3 or 4 years child" in {

        val household = buildHousehold(
          child3Or4 = Some(true),
          theLocation = Some(LocationEnum.ENGLAND)
        )

        SUT.convert(household) shouldBe FreeEntitlementEligibilityInput("england", Nil, Some(true))
      }

      "a household with england location and no to child of 3 or 4 years" in {

        val household = buildHousehold(
          child3Or4 = Some(false),
          theLocation = Some(LocationEnum.ENGLAND)
        )

        SUT.convert(household) shouldBe FreeEntitlementEligibilityInput("england", Nil, Some(false))
      }

      "a household with non england location and no to child of 3 or 4 years" in {

        val household = buildHousehold(
          child3Or4 = Some(false),
          theLocation = Some(LocationEnum.SCOTLAND)
        )

        SUT.convert(household) shouldBe FreeEntitlementEligibilityInput("scotland", Nil, Some(false))
      }

      "a household with non england location and no child" in {

        val household = buildHousehold(
          child3Or4 = None,
          theLocation = Some(LocationEnum.WALES)
        )

        SUT.convert(household) shouldBe FreeEntitlementEligibilityInput("wales", Nil, None)
      }

    }
  }
}
