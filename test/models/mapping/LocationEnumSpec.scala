/*
 * Copyright 2022 HM Revenue & Customs
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
import models.LocationEnum
import org.scalatest.wordspec.AnyWordSpec

class LocationEnumSpec extends AnyWordSpec with FakeCCEligibilityApplication {

  "LocationEnum" must {
    "have toString" when {
      "LocationEnum is ENGLAND" in {
        LocationEnum.ENGLAND.toString shouldBe "england"
      }

      "LocationEnum is SCOTLAND" in {
        LocationEnum.SCOTLAND.toString shouldBe "scotland"
      }

      "LocationEnum is WALES" in {
        LocationEnum.WALES.toString shouldBe "wales"
      }

      "LocationEnum is NORTHERNIRELAND" in {
        LocationEnum.NORTHERNIRELAND.toString shouldBe "northern-ireland"
      }
    }
  }

}
