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

import models.PeriodEnum
import models.mappings.PeriodEnumToPeriod
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import utils.Periods

class PeriodEnumToPeriodSpec extends AnyWordSpec with Matchers{

  val SUT = PeriodEnumToPeriod

  "PeriodEnumToPeriod" must {

    "convert periodEnum to Periods for ESCEligibilityInput" when {
      "periodEnum is weekly" in {
        SUT.convert(PeriodEnum.WEEKLY) shouldBe Periods.Weekly
      }

      "periodEnum is fortnightly" in {
        SUT.convert(PeriodEnum.FORTNIGHTLY) shouldBe Periods.Fortnightly
      }

      "periodEnum is yearly" in {
        SUT.convert(PeriodEnum.YEARLY) shouldBe Periods.Yearly
      }

      "periodEnum is quarterly" in {
        SUT.convert(PeriodEnum.QUARTERLY) shouldBe Periods.Quarterly
      }

      "periodEnum is invalid" in {
        SUT.convert(PeriodEnum.INVALID) shouldBe Periods.INVALID
      }
    }
  }
}
