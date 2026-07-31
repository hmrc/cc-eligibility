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

import models.PeriodEnum
import models.mappings.PeriodEnumToPeriod
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import utils.Period

class PeriodEnumToPeriodSpec extends AnyWordSpec with Matchers {

  val SUT = PeriodEnumToPeriod

  "PeriodEnumToPeriod" must {

    "convert periodEnum to Period for ESCEligibilityInput" when {
      "periodEnum is weekly" in {
        SUT.convert(PeriodEnum.WEEKLY) shouldBe Period.Weekly
      }

      "periodEnum is fortnightly" in {
        SUT.convert(PeriodEnum.FORTNIGHTLY) shouldBe Period.Fortnightly
      }

      "periodEnum is monthly" in {
        SUT.convert(PeriodEnum.MONTHLY) shouldBe Period.Monthly
      }

      "periodEnum is quarterly" in {
        SUT.convert(PeriodEnum.QUARTERLY) shouldBe Period.Quarterly
      }

      "periodEnum is yearly" in {
        SUT.convert(PeriodEnum.YEARLY) shouldBe Period.Yearly
      }

      "periodEnum is invalid" in {
        SUT.convert(PeriodEnum.INVALID) shouldBe Period.INVALID
      }

      "periodEnum is null" in {
        SUT.convert(null) shouldBe Period.INVALID
      }
    }
  }

}
