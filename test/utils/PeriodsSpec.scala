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

package utils

import controllers.FakeCCEligibilityApplication
import play.api.libs.json.{JsString, Json}

/** Created by adamconder on 09/06/15.
  */
class PeriodsSpec extends FakeCCEligibilityApplication {

  "Periods" must {

    "convert Periods.Weekly to Json" in {
      val weekly = Periods.Weekly
      Json.toJson(weekly) shouldBe JsString("Week")
    }

    "convert Periods.Fortnightly to Json" in {
      val fortnightly = Periods.Fortnightly
      Json.toJson(fortnightly) shouldBe JsString("Fortnight")
    }

    "convert Periods.Monthly to Json" in {
      val monthly = Periods.Monthly
      Json.toJson(monthly) shouldBe JsString("Month")
    }

    "convert Periods.Quarterly to Json" in {
      val quarterly = Periods.Quarterly
      Json.toJson(quarterly) shouldBe JsString("3 month")
    }

    "convert Periods.Yearly to Json" in {
      val yearly = Periods.Yearly
      Json.toJson(yearly) shouldBe JsString("Year")
    }

    "convert Periods.INVALID to Json" in {
      val invalid = Periods.INVALID
      Json.toJson(invalid) shouldBe JsString("INVALID")
    }

  }

}
