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
class PeriodSpec extends FakeCCEligibilityApplication {

  "Period" must {

    "convert Period.Weekly to Json" in {
      val weekly = Period.Weekly
      Json.toJson(weekly) shouldBe JsString("Week")
    }

    "convert Period.Fortnightly to Json" in {
      val fortnightly = Period.Fortnightly
      Json.toJson(fortnightly) shouldBe JsString("Fortnight")
    }

    "convert Period.Monthly to Json" in {
      val monthly = Period.Monthly
      Json.toJson(monthly) shouldBe JsString("Month")
    }

    "convert Period.Quarterly to Json" in {
      val quarterly = Period.Quarterly
      Json.toJson(quarterly) shouldBe JsString("3 month")
    }

    "convert Period.Yearly to Json" in {
      val yearly = Period.Yearly
      Json.toJson(yearly) shouldBe JsString("Year")
    }

    "convert Period.INVALID to Json" in {
      val invalid = Period.INVALID
      Json.toJson(invalid) shouldBe JsString("INVALID")
    }

  }

}
