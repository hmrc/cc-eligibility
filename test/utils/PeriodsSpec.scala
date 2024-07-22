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
import play.api.libs.json.{JsError, JsString, Json}

/**
 * Created by adamconder on 09/06/15.
 */
class PeriodsSpec extends FakeCCEligibilityApplication {

  "Periods" must {

    "convert Periods.Weekly to Json" in {
      val monthly = Periods.Weekly
      Json.toJson(monthly) shouldBe JsString("Week")
    }

    "convert Periods.Fortnightly to Json" in {
      val monthly = Periods.Fortnightly
      Json.toJson(monthly) shouldBe JsString("Fortnight")
    }

    "convert Periods.Monthly to Json" in {
      val monthly = Periods.Monthly
      Json.toJson(monthly) shouldBe JsString("Month")
    }

    "convert Periods.Quarterly to Json" in {
      val monthly = Periods.Quarterly
      Json.toJson(monthly) shouldBe JsString("3 month")
    }

    "convert Periods.Yearly to Json" in {
      val monthly = Periods.Yearly
      Json.toJson(monthly) shouldBe JsString("Year")
    }

    "convert Periods.INVALID to Json" in {
      val monthly = Periods.INVALID
      Json.toJson(monthly) shouldBe JsString("INVALID")
    }

  }

  "Enumutils" must {
    "return JsError" in {
      class test extends Enumeration

      val utilRes = EnumUtils.enumFormat(new test).reads(Json.obj("periods" -> "0"))

      utilRes shouldBe JsError("String value expected")
    }
  }

}
