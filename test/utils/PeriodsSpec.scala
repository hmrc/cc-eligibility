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

    "convert Periods.Monthly to Json" in {
      val monthly = Periods.Monthly
      Json.toJson(monthly) shouldBe JsString("Month")
    }

    "convert to Periods.Monthly" in {
      val monthly = "monthly"
      Periods.toPeriod(monthly) shouldBe Periods.Monthly
    }

    "convert to Periods.Quarterly" in {
      val quarterly = "3-monthly"
      Periods.toPeriod(quarterly) shouldBe Periods.Quarterly
    }

    "return INVALID for an incorrect Period" in {
      val invalid = "invalid"
      Periods.toPeriod(invalid) shouldBe Periods.INVALID
    }

    "convert Weekly to string" in {
      val weekly = Periods.Weekly
      Periods.toString(weekly) shouldBe getMessages("cc.period.weekly")
    }

    "convert Fortnightly to string" in {
      val fortnightly = Periods.Fortnightly
      Periods.toString(fortnightly) shouldBe getMessages("cc.period.fortnightly")
    }

    "convert Monthly to string" in {
      val monthly = Periods.Monthly
      Periods.toString(monthly) shouldBe getMessages("cc.period.monthly")
    }

    "convert Quarterly to string" in {
      val quarterly = Periods.Quarterly
      Periods.toString(quarterly) shouldBe getMessages("cc.period.3monthly")
    }

    "convert Yearly to string" in {
      val yearly = Periods.Yearly
      Periods.toString(yearly) shouldBe getMessages("cc.period.yearly")
    }

    "convert INVALID to string" in {
      val invalid = Periods.INVALID
      Periods.toString(invalid) shouldBe getMessages("cc.period.invalid")
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
