/*
 * Copyright 2019 HM Revenue & Customs
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
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat

class ESCSchemeConfigSpec extends FakeCCEligibilityApplication {

  "ESC Scheme Config" should {

    "return 1st september date for current tax year date (as ESC TAX YEAR)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val from = LocalDate.parse("2016-01-20", formatter)
      val until = LocalDate.parse("2016-04-05", formatter)

      val ty = models.input.esc.ESCTaxYear(
        from = from,
        until = until,
        claimants = List(),
        children = List()
      )
      ESCConfig.september1stForDate(ty.from) shouldBe LocalDate.parse("2015-09-01", formatter)
    }

    "return prior 1st september date for current tax year date (as ESC TAX YEAR)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val from = LocalDate.parse("2015-10-20", formatter)
      val until = LocalDate.parse("2016-04-05", formatter)

      val ty = models.input.esc.ESCTaxYear(
        from = from,
        until = until,
        claimants = List(),
        children = List()
      )
      ESCConfig.previousSeptember1stForDate(ty.from) shouldBe LocalDate.parse("2014-09-01", formatter)
    }

    "get default Tax Year Config" in {
      val configs : Seq[play.api.Configuration] = app.configuration.getConfigSeq("esc.rule-change").get
      val defaultConfig = ESCConfig.getESCConfigDefault(configs)

      val resultTaxYearConfig = ESCTaxYearConfig(
        childAgeLimit = defaultConfig.getInt("child-age-limit").get,
        childAgeLimitDisabled = defaultConfig.getInt("child-age-limit-disabled").get
      )

      resultTaxYearConfig.childAgeLimit shouldBe 15
      resultTaxYearConfig.childAgeLimitDisabled shouldBe 16
    }

    "return default tax year rule" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("01-01-2014", formatter)

      val result = ESCConfig.getConfig(current)

      result.childAgeLimit shouldBe 15
      result.childAgeLimitDisabled shouldBe 16

    }

    "return 2018 tax year rule" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("06-04-2018", formatter)

      val result = ESCConfig.getConfig(current)

      result.childAgeLimit shouldBe 15
      result.childAgeLimitDisabled shouldBe 16
    }

    "return 2018 tax year rule as 2017" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("01-01-2018", formatter)

      val result = ESCConfig.getConfig(current)

      result.childAgeLimit shouldBe 15
      result.childAgeLimitDisabled shouldBe 16
    }

    "return 2017 tax year rule" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("01-08-2017", formatter)

      val result = ESCConfig.getConfig(current)

      result.childAgeLimit shouldBe 15
      result.childAgeLimitDisabled shouldBe 16
    }

    "return 2017 tax year rule on the date of change" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("06-04-2017", formatter)

      val result = ESCConfig.getConfig(current)

      result.childAgeLimit shouldBe 15
      result.childAgeLimitDisabled shouldBe 16
    }

    "return 2016 tax year rule" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("01-08-2016", formatter)

      val result = ESCConfig.getConfig(current)

      result.childAgeLimit shouldBe 15
      result.childAgeLimitDisabled shouldBe 16
    }

    "return 2016 tax year rule on the date of change" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("06-04-2016", formatter)

      val result = ESCConfig.getConfig(current)

      result.childAgeLimit shouldBe 15
      result.childAgeLimitDisabled shouldBe 16
    }

    "return 2015 tax year rule" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("01-08-2015", formatter)

      val result = ESCConfig.getConfig(current)

      result.childAgeLimit shouldBe 15
      result.childAgeLimitDisabled shouldBe 16
    }

    "return 2015 tax year rule on the date of change" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("06-04-2015", formatter)

      val result = ESCConfig.getConfig(current)

      result.childAgeLimit shouldBe 15
      result.childAgeLimitDisabled shouldBe 16
    }
  }
}
