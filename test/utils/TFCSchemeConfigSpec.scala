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

package utils


import controllers.FakeCCEligibilityApplication
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.Play
import spec.CCSpecConfig

class TFCSchemeConfigSpec extends CCSpecConfig with FakeCCEligibilityApplication {

  "TFC Scheme Config" should {

    "return 1st september date for current tax year date" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val from = LocalDate.parse("2016-06-20", formatter)

      TFCConfig.september1stForDate(from) shouldBe LocalDate.parse("2016-09-01", formatter)
    }

    "return prior 1st september date for current tax year date" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val from = LocalDate.parse("2016-06-20", formatter)

      TFCConfig.previousSeptember1stForDate(from) shouldBe LocalDate.parse("2015-09-01", formatter)
    }

    "get default Tax Year Config" in {
      val configs : Seq[play.api.Configuration] = Play.application.configuration.getConfigSeq("tfc.rule-change").get
      val defaultConfig = TFCConfig.getTFCConfigDefault(configs)

      val resultTaxYearConfig = TFCTaxYearConfig(
        childAgeLimit = defaultConfig.getInt("child-age-limit").get,
        childAgeLimitDisabled = defaultConfig.getInt("child-age-limit-disabled").get,
        minimumHoursWorked = defaultConfig.getDouble("minimum-hours-worked-per-week").get,
        maxIncomePerClaimant = defaultConfig.getDouble("maximum-income-per-claimant").get
      )

      resultTaxYearConfig.childAgeLimit shouldBe 11
      resultTaxYearConfig.childAgeLimitDisabled shouldBe 16
      resultTaxYearConfig.minimumHoursWorked shouldBe 16.00
      resultTaxYearConfig.maxIncomePerClaimant shouldBe 100000.00
    }

    "return default tax year rule" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("01-01-2015", formatter)

      val result = TFCConfig.getConfig(current)
      result.childAgeLimit shouldBe 11
      result.childAgeLimitDisabled shouldBe 16
      result.minimumHoursWorked shouldBe 16.00
      result.maxIncomePerClaimant shouldBe 100000.00
    }

    "return 2019 tax year rule as 2018" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("01-01-2019", formatter)

      val result = TFCConfig.getConfig(current)
      result.childAgeLimit shouldBe 11
      result.childAgeLimitDisabled shouldBe 16
      result.minimumHoursWorked shouldBe 16.00
      result.maxIncomePerClaimant shouldBe 100000.00
    }

    "return 2018 tax year rule" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("01-08-2018", formatter)

      val result = TFCConfig.getConfig(current)
      result.childAgeLimit shouldBe 11
      result.childAgeLimitDisabled shouldBe 16
      result.minimumHoursWorked shouldBe 16.00
      result.maxIncomePerClaimant shouldBe 100000.00
    }

    "return 2018 tax year rule on the date of change" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("06-04-2018", formatter)

      val result = TFCConfig.getConfig(current)
      result.childAgeLimit shouldBe 11
      result.childAgeLimitDisabled shouldBe 16
      result.minimumHoursWorked shouldBe 16.00
      result.maxIncomePerClaimant shouldBe 100000.00
    }

    "return 2017 tax year rule" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("01-08-2017", formatter)

      val result = TFCConfig.getConfig(current)
      result.childAgeLimit shouldBe 11
      result.childAgeLimitDisabled shouldBe 16
      result.minimumHoursWorked shouldBe 16.00
      result.maxIncomePerClaimant shouldBe 100000.00
    }

    "return 2017 tax year rule on the date of change" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("06-04-2017", formatter)

      val result = TFCConfig.getConfig(current)
      result.childAgeLimit shouldBe 11
      result.childAgeLimitDisabled shouldBe 16
      result.minimumHoursWorked shouldBe 16.00
      result.maxIncomePerClaimant shouldBe 100000.00
    }

    "return 2016 tax year rule" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("01-08-2016", formatter)

      val result = TFCConfig.getConfig(current)
      result.childAgeLimit shouldBe 11
      result.childAgeLimitDisabled shouldBe 16
      result.minimumHoursWorked shouldBe 16.00
      result.maxIncomePerClaimant shouldBe 100000.00
    }

    "return 2016 tax year rule on the date of change" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("06-04-2016", formatter)

      val result = TFCConfig.getConfig(current)
      result.childAgeLimit shouldBe 11
      result.childAgeLimitDisabled shouldBe 16
      result.minimumHoursWorked shouldBe 16.00
      result.maxIncomePerClaimant shouldBe 100000.00
    }
  }
}
