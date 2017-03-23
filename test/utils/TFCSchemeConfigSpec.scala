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
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

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
        maxIncomePerClaimant = defaultConfig.getDouble("maximum-income-per-claimant").get,
        personalAllowancePerClaimant = defaultConfig.getDouble("personal-allowance").get,
        nmwApprentice = defaultConfig.getInt("nmw.apprentice").get,
        nmwUnder18 = defaultConfig.getInt("nmw.apprentice").get,
        nmw18To20 = defaultConfig.getInt("nmw.apprentice").get,
        nmw21To24 = defaultConfig.getInt("nmw.apprentice").get,
        nmw25Over = defaultConfig.getInt("nmw.apprentice").get
      )

      resultTaxYearConfig.childAgeLimit shouldBe 11
      resultTaxYearConfig.childAgeLimitDisabled shouldBe 16
      resultTaxYearConfig.minimumHoursWorked shouldBe 16.00
      resultTaxYearConfig.maxIncomePerClaimant shouldBe 100000.00
      resultTaxYearConfig.personalAllowancePerClaimant shouldBe 11500.00
      resultTaxYearConfig.nmwApprentice shouldBe 56
      resultTaxYearConfig.nmwUnder18 shouldBe 64
      resultTaxYearConfig.nmw18To20 shouldBe 89
      resultTaxYearConfig.nmw21To24 shouldBe 112
      resultTaxYearConfig.nmw25Over shouldBe 120
    }

    val testCases = Table(
      ("test", "date", "childAgeLimit", "childAgeLimitDisabled", "minimumHoursWorked", "maxIncomePerClaimant", "personalAllowancePerClaimant"),
      ("default tax year rule", "01-01-2015", 11, 16, 16.00, 100000.00, 11500.00),
      ("2019 tax year rule as 2018", "01-01-2019", 11, 16, 16.00, 100000.00, 11500.00),
      ("2018 tax year rule", "01-08-2018", 11, 16, 16.00, 100000.00, 11500.00),
      ("2018 tax year rule on the date of change", "06-04-2018", 11, 16, 16.00, 100000.00, 11500.00),
      ("2017 tax year rule", "01-08-2017", 11, 16, 16.00, 100000.00, 11500.00),
      ("2017 tax year rule on the date of change", "06-04-2017", 11, 16, 16.00, 100000.00, 11500.00),
      ("2016 tax year rule", "01-08-2016", 11, 16, 16.00, 100000.00, 11000.00),
      ("2016 tax year rule on the date of change", "06-04-2016", 11, 16, 16.00, 100000.00, 11000.00)
    )

    forAll(testCases) { case (test, date, childAgeLimit, childAgeLimitDisabled, minimumHoursWorked, maxIncomePerClaimant, personalAllowancePerClaimant) =>
      s"return ${test} (date: ${date} childAgeLimit: ${childAgeLimit} childAgeLimitDisabled: ${childAgeLimitDisabled} minimumHoursWorked: ${minimumHoursWorked} maxIncomePerClaimant: ${maxIncomePerClaimant} personalAllowancePerClaimant: ${personalAllowancePerClaimant})" in {
        val pattern = "dd-MM-yyyy"
        val formatter = DateTimeFormat.forPattern(pattern)
        val current = LocalDate.parse(date, formatter)

        val result = TFCConfig.getConfig(current)
        result.childAgeLimit shouldBe childAgeLimit
        result.childAgeLimitDisabled shouldBe childAgeLimitDisabled
        result.minimumHoursWorked shouldBe minimumHoursWorked
        result.maxIncomePerClaimant shouldBe maxIncomePerClaimant
        result.personalAllowancePerClaimant shouldBe personalAllowancePerClaimant
      }
    }
  }
}
