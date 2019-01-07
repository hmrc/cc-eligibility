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
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito.when
import org.scalatest.mockito.MockitoSugar
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table
import play.api.Configuration

class TFCSchemeConfigSpec extends FakeCCEligibilityApplication with MockitoSugar {

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

    "determine number of periods for TFC" in {

      val testObj = new TFCConfig {
        override val conf: Configuration = mock[Configuration]
      }

      when(
        testObj.conf.getInt(anyString())
      ).thenReturn(
        None
      )

      val result = testObj.tfcNoOfPeriods
      result shouldBe 4
    }

    "get default Tax Year Config" in {
      val configs : Seq[play.api.Configuration] = app.configuration.getConfigSeq("tfc.rule-change").get
      val defaultConfig = TFCConfig.getTFCConfigDefault(configs)

      val resultTaxYearConfig = TFCTaxYearConfig(
        childAgeLimit = defaultConfig.getInt("child-age-limit").get,
        childAgeLimitDisabled = defaultConfig.getInt("child-age-limit-disabled").get,
        minimumHoursWorked = defaultConfig.getDouble("minimum-hours-worked-per-week").get,
        maxIncomePerClaimant = defaultConfig.getDouble("maximum-income-per-claimant").get,
        personalAllowancePerClaimant = defaultConfig.getDouble("default.personal-allowance").get,
        nmwApprentice = defaultConfig.getInt("nmw.apprentice").get,
        nmwUnder18 = defaultConfig.getInt("nmw.under-18").get,
        nmw18To20 = defaultConfig.getInt("nmw.18-20").get,
        nmw21To24 = defaultConfig.getInt("nmw.21-24").get,
        nmw25Over = defaultConfig.getInt("nmw.over-25").get
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
      ("test", "date", "childAgeLimit", "childAgeLimitDisabled", "minimumHoursWorked", "maxIncomePerClaimant", "personalAllowancePerClaimant",
      "nmwApprentice", "nmwUnder18", "nmw18To20", "nmw21To24", "nmwOver25"),
      ("default tax year rule", "01-01-2015", 11, 16, 16.00, 100000.00, 11500.00, 56, 64, 89, 112, 120),
      ("2019 tax year rule as 2018", "01-01-2019", 11, 16, 16.00, 100000.00, 11500.00, 56, 64, 89, 112, 120),
      ("2018 tax year rule", "01-08-2018", 11, 16, 16.00, 100000.00, 11500.00, 56, 64, 89, 112, 120),
      ("2018 tax year rule on the date of change", "06-04-2018", 11, 16, 16.00, 100000.00, 11500.00, 56, 64, 89, 112, 120),
      ("2017 tax year rule", "01-08-2017", 11, 16, 16.00, 100000.00, 11500.00, 56, 64, 89, 112, 120),
      ("2017 tax year rule on the date of change", "06-04-2017", 11, 16, 16.00, 100000.00, 11500.00, 56, 64, 89, 112, 120),
      ("2016 tax year rule", "01-08-2016", 11, 16, 16.00, 100000.00, 11000.00, 54, 64, 88, 111, 115),
      ("2016 tax year rule on the date of change", "06-04-2016", 11, 16, 16.00, 100000.00, 11000.00, 54, 64, 88, 111, 115)
    )

    forAll(testCases) { case (test, date, childAgeLimit, childAgeLimitDisabled, minimumHoursWorked, maxIncomePerClaimant, personalAllowancePerClaimant,
      nmwApprentice, nmwUnder18, nmw18To20, nmw21To24, nmwOver25) =>
      s"return ${test} (date: ${date} childAgeLimit: ${childAgeLimit} childAgeLimitDisabled: ${childAgeLimitDisabled} minimumHoursWorked: " +
        s"${minimumHoursWorked} maxIncomePerClaimant: ${maxIncomePerClaimant} personalAllowancePerClaimant: ${personalAllowancePerClaimant} " +
        s"nmwApprentice: ${nmwApprentice} nmwUnder18: ${nmwUnder18} nmw18To20: ${nmw18To20} nmw21To24: ${nmw21To24} nmwOver25: ${nmwOver25})" in {
        val pattern = "dd-MM-yyyy"
        val formatter = DateTimeFormat.forPattern(pattern)
        val current = LocalDate.parse(date, formatter)

        val result = TFCConfig.getConfig(current, "england")
        result.childAgeLimit shouldBe childAgeLimit
        result.childAgeLimitDisabled shouldBe childAgeLimitDisabled
        result.minimumHoursWorked shouldBe minimumHoursWorked
        result.maxIncomePerClaimant shouldBe maxIncomePerClaimant
        result.personalAllowancePerClaimant shouldBe personalAllowancePerClaimant
        result.nmwApprentice shouldBe nmwApprentice
        result.nmwUnder18 shouldBe nmwUnder18
        result.nmw18To20 shouldBe nmw18To20
        result.nmw21To24 shouldBe nmw21To24
        result.nmw25Over shouldBe nmwOver25
      }
    }
  }
}
