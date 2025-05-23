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

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import org.mockito.Mockito.when
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table
import org.scalatestplus.mockito.MockitoSugar
import play.api.Configuration
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.jdk.CollectionConverters.ListHasAsScala

class TFCSchemeConfigSpec extends FakeCCEligibilityApplication with MockitoSugar {

  "TFC Scheme Config" must {

    "return 1st september date for current tax year date" in {
      val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
      val from      = LocalDate.parse("2016-06-20", formatter)

      tfcConfig.config.september1stForDate(from) shouldBe LocalDate.parse("2016-09-01", formatter)
    }

    "return prior 1st september date for current tax year date" in {
      val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
      val from      = LocalDate.parse("2016-06-20", formatter)

      tfcConfig.config.previousSeptember1stForDate(from) shouldBe LocalDate.parse("2015-09-01", formatter)
    }

    "determine number of periods for TFC" in {

      val mockServicesConf = mock[Configuration]
      val testObj          = new TFCConfig(new CCConfig(mock[ServicesConfig], mockServicesConf))

      when(mockServicesConf.getOptional[Int]("tax.quarters.multiplier")).thenReturn(None)

      val result = testObj.tfcNoOfPeriods
      result shouldBe 4
    }

    "get default Tax Year Config" in {
      val configs: Seq[play.api.Configuration] =
        app.configuration.underlying.getConfigList("tfc.rule-change").asScala.map(Configuration(_)).toSeq
      val defaultConfig = tfcConfig.getTFCConfigDefault(configs)

      val resultTaxYearConfig = TFCTaxYearConfig(
        childAgeLimit = defaultConfig.get[Int]("child-age-limit"),
        childAgeLimitDisabled = defaultConfig.get[Int]("child-age-limit-disabled"),
        minimumHoursWorked = defaultConfig.get[Double]("minimum-hours-worked-per-week"),
        maxIncomePerClaimant = defaultConfig.get[Double]("maximum-income-per-claimant"),
        personalAllowancePerClaimant = defaultConfig.get[Double]("default.personal-allowance")
      )

      resultTaxYearConfig.childAgeLimit shouldBe 11
      resultTaxYearConfig.childAgeLimitDisabled shouldBe 16
      resultTaxYearConfig.minimumHoursWorked shouldBe 16.00
      resultTaxYearConfig.maxIncomePerClaimant shouldBe 100000.00
      resultTaxYearConfig.personalAllowancePerClaimant shouldBe 11500.00
    }

    val testCases = Table(
      (
        "test",
        "date",
        "childAgeLimit",
        "childAgeLimitDisabled",
        "minimumHoursWorked",
        "maxIncomePerClaimant",
        "personalAllowancePerClaimant",
        "nmwApprentice",
        "nmwUnder18",
        "nmw18To20",
        "nmw21To24",
        "nmwOver25"
      ),
      ("default tax year rule", "01-01-2015", 11, 16, 16.00, 100000.00, 11500.00, 56, 64, 89, 112, 120),
      ("2019 tax year rule", "01-08-2019", 11, 16, 16.00, 100000.00, 12500.00, 62, 69, 98, 123, 131),
      ("2019 tax year rule on date of change", "06-04-2019", 11, 16, 16.00, 100000.00, 12500.00, 62, 69, 98, 123, 131),
      ("2019 nmw values on the date of change", "01-04-2019", 11, 16, 16.00, 100000.00, 11500.00, 62, 69, 98, 123, 131),
      ("2018 nmw values", "31-03-2019", 11, 16, 16.00, 100000.00, 11500.00, 59, 67, 94, 118, 125),
      ("2018 tax year rule", "01-08-2018", 11, 16, 16.00, 100000.00, 11500.00, 59, 67, 94, 118, 125),
      (
        "2018 tax year rule on the date of change",
        "06-04-2018",
        11,
        16,
        16.00,
        100000.00,
        11500.00,
        59,
        67,
        94,
        118,
        125
      ),
      ("2017 tax year rule", "01-08-2017", 11, 16, 16.00, 100000.00, 11500.00, 56, 64, 89, 112, 120),
      (
        "2017 tax year rule on the date of change",
        "06-04-2017",
        11,
        16,
        16.00,
        100000.00,
        11500.00,
        56,
        64,
        89,
        112,
        120
      ),
      ("2016 tax year rule", "01-08-2016", 11, 16, 16.00, 100000.00, 11000.00, 54, 64, 88, 111, 115),
      (
        "2016 tax year rule on the date of change",
        "06-04-2016",
        11,
        16,
        16.00,
        100000.00,
        11000.00,
        54,
        64,
        88,
        111,
        115
      )
    )

    forAll(testCases) {
      case (
            test,
            date,
            childAgeLimit,
            childAgeLimitDisabled,
            minimumHoursWorked,
            maxIncomePerClaimant,
            personalAllowancePerClaimant,
            nmwApprentice,
            nmwUnder18,
            nmw18To20,
            nmw21To24,
            nmwOver25
          ) =>
        s"return $test (date: $date childAgeLimit: $childAgeLimit childAgeLimitDisabled: $childAgeLimitDisabled minimumHoursWorked: " +
          s"$minimumHoursWorked maxIncomePerClaimant: $maxIncomePerClaimant personalAllowancePerClaimant: $personalAllowancePerClaimant " +
          s"nmwApprentice: $nmwApprentice nmwUnder18: $nmwUnder18 nmw18To20: $nmw18To20 nmw21To24: $nmw21To24 nmwOver25: $nmwOver25)" in {
            val pattern   = "dd-MM-yyyy"
            val formatter = DateTimeFormatter.ofPattern(pattern)
            val current   = LocalDate.parse(date, formatter)

            val result = tfcConfig.getConfig(current, "england")
            result.childAgeLimit shouldBe childAgeLimit
            result.childAgeLimitDisabled shouldBe childAgeLimitDisabled
            result.minimumHoursWorked shouldBe minimumHoursWorked
            result.maxIncomePerClaimant shouldBe maxIncomePerClaimant
            result.personalAllowancePerClaimant shouldBe personalAllowancePerClaimant
          }
    }
  }

}
