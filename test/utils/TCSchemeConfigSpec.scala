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
import models.input.tc.TCTaxYear
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.mockito.Mockito.when
import play.api.Configuration
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import scala.collection.JavaConverters.asScalaBufferConverter

class TCSchemeConfigSpec extends FakeCCEligibilityApplication {

  val mockConfiguration: Configuration = mock[Configuration]
  val ccConfig= new CCConfig(mock[ServicesConfig], mockConfiguration)
  val tccConfig = new TCConfig(ccConfig)

  val getConfig: Seq[Configuration] = Seq(
    Configuration(
      "rule-date" -> "06-04-2018",
      "child-age-limit" -> "15",
      "child-age-limit-disabled" -> "16",
      "young-adult-education-age-limit" -> "19",
      "young-adult-age-limit" -> "20",
      "minimum-hours-worked-per-week" -> "16.00",
      "minimum-hours-worked-if-couple-per-week" -> "24.00",
      "hours-30-worked-per-week" -> "30.00",
      "current-income-fall-difference-amount" -> "2500",
      "current-income-rise-difference-amount" -> "2500"
    ),
    Configuration(
      "rule-date" -> "06-04-2017",
      "child-age-limit" -> "15",
      "child-age-limit-disabled" -> "16",
      "young-adult-education-age-limit" -> "19",
      "young-adult-age-limit" -> "20",
      "minimum-hours-worked-per-week" -> "16.00",
      "minimum-hours-worked-if-couple-per-week" -> "24.00",
      "hours-30-worked-per-week" -> "30.00",
      "current-income-fall-difference-amount" -> "2500",
      "current-income-rise-difference-amount" -> "2500"
    ),
    Configuration(
      "rule-date" -> "06-04-2016",
      "child-age-limit" -> "15",
      "child-age-limit-disabled" -> "16",
      "young-adult-education-age-limit" -> "19",
      "young-adult-age-limit" -> "20",
      "minimum-hours-worked-per-week" -> "16.00",
      "minimum-hours-worked-if-couple-per-week" -> "24.00",
      "hours-30-worked-per-week" -> "30.00",
      "current-income-fall-difference-amount" -> "2500",
      "current-income-rise-difference-amount" -> "2500"
    ),
    Configuration(
      "rule-date" -> "default",
      "child-age-limit" -> "15",
      "child-age-limit-disabled" -> "16",
      "young-adult-education-age-limit" -> "19",
      "young-adult-age-limit" -> "20",
      "minimum-hours-worked-per-week" -> "16.00",
      "minimum-hours-worked-if-couple-per-week" -> "24.00",
      "hours-30-worked-per-week" -> "30.00",
      "current-income-fall-difference-amount" -> "2500",
      "current-income-rise-difference-amount" -> "2500"
    )
  )

  val configuration: Configuration = Configuration("tc" -> Map("rule-change" -> getConfig.map(_.entrySet.toMap)))
  when(mockConfiguration.underlying).thenReturn(configuration.underlying)

  "TCSchemeConfig" must {

    "return 1st september date for current tax year date (as TC TAX YEAR)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val from = LocalDate.parse("2015-06-20", formatter)
      val until = LocalDate.parse("2016-04-05", formatter)

      val ty = TCTaxYear(
        from = from,
        until = until,
        claimants = List(),
        children = List()
      )(None)
      tccConfig.config.september1stForDate(ty.from) shouldBe LocalDate.parse("2015-09-01", formatter)
    }

    "return prior 1st september date for current tax year date (as TC TAX YEAR)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val from = LocalDate.parse("2015-06-20", formatter)
      val until = LocalDate.parse("2016-04-05", formatter)

      val ty = TCTaxYear(
        from = from,
        until = until,
        claimants = List(),
        children = List()
      )(None)
      tccConfig.config.previousSeptember1stForDate(ty.from) shouldBe LocalDate.parse("2014-09-01", formatter)
    }

    "(child birthday is before september 1st) return September 1st following child's birthday" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val birthday = LocalDate.parse("2015-06-20", formatter)
      tccConfig.config.september1stFollowingChildBirthday(childBirthday = birthday) shouldBe LocalDate.parse("2015-09-01", formatter)
    }

    "(child birthday is after september 1st) return September 1st following child's birthday" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val birthday = LocalDate.parse("2015-09-02", formatter)
      tccConfig.config.september1stFollowingChildBirthday(childBirthday = birthday) shouldBe LocalDate.parse("2016-09-01", formatter)
    }

    "(child birthday is on september 1st) return September 1st following child's birthday" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val birthday = LocalDate.parse("2015-09-01", formatter)
      tccConfig.config.september1stFollowingChildBirthday(childBirthday = birthday) shouldBe LocalDate.parse("2016-09-01", formatter)
    }

    "(after april before december) determine the correct tax year for a date" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val today = LocalDate.parse("2016-06-20", formatter)

      val taxYear = tccConfig.config.determineTaxYearFromNow(from = today)
      taxYear shouldBe 2016
    }

    "(after december before april) determine the correct tax year for a date" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val today = LocalDate.parse("2016-02-20", formatter)

      val taxYear = tccConfig.config.determineTaxYearFromNow(from = today)
      taxYear shouldBe 2015
    }

    "get default Tax Year Config" in {
      val configs: Seq[Configuration] = app.configuration.underlying.getConfigList("tc.rule-change").asScala.map(Configuration(_))
      val defaultTaxYearConfig = tccConfig.getTCConfigDefault(configs)

      val tcTaxYearConfig = tccConfig.getTCTaxYearConfig(defaultTaxYearConfig)

      tcTaxYearConfig.childAgeLimit shouldBe 15
      tcTaxYearConfig.childAgeLimitDisabled shouldBe 16
      tcTaxYearConfig.childAgeLimitEducation shouldBe 19
      tcTaxYearConfig.youngAdultAgeLimit shouldBe 20
      tcTaxYearConfig.minimumHoursWorked shouldBe 16.00
      tcTaxYearConfig.minimumHoursWorkedIfCouple shouldBe 24.00
      tcTaxYearConfig.hours30Worked shouldBe 30.00
      tcTaxYearConfig.currentIncomeFallDifferenceAmount shouldBe 2500
      tcTaxYearConfig.currentIncomeRiseDifferenceAmount shouldBe 2500
    }

    "return default tax year rule" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("01-01-2014", formatter)

      val result = tccConfig.getConfig(current)

      result.childAgeLimit shouldBe 15
      result.childAgeLimitDisabled shouldBe 16
      result.childAgeLimitEducation shouldBe 19
      result.youngAdultAgeLimit shouldBe 20
      result.minimumHoursWorked shouldBe 16.00
      result.minimumHoursWorkedIfCouple shouldBe 24.00
      result.hours30Worked shouldBe 30.00
      result.currentIncomeFallDifferenceAmount shouldBe 2500
      result.currentIncomeRiseDifferenceAmount shouldBe 2500

    }

    "return 2018 tax year rule as 2017" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("01-01-2018", formatter)

      val result = tccConfig.getConfig(current)

      result.childAgeLimit shouldBe 15
      result.childAgeLimitDisabled shouldBe 16
      result.childAgeLimitEducation shouldBe 19
      result.youngAdultAgeLimit shouldBe 20
      result.minimumHoursWorked shouldBe 16.00
      result.minimumHoursWorkedIfCouple shouldBe 24.00
      result.hours30Worked shouldBe 30.00
      result.currentIncomeFallDifferenceAmount shouldBe 2500
      result.currentIncomeRiseDifferenceAmount shouldBe 2500
    }

    "return 2017 tax year rule" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("01-08-2017", formatter)

      val result = tccConfig.getConfig(current)

      result.childAgeLimit shouldBe 15
      result.childAgeLimitDisabled shouldBe 16
      result.childAgeLimitEducation shouldBe 19
      result.youngAdultAgeLimit shouldBe 20
      result.minimumHoursWorked shouldBe 16.00
      result.minimumHoursWorkedIfCouple shouldBe 24.00
      result.hours30Worked shouldBe 30.00
      result.currentIncomeFallDifferenceAmount shouldBe 2500
      result.currentIncomeRiseDifferenceAmount shouldBe 2500
    }

    "return 2017 tax year rule on the date of change" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("06-04-2017", formatter)

      val result = tccConfig.getConfig(current)

      result.childAgeLimit shouldBe 15
      result.childAgeLimitDisabled shouldBe 16
      result.childAgeLimitEducation shouldBe 19
      result.youngAdultAgeLimit shouldBe 20
      result.minimumHoursWorked shouldBe 16.00
      result.minimumHoursWorkedIfCouple shouldBe 24.00
      result.hours30Worked shouldBe 30.00
      result.currentIncomeFallDifferenceAmount shouldBe 2500
      result.currentIncomeRiseDifferenceAmount shouldBe 2500
    }

    "return 2016 tax year rule" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("01-08-2016", formatter)

      val result = tccConfig.getConfig(current)

      result.childAgeLimit shouldBe 15
      result.childAgeLimitDisabled shouldBe 16
      result.childAgeLimitEducation shouldBe 19
      result.youngAdultAgeLimit shouldBe 20
      result.minimumHoursWorked shouldBe 16.00
      result.minimumHoursWorkedIfCouple shouldBe 24.00
      result.hours30Worked shouldBe 30.00
      result.currentIncomeFallDifferenceAmount shouldBe 2500
      result.currentIncomeRiseDifferenceAmount shouldBe 2500
    }

    "return 2016 tax year rule on the date of change" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("06-04-2016", formatter)

      val result = tccConfig.getConfig(current)

      result.childAgeLimit shouldBe 15
      result.childAgeLimitDisabled shouldBe 16
      result.childAgeLimitEducation shouldBe 19
      result.youngAdultAgeLimit shouldBe 20
      result.minimumHoursWorked shouldBe 16.00
      result.minimumHoursWorkedIfCouple shouldBe 24.00
      result.hours30Worked shouldBe 30.00
      result.currentIncomeFallDifferenceAmount shouldBe 2500
      result.currentIncomeRiseDifferenceAmount shouldBe 2500
    }

    "return 2015 tax year rule" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("01-08-2015", formatter)

      val result = tccConfig.getConfig(current)

      result.childAgeLimit shouldBe 15
      result.childAgeLimitDisabled shouldBe 16
      result.childAgeLimitEducation shouldBe 19
      result.youngAdultAgeLimit shouldBe 20
      result.minimumHoursWorked shouldBe 16.00
      result.minimumHoursWorkedIfCouple shouldBe 24.00
      result.hours30Worked shouldBe 30.00
      result.currentIncomeFallDifferenceAmount shouldBe 2500
      result.currentIncomeRiseDifferenceAmount shouldBe 2500
    }

    "return 2015 tax year rule on the date of change" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val current = LocalDate.parse("06-04-2015", formatter)

      val result = tccConfig.getConfig(current)

      result.childAgeLimit shouldBe 15
      result.childAgeLimitDisabled shouldBe 16
      result.childAgeLimitEducation shouldBe 19
      result.youngAdultAgeLimit shouldBe 20
      result.minimumHoursWorked shouldBe 16.00
      result.minimumHoursWorkedIfCouple shouldBe 24.00
      result.hours30Worked shouldBe 30.00
      result.currentIncomeFallDifferenceAmount shouldBe 2500
      result.currentIncomeRiseDifferenceAmount shouldBe 2500
    }

  }
}
