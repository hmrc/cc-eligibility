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
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import play.api.Configuration
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

class SchemeConfigSpec extends FakeCCEligibilityApplication with MockitoSugar {

  lazy val ccConfig = new CCConfig(
    app.injector.instanceOf[ServicesConfig],
    app.injector.instanceOf[Configuration]
  )

  "SchemeConfig" should {

    "return 1st september date for current tax year date" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val from = LocalDate.parse("2015-06-20", formatter)

      ccConfig.september1stForDate(from) shouldBe LocalDate.parse("2015-09-01", formatter)
    }

    "return prior 1st september date for current tax year date" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val from = LocalDate.parse("2015-06-20", formatter)

      ccConfig.previousSeptember1stForDate(from) shouldBe LocalDate.parse("2014-09-01", formatter)
    }


    "(child birthday is before september 1st) return September 1st following child's birthday" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val birthday = LocalDate.parse("2015-06-20", formatter)
      ccConfig.september1stFollowingChildBirthday(childBirthday = birthday) shouldBe LocalDate.parse("2015-09-01", formatter)
    }

    "(child birthday is after september 1st) return September 1st following child's birthday" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val birthday = LocalDate.parse("2015-09-02", formatter)
      ccConfig.september1stFollowingChildBirthday(childBirthday = birthday) shouldBe LocalDate.parse("2016-09-01", formatter)
    }

    "(child birthday is on september 1st) return September 1st following child's birthday" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val birthday = LocalDate.parse("2015-09-01", formatter)
      ccConfig.september1stFollowingChildBirthday(childBirthday = birthday) shouldBe LocalDate.parse("2016-09-01", formatter)
    }

    "(after april before december) determine the correct tax year for a date" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val today = LocalDate.parse("2016-06-20", formatter)

      val taxYear = ccConfig.determineTaxYearFromNow(from = today)
      taxYear shouldBe 2016
    }

    "(after december before april) determine the correct tax year for a date" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val today = LocalDate.parse("2016-02-20", formatter)

      val taxYear = ccConfig.determineTaxYearFromNow(from = today)
      taxYear shouldBe 2015
    }
  }

  val mockServiceConf: ServicesConfig = mock[ServicesConfig]
  val mockConfiguration: Configuration = mock[Configuration]

  "loadConfigByType" should {
    "return config" when {
      "configType is passed" in {
        val testConfig = new CCConfig(mockServiceConf, mockConfiguration)
        val configurationObject = Configuration(("rule-date", "2017-07-04"))

        when(mockConfiguration.getConfigSeq(any())).thenReturn(Some(Seq(configurationObject, configurationObject)))
        testConfig.loadConfigByType("tfc-rollout").isInstanceOf[Configuration] shouldBe true
      }
    }
  }

  "determine the current date" in  {
    val testObj = new CCConfig(mockServiceConf, mockConfiguration)

    when(mockServiceConf.getString(anyString()))
      .thenReturn("")

    val result = testObj.startDate
    result shouldBe LocalDate.now()
  }
}
