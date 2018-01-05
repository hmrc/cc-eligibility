/*
 * Copyright 2018 HM Revenue & Customs
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
import play.api.Configuration
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import org.mockito.Matchers._

class SchemeConfigSpec extends CCConfigSpec with FakeCCEligibilityApplication with MockitoSugar {

  "SchemeConfig" should {

    "return 1st september date for current tax year date" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val from = LocalDate.parse("2015-06-20", formatter)

      CCConfig.september1stForDate(from) shouldBe LocalDate.parse("2015-09-01", formatter)
    }

    "return prior 1st september date for current tax year date" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val from = LocalDate.parse("2015-06-20", formatter)

      CCConfig.previousSeptember1stForDate(from) shouldBe LocalDate.parse("2014-09-01", formatter)
    }


    "(child birthday is before september 1st) return September 1st following child's birthday" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val birthday = LocalDate.parse("2015-06-20", formatter)
      CCConfig.september1stFollowingChildBirthday(childBirthday = birthday) shouldBe LocalDate.parse("2015-09-01", formatter)
    }

    "(child birthday is after september 1st) return September 1st following child's birthday" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val birthday = LocalDate.parse("2015-09-02", formatter)
      CCConfig.september1stFollowingChildBirthday(childBirthday = birthday) shouldBe LocalDate.parse("2016-09-01", formatter)
    }

    "(child birthday is on september 1st) return September 1st following child's birthday" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val birthday = LocalDate.parse("2015-09-01", formatter)
      CCConfig.september1stFollowingChildBirthday(childBirthday = birthday) shouldBe LocalDate.parse("2016-09-01", formatter)
    }

    "(after april before december) determine the correct tax year for a date" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val today = LocalDate.parse("2016-06-20", formatter)

      val taxYear = TCConfig.determineTaxYearFromNow(from = today)
      taxYear shouldBe 2016
    }

    "(after december before april) determine the correct tax year for a date" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val today = LocalDate.parse("2016-02-20", formatter)

      val taxYear = TCConfig.determineTaxYearFromNow(from = today)
      taxYear shouldBe 2015
    }
  }

  "loadConfigByType" should {
    "return config" when {
      "configType is passed" in{


        val testConfig = new CCConfig{
          override val conf: Configuration = mock[Configuration]
        }
        val configurationObject = Configuration(("rule-date", "2017-07-04"))
        when(testConfig.conf.getConfigSeq(any())).thenReturn(Some(Seq(configurationObject, configurationObject)))
        testConfig.loadConfigByType("tfc-rollout").isInstanceOf[Configuration] shouldBe true
      }
    }
  }

  "determine the current date" in  {
    val testObj = new CCConfig {
      override val conf: Configuration = mock[Configuration]
    }

    when(
      testObj.conf.getString(anyString(), any())
    ).thenReturn(
      None
    )

    val result = testObj.StartDate
    result shouldBe LocalDate.now()
  }

  "determine the current date from config" in  {
    val testObj = new CCConfig {
      override val conf = mock[Configuration]
    }

    when(
      testObj.conf.getString(anyString(), any())
    ).thenReturn(
      Some("2017-02-06")
    )

    val result = testObj.StartDate
    result shouldBe LocalDate.parse("2017-02-06")
  }

}
