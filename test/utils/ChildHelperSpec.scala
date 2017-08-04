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

import org.joda.time.LocalDate
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table

class ChildHelperSpec extends CCConfigSpec {

  class Test extends ChildHelper {}

  val testClass = new Test

  "age" should {

    val now = LocalDate.now
    val testCases = Table(
      ("Date of Birth", "Current Date", "Age"),
      (now.plusDays(1), now, -1),
      (now, now, 0),
      (now.minusDays(1), now, 0),
      (now.minusMonths(1), now, 0),
      (now.minusYears(1).plusDays(1), now, 0),
      (now.minusYears(1), now, 1)
    )

    forAll(testCases) { case (dob, currentDate, result) =>
      s"return age = ${result} if dob = ${dob}" in {
        testClass.age(dob, currentDate) shouldBe result
      }
    }

  }

}
