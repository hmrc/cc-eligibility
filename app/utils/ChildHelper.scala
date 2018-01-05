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

import org.joda.time.LocalDate

trait ChildHelper {

  def age(dob: LocalDate, currentDate: LocalDate = CCConfig.StartDate): Int = {
    if (dob.isAfter(currentDate)) {
      -1
    } else {
      val age: Int = currentDate.getYear - dob.getYear
      if (
        (currentDate.getMonthOfYear < dob.getMonthOfYear)
          || (currentDate.getMonthOfYear == dob.getMonthOfYear && currentDate.getDayOfMonth < dob.getDayOfMonth)
      ) {
        age - 1
      }
      else {
        age
      }
    }
  }

}
