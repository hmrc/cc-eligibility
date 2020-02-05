/*
 * Copyright 2020 HM Revenue & Customs
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

trait HelperManager {

  def determineApril6DateFromNow(from: LocalDate): LocalDate = {
    val periodYear = from.getYear
    val january1st = LocalDate.parse(s"${periodYear}-01-01")
    val april6CurrentYear = LocalDate.parse(s"${periodYear}-04-06")

    if ((from.compareTo(january1st) == 0 || (from.isAfter(january1st)) && from.isBefore(april6CurrentYear))) {
      april6CurrentYear
    } else {
      april6CurrentYear.plusYears(1)
    }
  }

}

object HelperManager extends HelperManager
