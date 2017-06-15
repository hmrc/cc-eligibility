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

package config

import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat

object ConfigConstants {
  lazy val defaultAmount = BigDecimal(0)

  lazy val noOfMonths: Int = 12
  lazy val freeEntitlementTwoYearOld: Int = 2
  lazy val freeEntitlementThreeYearOld: Int = 3
  lazy val freeEntitlementFourYearOld: Int = 4

  lazy val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
  lazy val firstSept2017 = LocalDate.parse("2017-09-01", formatter)
}
