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

import play.api.libs.json.*
import models.Enumerable

enum Periods {
  case Weekly, Fortnightly, Monthly, Quarterly, Yearly, INVALID
} 

object Periods extends Enumerable.Implicits {
  val periodValues: Seq[Periods] = Seq(Weekly, Fortnightly, Monthly, Quarterly, Yearly, INVALID)

  given Enumerable[Periods] = Enumerable(periodValues.map(value => value.toString -> value) *)

  def periods(): Unit = {
    val b = Json.parse("{}").validate[Periods]
  }
}