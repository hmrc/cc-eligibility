/*
 * Copyright 2016 HM Revenue & Customs
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
import org.joda.time.format.DateTimeFormat
import play.api.Logger
import play.api.libs.json.{Writes, JsValue, JsString, JsNull}

object CCFormat extends CCFormat

trait CCFormat {
  val datePattern = "yyyy-MM-dd"

  implicit def jodaLocalDateWrites(pattern: String): Writes[LocalDate] = new Writes[LocalDate] {
    Logger.info(s"CCFormat.jodaLocalDateWrites")
    val df = DateTimeFormat.forPattern(pattern)
    def writes(d: LocalDate) : JsValue = {
      val date = if (d != null)
        JsString(d.toString(df))
      else
        JsNull
      date
    }
  }
}
