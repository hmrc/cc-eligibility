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
import play.api.Play._
import uk.gov.hmrc.play.config.{RunMode, ServicesConfig}

import scala.util.Try


object ApplicationConfig extends ServicesConfig {

  val localDate: String = Try(getString("local-date")).getOrElse("")
  val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
  def startDate: LocalDate = if(localDate.isEmpty) LocalDate.now() else LocalDate.parse(localDate, formatter)


}
