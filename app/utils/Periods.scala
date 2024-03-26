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

import play.api.libs.json._

object Periods extends Enumeration {
  type Period = Value

  val Weekly = Value(0, "Week")
  val Fortnightly = Value(1, "Fortnight")
  val Monthly = Value(2, "Month")
  val Quarterly = Value(3, "3 month")
  val Yearly = Value(4, "Year")
  val INVALID = Value(5, "INVALID")

  implicit val enumReads: Reads[Period] = EnumUtils.enumReads(Periods)

  implicit def enumWrites: Writes[Period] = EnumUtils.enumWrites

  def toString(period: Value): String = {
    period match {
      case Weekly => "cc.period.weekly"
      case Fortnightly => "cc.period.fortnightly"
      case Monthly => "cc.period.monthly"
      case Quarterly => "cc.period.3monthly"
      case Yearly => "cc.period.yearly"
      case _ => "cc.period.invalid"
    }
  }

  def toPeriod(period: String): Value = {
    period.toLowerCase match {
      case "monthly" => Monthly
      case "3-monthly" => Quarterly
      case _ => INVALID
    }
  }
}
