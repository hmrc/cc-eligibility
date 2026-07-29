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

package models

import play.api.libs.json.Json
import models.Enumerable

enum CreditsEnum {
  case UNIVERSALCREDIT, NONE
}

object CreditsEnum extends Enumerable.Implicits {
  val creditsValues: Seq[CreditsEnum] = Seq(UNIVERSALCREDIT, NONE)

  given Enumerable[CreditsEnum] = Enumerable(creditsValues.map(value => value.toString -> value)*)

  def credits(): Unit = {
    val b = Json.parse("{}").validate[CreditsEnum]
  }

}

enum LocationEnum(val location: String) {
  case ENGLAND         extends LocationEnum("england")
  case SCOTLAND        extends LocationEnum("scotland")
  case WALES           extends LocationEnum("wales")
  case NORTHERNIRELAND extends LocationEnum("northern-ireland")

  override def toString: String = location
}

object LocationEnum extends Enumerable.Implicits {

  val locationValues: Seq[LocationEnum] = Seq(ENGLAND, SCOTLAND, WALES, NORTHERNIRELAND)

  given Enumerable[LocationEnum] = Enumerable(locationValues.map(value => value.toString -> value)*)

  def location(): Unit = {
    val b = Json.parse("{}").validate[LocationEnum]
  }

}

enum AgeRangeEnum {
  case UNDER18, EIGHTEENTOTWENTY, TWENTYONEOROVER
}

object AgeRangeEnum extends Enumerable.Implicits {

  val ageValues: Seq[AgeRangeEnum] = Seq(UNDER18, EIGHTEENTOTWENTY, TWENTYONEOROVER)

  given Enumerable[AgeRangeEnum] = Enumerable(ageValues.map(value => value.toString -> value)*)

  def ageRange(): Unit = {
    val b = Json.parse("{}").validate[AgeRangeEnum]
  }

}

enum EmploymentStatusEnum {
  case SELFEMPLOYED, APPRENTICE, NEITHER
}

object EmploymentStatusEnum extends Enumerable.Implicits {

  val employmentValues: Seq[EmploymentStatusEnum] = Seq(SELFEMPLOYED, APPRENTICE, NEITHER)

  given Enumerable[EmploymentStatusEnum] = Enumerable(employmentValues.map(value => value.toString -> value)*)

  def employmentStatus(): Unit = {
    val b = Json.parse("{}").validate[EmploymentStatusEnum]
  }

}

enum YesNoUnsureEnum {
  case YES, NO, NOTSURE
}

object YesNoUnsureEnum extends Enumerable.Implicits {

  val yesNoUnsureValues: Seq[YesNoUnsureEnum] = Seq(YES, NO, NOTSURE)

  given Enumerable[YesNoUnsureEnum] = Enumerable(yesNoUnsureValues.map(value => value.toString -> value)*)

  def yesNoUnsure(): Unit = {
    val b = Json.parse("{}").validate[YesNoUnsureEnum]
  }

}

enum PeriodEnum {
  case WEEKLY, FORTNIGHTLY, MONTHLY, QUARTERLY, YEARLY, INVALID
}

object PeriodEnum extends Enumerable.Implicits {

  val periodValues: Seq[PeriodEnum] = Seq(WEEKLY, FORTNIGHTLY, MONTHLY, QUARTERLY, YEARLY, INVALID)

  given Enumerable[PeriodEnum] = Enumerable(periodValues.map(value => value.toString -> value)*)

  def period(): Unit = {
    val b = Json.parse("{}").validate[PeriodEnum]
  }

}

enum SchemeEnum(scheme: String) {
  case TFCELIGIBILITY extends SchemeEnum("tfcEligibility")
  case ESCELIGIBILITY extends SchemeEnum("escEligibility")
}

object SchemeEnum extends Enumerable.Implicits {

  val schemeValues: Seq[SchemeEnum] = Seq(TFCELIGIBILITY, ESCELIGIBILITY)

  given Enumerable[SchemeEnum] = Enumerable(schemeValues.map(value => value.toString -> value)*)

  def scheme(): Unit = {
    val b = Json.parse("{}").validate[SchemeEnum]
  }

}
