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

import models.Enumerable

enum CreditsEnum {
  case UNIVERSALCREDIT, NONE
}

object CreditsEnum extends Enumerable.Implicits {
  given Enumerable[CreditsEnum] = Enumerable.apply(CreditsEnum.values)
}

enum LocationEnum(val location: String) {
  case ENGLAND         extends LocationEnum("england")
  case SCOTLAND        extends LocationEnum("scotland")
  case WALES           extends LocationEnum("wales")
  case NORTHERNIRELAND extends LocationEnum("northern-ireland")

  override def toString: String = location
}

object LocationEnum extends Enumerable.Implicits {
  given Enumerable[LocationEnum] = Enumerable.apply(LocationEnum.values)
}

enum AgeRangeEnum {
  case UNDER18, EIGHTEENTOTWENTY, TWENTYONEOROVER
}

object AgeRangeEnum extends Enumerable.Implicits {
  given Enumerable[AgeRangeEnum] = Enumerable.apply(AgeRangeEnum.values)
}

enum EmploymentStatusEnum {
  case SELFEMPLOYED, APPRENTICE, NEITHER
}

object EmploymentStatusEnum extends Enumerable.Implicits {
  given Enumerable[EmploymentStatusEnum] = Enumerable.apply(EmploymentStatusEnum.values)
}

enum YesNoUnsureEnum {
  case YES, NO, NOTSURE
}

object YesNoUnsureEnum extends Enumerable.Implicits {
  given Enumerable[YesNoUnsureEnum] = Enumerable.apply(YesNoUnsureEnum.values)
}

enum PeriodEnum {
  case WEEKLY, FORTNIGHTLY, MONTHLY, QUARTERLY, YEARLY, INVALID
}

object PeriodEnum extends Enumerable.Implicits {
  given Enumerable[PeriodEnum] = Enumerable.apply(PeriodEnum.values)
}

enum SchemeEnum(val scheme: String) {
  case TFCELIGIBILITY extends SchemeEnum("tfcEligibility")
  case ESCELIGIBILITY extends SchemeEnum("escEligibility")

  override def toString: String = scheme
}

object SchemeEnum extends Enumerable.Implicits {
  given Enumerable[SchemeEnum] = Enumerable.apply(SchemeEnum.values)
}
