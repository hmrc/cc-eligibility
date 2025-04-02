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

import models.AgeRangeEnum.AgeRangeEnum
import models.CreditsEnum.CreditsEnum
import models.EmploymentStatusEnum.EmploymentStatusEnum
import models.LocationEnum.LocationEnum
import models.PeriodEnum.PeriodEnum
import models.YesNoUnsureEnum.YesNoUnsureEnum
import play.api.libs.json.{Json, OFormat}

import java.time.LocalDate

//Note :- The order of these classes need to preserved to ensure json formatters are prepared in the correct order
//Should also match childcarecalculatorfrontend.models.Household

case class Income(
                   employmentIncome: Option[BigDecimal] = None,
                   pension: Option[BigDecimal] = None,
                   otherIncome: Option[BigDecimal] = None,
                   benefits: Option[BigDecimal] = None,
                   taxCode: Option[String] = None
                 )

object Income {
  implicit val formatIncome: OFormat[Income] = Json.format[Income]
}

case class MinimumEarnings(
                           amount: BigDecimal =   0.00,
                           employmentStatus: Option[EmploymentStatusEnum] =   None,
                           selfEmployedIn12Months: Option[Boolean] =   None
                          )

object MinimumEarnings {
  implicit val formatMinimumEarnings: OFormat[MinimumEarnings] = Json.format[MinimumEarnings]
}

case class Disability(
                      disabled: Boolean,
                      severelyDisabled: Boolean,
                      blind: Boolean
                     )

object Disability {
  implicit val formatDisability: OFormat[Disability] = Json.format[Disability]
}

case class ChildCareCost(
                         amount: Option[BigDecimal] = None,
                         period: Option[PeriodEnum] = None
                        )

object ChildCareCost {
  implicit val formatChildCareCost: OFormat[ChildCareCost] = Json.format[ChildCareCost]
}

case class Child(
                  id: Short,
                  name: String,
                  dob: Option[LocalDate] = None,
                  disability: Option[Disability] = None,
                  childcareCost: Option[ChildCareCost] = None,
                )

object Child {
  implicit val formatChild: OFormat[Child] = Json.format[Child]
}

case class Claimant(
                     ageRange: Option[AgeRangeEnum] = None,
                     benefits: Option[Set[ParentsBenefits]] = None,
                     currentYearlyIncome: Option[Income]  = None,
                     minimumEarnings: Option[MinimumEarnings]= None,
                     escVouchers: Option[YesNoUnsureEnum] =   None,
                     maximumEarnings: Option[Boolean] = None
                   )

object Claimant {
  implicit val formatClaimant: OFormat[Claimant] = Json.format[Claimant]
}

case class Household(
                      credits: Option[CreditsEnum] =   None,
                      location: Option[LocationEnum] = None,
                      children: List[Child],
                      parent: Claimant,
                      partner: Option[Claimant] = None
                    )

object Household {
  implicit val formatHousehold: OFormat[Household] = Json.format[Household]
}
