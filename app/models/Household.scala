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

package models

import models.AgeRangeEnum.AgeRangeEnum
import models.LocationEnum.LocationEnum
import models.TcUcBenefitsEnum.TcUcBenefitsEnum
import models.YesNoUnsureBothEnum.YesNoUnsureBothEnum
import models.EmploymentStatusEnum.EmploymentStatusEnum
import models.PeriodEnum.PeriodEnum
import org.joda.time.LocalDate
import play.api.libs.json.Json

//Note :- The order of these classes need to preserved to ensure json formatters are prepared in the correct order
//Should also match childcarecalculatorfrontend.models.Household
case class StatutoryIncome(
                            statutoryWeeks: Double = 0.00,
                            statutoryAmount: BigDecimal =   0.00
                          )

object StatutoryIncome {
  implicit val formatStatutoryIncome = Json.format[StatutoryIncome]
}

case class Income(
                   employmentIncome: Option[BigDecimal] = None,
                   pension: Option[BigDecimal] =   None,
                   otherIncome: Option[BigDecimal] = None,
                   benefits: Option[BigDecimal] =   None,
                   statutoryIncome: Option[StatutoryIncome]=None
                 )

object Income {
  implicit val formatIncome = Json.format[Income]
}

case class Benefits(
                    disabilityBenefits: Boolean = false,
                    highRateDisabilityBenefits: Boolean =   false,
                    incomeBenefits: Boolean =   false,
                    carersAllowance: Boolean = false
                   )

object Benefits {
  implicit val formatBenefits = Json.format[Benefits]

}

case class MinimumEarnings(
                           amount: BigDecimal =   0.00,
                           employmentStatus: Option[EmploymentStatusEnum] =   None,
                           selfEmployedIn12Months: Option[Boolean] =   None
                          )

object MinimumEarnings {
  implicit val formatMinimumEarnings = Json.format[MinimumEarnings]
}

case class Disability(
                      disabled: Boolean,
                      severelyDisabled: Boolean,
                      blind: Boolean
                     )

object Disability {
  implicit val formatDisability = Json.format[Disability]
}

case class ChildCareCost(
                         amount: Option[BigDecimal] =   None,
                         period: Option[PeriodEnum] =   None
                        )

object ChildCareCost {
  implicit val formatChildCareCost = Json.format[ChildCareCost]
}

case class Education(
                      inEducation: Boolean = false,
                      startDate: Option[LocalDate] =   None
                    )

object Education {
  implicit val formatEducation = Json.format[Education]
}

case class Child(
                  id: Short,
                  name: String,
                  dob: Option[LocalDate]=None,
                  disability: Option[Disability]=None,
                  childcareCost: Option[ChildCareCost]=None,
                  education: Option[Education]= None
                )

object Child {
  implicit val formatChild = Json.format[Child]
}

case class Claimant(
                     ageRange: Option[AgeRangeEnum] = None,
                     benefits: Option[Benefits] = None,
                     lastYearlyIncome: Option[Income]  =   None,
                     currentYearlyIncome: Option[Income]  = None,
                     hours: Option[BigDecimal] =   None,
                     minimumEarnings: Option[MinimumEarnings]= None,
                     escVouchers: Option[YesNoUnsureBothEnum] =   None
                   )

object Claimant {
  implicit val formatClaimant = Json.format[Claimant]
}

case class Household(
                      tcUcBenefits: Option[TcUcBenefitsEnum] =   None,
                      location: Option[LocationEnum] = None,
                      children: List[Child],
                      parent: Claimant,
                      partner: Option[Claimant]
                    )

object Household {
  implicit val formatHousehold = Json.format[Household]
}

