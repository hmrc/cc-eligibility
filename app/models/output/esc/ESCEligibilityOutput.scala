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

package models.output.esc

import models.LocationEnum.LocationEnum
import java.time.LocalDate
import play.api.libs.json.{Json, Writes}
import utils.Periods

case class ESCEligibilityOutput(
    taxYears: List[ESCTaxYear],
    eligibility: Boolean = false,
    parentEligibility: Boolean = false,
    partnerEligibility: Boolean = false,
    location: Option[LocationEnum] = None
)

object ESCEligibilityOutput {
  implicit val escEligible: Writes[ESCEligibilityOutput] = Json.writes[ESCEligibilityOutput]
}

case class ESCTaxYear(
    from: LocalDate,
    until: LocalDate,
    periods: List[ESCPeriod]
)

object ESCTaxYear {
  implicit val taxYearWrites: Writes[ESCTaxYear] = Json.writes[ESCTaxYear]
}

case class ESCPeriod(
    from: LocalDate,
    until: LocalDate,
    claimants: List[ESCClaimant],
    children: List[ESCChild]
)

object ESCPeriod {
  implicit val periodWrites: Writes[ESCPeriod] = Json.writes[ESCPeriod]
}

case class ESCClaimant(
    qualifying: Boolean = false,
    isPartner: Boolean = false,
    eligibleMonthsInPeriod: Int = 0,
    currentIncome: Option[ESCIncome] = None,
    vouchers: Boolean = false,
    escStartDate: LocalDate = LocalDate.now
)

object ESCClaimant {
  implicit val claimantWrites: Writes[ESCClaimant] = Json.writes[ESCClaimant]
}

case class ESCIncome(
    employmentIncome: Option[BigDecimal] = None,
    pension: Option[BigDecimal] = None,
    taxCode: Option[String] = None
)

object ESCIncome {
  implicit val incomeWrite: Writes[ESCIncome] = Json.writes[ESCIncome]
}

case class ESCChild(
    qualifying: Boolean = false,
    childCareCost: BigDecimal,
    childCareCostPeriod: Periods.Period = Periods.Monthly
)

object ESCChild {
  implicit val childWrites: Writes[ESCChild] = Json.writes[ESCChild]
}
