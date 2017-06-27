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

package models.output.esc

import org.joda.time.LocalDate
import play.api.libs.json.{Json, Writes}
import utils.Periods

case class ESCEligibilityOutput(
                                taxYears: List[TaxYear],
                                eligibility: Boolean = false,
                                parentEligibility: Boolean = false,
                                partnerEligibility: Boolean = false
                              )

object ESCEligibilityOutput {
  implicit val escEligible: Writes[ESCEligibilityOutput] = Json.writes[ESCEligibilityOutput]
}

case class TaxYear(
                    from: LocalDate,
                    until: LocalDate,
                    periods: List[ESCPeriod]
                  )

object TaxYear {
  implicit val taxYearWrites: Writes[TaxYear] = Json.writes[TaxYear]
}

case class ESCPeriod(
                      from: LocalDate,
                      until: LocalDate,
                      claimants: List[ESCOutputClaimant],
                      children: List[OutputChild]
                    )

object ESCPeriod {
  implicit val periodWrites: Writes[ESCPeriod] = Json.writes[ESCPeriod]
}

case class ESCOutputClaimant(
                           qualifying: Boolean = false,
                           isPartner: Boolean = false,
                           eligibleMonthsInPeriod: Int = 0,
                           vouchers: Boolean = false
                         )

//escAmount can be a voucher amount, childcare bursary amount or directly contracted amount
object ESCOutputClaimant {
  implicit val claimantWrites: Writes[ESCOutputClaimant] = Json.writes[ESCOutputClaimant]
}

case class OutputChild(
                        qualifying: Boolean = false,
                        childCareCost: BigDecimal,
                        childCareCostPeriod: Periods.Period = Periods.Monthly
                      )

object OutputChild {
  implicit val childWrites: Writes[OutputChild] = Json.writes[OutputChild]
}
