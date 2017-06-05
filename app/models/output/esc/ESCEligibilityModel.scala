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
import utils.CCFormat

case class ESCEligibilityModel(
                                taxYears: List[TaxYear],
                                eligibility: Boolean = false,
                                parentEligibility: Boolean = false,
                                partnerEligibility: Boolean = false
                              )

object ESCEligibilityModel {
  implicit val escEligible: Writes[ESCEligibilityModel] = Json.writes[ESCEligibilityModel]
}

case class TaxYear(
                    from: LocalDate,
                    until: LocalDate,
                    periods: List[ESCPeriod]
                  )

object TaxYear extends CCFormat {
  implicit val taxYearWrites: Writes[TaxYear] = Json.writes[TaxYear]
}

case class ESCPeriod(
                      from: LocalDate,
                      until: LocalDate,
                      claimants: List[OutputClaimant],
                      children: List[OutputChild]
                    )

object ESCPeriod extends CCFormat {
  implicit val periodWrites: Writes[ESCPeriod] = Json.writes[ESCPeriod]
}

case class ClaimantElements(
                             // claimants qualification is determined by employer providing esc
                             // and children's qualification (if there is at least 1 qualifying child)
                             vouchers: Boolean = false
                           )

object ClaimantElements {
  implicit val claimantWrites: Writes[ClaimantElements] = Json.writes[ClaimantElements]
}

case class OutputClaimant(
                           qualifying: Boolean = false,
                           isPartner: Boolean = false,
                           eligibleMonthsInPeriod: Int = 0,
                           elements: ClaimantElements
                         )

//escAmount can be a voucher amount, childcare bursary amount or directly contracted amount
object OutputClaimant extends CCFormat {
  implicit val claimantWrites: Writes[OutputClaimant] = Json.writes[OutputClaimant]
}

case class OutputChild(
                        id: Short,
                        qualifying: Boolean = false
                      )

object OutputChild {
  implicit val childWrites: Writes[OutputChild] = Json.writes[OutputChild]
}
