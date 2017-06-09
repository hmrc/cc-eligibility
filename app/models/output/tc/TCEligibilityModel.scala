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

package models.output.tc

import org.joda.time.LocalDate
import play.api.libs.json.{Writes, Json}
import utils.{CCFormat, Periods}

case class TCEligibilityModel(
                               eligible: Boolean = false,
                               taxYears: List[TaxYear],
                               wtc: Boolean = false,
                               ctc: Boolean = false
                             )

object TCEligibilityModel {
  implicit val tcEligible: Writes[TCEligibilityModel] = Json.writes[TCEligibilityModel]
}

case class TaxYear(
                    from: LocalDate,
                    until: LocalDate,
                    periods: List[TCPeriod]
                  )

object TaxYear extends CCFormat {
  implicit val taxYearWrites: Writes[TaxYear] = Json.writes[TaxYear]
}

case class TCPeriod(
                     from: LocalDate,
                     until: LocalDate,
                     householdElements: HouseholdElements,
                     claimants: List[OutputClaimant],
                     children: List[OutputChild]
                   )

object TCPeriod extends CCFormat {
  implicit val periodWrites: Writes[TCPeriod] = Json.writes[TCPeriod]
}

case class HouseholdElements(
                              basic: Boolean = false,
                              hours30: Boolean = false,
                              childcare: Boolean = false,
                              loneParent: Boolean = false,
                              secondParent: Boolean = false,
                              family: Boolean = false,
                              wtc: Boolean = false,
                              ctc: Boolean = false
                            )

object HouseholdElements {
  implicit val householdWrites: Writes[HouseholdElements] = Json.writes[HouseholdElements]
}

case class OutputClaimant(
                           qualifying: Boolean = false,
                           isPartner: Boolean = false,
                           claimantDisability: ClaimantDisability
                         )

object OutputClaimant {
  implicit val claimantWrites: Writes[OutputClaimant] = Json.writes[OutputClaimant]
}

case class ClaimantDisability(
                               disability: Boolean,
                               severeDisability: Boolean
                             )

object ClaimantDisability {
  implicit val claimantElementWrites: Writes[ClaimantDisability] = Json.writes[ClaimantDisability]
}

case class OutputChild(
                        id: Short,
                        childcareCost: BigDecimal = BigDecimal(0.00),
                        childcareCostPeriod: Periods.Period,
                        qualifying: Boolean = false,
                        childElements: ChildElements
                      )

object OutputChild {
  implicit val childWrites: Writes[OutputChild] = Json.writes[OutputChild]
}

case class ChildElements(
                          child: Boolean = false,
                          youngAdult: Boolean = false,
                          disability: Boolean = false,
                          severeDisability: Boolean = false,
                          childcare: Boolean = false
                        )

object ChildElements {
  implicit val childElementsWrites: Writes[ChildElements] = Json.writes[ChildElements]
}
