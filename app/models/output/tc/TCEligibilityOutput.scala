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

package models.output.tc

import models.input.tc.TCIncome
import org.joda.time.LocalDate
import play.api.libs.json.{Json, Writes}
import play.api.libs.json.JodaReads._
import play.api.libs.json.JodaWrites._
import utils.Periods

case class TCEligibilityOutput(
                                eligible: Boolean = false,
                                taxYears: List[TCTaxYear],
                                wtc: Boolean = false,
                                ctc: Boolean = false
                             )

object TCEligibilityOutput {
  implicit val tcEligible: Writes[TCEligibilityOutput] = Json.writes[TCEligibilityOutput]
}

case class TCTaxYear(
                    from: LocalDate,
                    until: LocalDate,
                    previousHouseholdIncome: TCIncome = TCIncome(),
                    currentHouseholdIncome: TCIncome = TCIncome(),
                    periods: List[TCPeriod]
                  )

object TCTaxYear {
  implicit val taxYearWrites: Writes[TCTaxYear] = Json.writes[TCTaxYear]
}

case class TCPeriod(
                     from: LocalDate,
                     until: LocalDate,
                     householdElements: TCHouseHoldElements,
                     claimants: List[TCOutputClaimant],
                     children: List[TCOutputChild]
                   )

object TCPeriod {
  implicit val periodWrites: Writes[TCPeriod] = Json.writes[TCPeriod]
}

case class TCHouseHoldElements(
                              basic: Boolean = false,
                              hours30: Boolean = false,
                              childcare: Boolean = false,
                              loneParent: Boolean = false,
                              secondParent: Boolean = false,
                              family: Boolean = false,
                              wtc: Boolean = false,
                              ctc: Boolean = false
                              //wtc and ctc needed for TCEligibilityOutput, not used in frontend
                            )

object TCHouseHoldElements {
  implicit val householdWrites: Writes[TCHouseHoldElements] = Json.writes[TCHouseHoldElements]
}

case class TCOutputClaimant(
                           qualifying: Boolean = false,
                           isPartner: Boolean = false,
                           claimantDisability: TCDisability,
                           doesNotTaper: Boolean = false
                         )

object TCOutputClaimant {
  implicit val claimantWrites: Writes[TCOutputClaimant] = Json.writes[TCOutputClaimant]
}

case class TCDisability(
                               disability: Boolean,
                               severeDisability: Boolean
                             )

object TCDisability {
  implicit val claimantElementWrites: Writes[TCDisability] = Json.writes[TCDisability]
}

case class TCOutputChild(
                        childcareCost: BigDecimal = BigDecimal(0.00),
                        childcareCostPeriod: Periods.Period = Periods.Monthly,
                        qualifying: Boolean = false,
                        childElements: TCChildElements
                      )

object TCOutputChild {
  implicit val childWrites: Writes[TCOutputChild] = Json.writes[TCOutputChild]
}

case class TCChildElements(
                          child: Boolean = false,
                          youngAdult: Boolean = false,
                          disability: Boolean = false,
                          severeDisability: Boolean = false,
                          childcare: Boolean = false
                        )

object TCChildElements {
  implicit val childElementsWrites: Writes[TCChildElements] = Json.writes[TCChildElements]
}
