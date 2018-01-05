/*
 * Copyright 2018 HM Revenue & Customs
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

package models.output.tfc

import org.joda.time.LocalDate
import play.api.libs.json.{Json, Writes}
import utils.Periods

case class TFCEligibilityOutput(
                                from: LocalDate,
                                until: LocalDate,
                                householdEligibility: Boolean,
                                tfcRollout: Boolean,
                                periods: List[TFCPeriod]
                                )

object TFCEligibilityOutput {
  implicit val tfcEligible: Writes[TFCEligibilityOutput] = Json.writes[TFCEligibilityOutput]
}

case class TFCPeriod(
                      from: LocalDate,
                      until: LocalDate,
                      periodEligibility: Boolean,
                      claimants: List[TFCOutputClaimant],
                      children: List[TFCOutputChild]
                      )

object TFCPeriod {
  implicit val periodWrites: Writes[TFCPeriod] = Json.writes[TFCPeriod]
}

case class TFCOutputClaimant(
                           qualifying: Boolean,
                           isPartner: Boolean
                           )

object TFCOutputClaimant {
  implicit val claimantWrites: Writes[TFCOutputClaimant] = Json.writes[TFCOutputClaimant]
}

case class TFCOutputChild(
                        id: Short,
                        qualifying: Boolean,
                        from: Option[LocalDate],
                        until: Option[LocalDate],
                        tfcRollout: Boolean, //Not required in frontend
                        childcareCost: BigDecimal = BigDecimal(0),
                        childcareCostPeriod: Periods.Period = Periods.Monthly,
                        disability: TFCDisability = TFCDisability()
                        )

object TFCOutputChild {
  implicit val childWrites: Writes[TFCOutputChild] = Json.writes[TFCOutputChild]
}

case class TFCDisability(
                          disabled: Boolean = false,
                          severelyDisabled: Boolean = false
                          )

object TFCDisability  {
  implicit val disability: Writes[TFCDisability] = Json.writes[TFCDisability]
}
