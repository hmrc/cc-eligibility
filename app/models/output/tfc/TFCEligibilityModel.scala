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

package models.output.tfc

import org.joda.time.LocalDate
import play.api.libs.json.{Json, Writes}
import utils.CCFormat

case class TFCEligibilityModel(
                                from: LocalDate,
                                until: LocalDate,
                                householdEligibility: Boolean,
                                freeRollout: Boolean,
                                tfcRollout: Boolean,
                                periods: List[TFCPeriod]
                                )

object TFCEligibilityModel {
  implicit val tfcEligible: Writes[TFCEligibilityModel] = Json.writes[TFCEligibilityModel]
}

case class TFCPeriod(
                      from: LocalDate,
                      until: LocalDate,
                      periodEligibility: Boolean,
                      claimants: List[OutputClaimant],
                      children: List[OutputChild]
                      )

object TFCPeriod {
  implicit val periodWrites: Writes[TFCPeriod] = Json.writes[TFCPeriod]
}

case class OutputClaimant(
                           qualifying: Boolean,
                           isPartner: Boolean
                           )

object OutputClaimant {
  implicit val claimantWrites: Writes[OutputClaimant] = Json.writes[OutputClaimant]
}

case class OutputChild(
                        id: Short,
                        qualifying: Boolean,
                        from: Option[LocalDate],
                        until: Option[LocalDate],
                        freeRollout: Boolean, //Not required in frontend
                        tfcRollout: Boolean //Not required in frontend
                        )

object OutputChild extends CCFormat {
  implicit val childWrites : Writes[OutputChild] = Json.writes[OutputChild]
}
