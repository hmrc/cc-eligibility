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

import play.api.libs.json.Json

//TBD
case class FreeEntitlementPageModel (
                                      twoYearOld: Boolean = false,
                                      threeYearOld: Boolean = false,
                                      fourYearOld: Boolean = false,
                                      region: String = "",
                                      freeEntitlementRollout: Boolean = false,
                                      hasFifteenHours: Boolean = false,
                                      hasThirtyHours: Boolean = false
                                    )

object FreeEntitlementPageModel {
  implicit val formats = Json.format[FreeEntitlementPageModel]
}

case class EscVouchersAvailablePageModel (
                                           parent : Boolean = false,
                                           partner : Option[Boolean] = None

                                         )

object EscVouchersAvailablePageModel {
  implicit val formats = Json.format[EscVouchersAvailablePageModel]
}

case class SchemesResult (
                              household: Household,
                              annualCost: Int,
                              schemes: Seq[Scheme],
                              tfcEligibility: Boolean,
                              wtcEligibility: Boolean,
                              ctcEligibility: Boolean,
                              escEligibility: Boolean,
                              parentEscEligibility: Boolean = false,
                              partnerEscEligibility: Boolean = false,
                              freeEntitlement: FreeEntitlementPageModel,
                              escVouchersAvailable: EscVouchersAvailablePageModel,
                              displaySupportBulletPoint: Boolean = false,
                              tfcRollout: Boolean = false
                            )

object SchemesResult {
  implicit val schemeFormat = Json.format[Scheme]
  implicit val formats = Json.format[SchemesResult]
}

case class Scheme(val name: String, val amount: Int)
