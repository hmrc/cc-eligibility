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

package models.output

import models.SchemeEnum
import models.SchemeEnum.SchemeEnum
import play.api.libs.json.{Json, OFormat}

case class EscClaimantEligibility(
                                   parent: Boolean = false,
                                   partner: Boolean = false
                                 )

object EscClaimantEligibility {
  implicit val escClaimantEligibilityFormat: OFormat[EscClaimantEligibility] = Json.format[EscClaimantEligibility]
}


case class Scheme(name: SchemeEnum,
                  amount: BigDecimal ,
                  escClaimantEligibility: Option[EscClaimantEligibility] = None,
                 ) {
  private val missingEscClaimantEligibility = name == SchemeEnum.ESCELIGIBILITY && escClaimantEligibility.isEmpty
  require(!missingEscClaimantEligibility,"Missing values for escClaimantEligibility")
}

object Scheme {
  implicit val schemeFormat: OFormat[Scheme] = Json.format[Scheme]
}

case class SchemeResults (
                           schemes: List[Scheme]
                         )

object SchemeResults {
  implicit val schemeResultsformats: OFormat[SchemeResults] = Json.format[SchemeResults]
}
