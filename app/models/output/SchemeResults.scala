/*
 * Copyright 2022 HM Revenue & Customs
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
import play.api.libs.json.Json

case class EscClaimantEligibility(
                                   parent: Boolean = false,
                                   partner: Boolean = false
                                 )

object EscClaimantEligibility {
  implicit val escClaimantEligibilityFormat = Json.format[EscClaimantEligibility]
}

case class TaxCreditsEligibility(
                                  wtcEligibility: Boolean = false,
                                  ctcEligibility: Boolean = false
                                 )

object TaxCreditsEligibility {
  implicit val taxCreditsEligibilityFormat = Json.format[TaxCreditsEligibility]
}

case class Scheme(name: SchemeEnum,
                  amount: BigDecimal ,
                  escClaimantEligibility: Option[EscClaimantEligibility] = None,
                  taxCreditsEligibility: Option[TaxCreditsEligibility] = None
                 ) {
  val missingEscClaimantEligibility = (name == SchemeEnum.ESCELIGIBILITY && escClaimantEligibility == None)
  val missingTaxCreditsEligibility = (name == SchemeEnum.TCELIGIBILITY && taxCreditsEligibility == None)
  require(!missingEscClaimantEligibility,"Missing values for escClaimantEligibility")
  require(!missingTaxCreditsEligibility,"Missing values for taxCreditsEligibility")
}

object Scheme {
  implicit val schemeFormat = Json.format[Scheme]
}

case class SchemeResults (
                           schemes: List[Scheme],
                           tfcRollout: Boolean = false,
                           thirtyHrsRollout: Boolean = false
                         )

object SchemeResults {
  implicit val schemeResultsformats = Json.format[SchemeResults]
}
