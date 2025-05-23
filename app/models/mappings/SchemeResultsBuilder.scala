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

package models.mappings

import models.SchemeEnum
import models.input.CalculatorOutput
import models.output.esc.ESCEligibilityOutput
import models.output.{EscClaimantEligibility, Scheme, SchemeResults}

trait SchemeResultsBuilder {

  def buildESCResults(
      escEligibilityOutput: ESCEligibilityOutput,
      calculatorOutput: Option[CalculatorOutput] = None,
      schemeResultsIn: SchemeResults
  ): SchemeResults = {

    val parentEligibility     = escEligibilityOutput.parentEligibility
    val partnerEligibility    = escEligibilityOutput.partnerEligibility
    val escAmount: BigDecimal = calculatorOutput.flatMap(_.escAmount).getOrElse(BigDecimal(0))

    val newScheme = Scheme(
      name = SchemeEnum.ESCELIGIBILITY,
      amount = escAmount,
      escClaimantEligibility = Some(EscClaimantEligibility(parentEligibility, partnerEligibility))
    )
    val isESCSchemaPresent = schemeResultsIn.schemes.map(scheme => scheme.name).contains(SchemeEnum.ESCELIGIBILITY)

    val newList =
      if (isESCSchemaPresent)
        schemeResultsIn.schemes.map(scheme => if (scheme.name == SchemeEnum.ESCELIGIBILITY) newScheme else scheme)
      else newScheme :: schemeResultsIn.schemes

    schemeResultsIn.copy(schemes = newList)
  }

  def buildTFCResults(
      calculatorOutput: Option[CalculatorOutput] = None,
      schemeResultsIn: SchemeResults
  ): SchemeResults = {

    val tfcAmount = calculatorOutput.flatMap(_.tfcAmount).getOrElse(BigDecimal(0))

    val newScheme = Scheme(name = SchemeEnum.TFCELIGIBILITY, amount = tfcAmount)

    val isTFCSchemaPresent = schemeResultsIn.schemes.map(scheme => scheme.name).contains(SchemeEnum.TFCELIGIBILITY)

    val newList =
      if (isTFCSchemaPresent)
        schemeResultsIn.schemes.map(scheme => if (scheme.name == SchemeEnum.TFCELIGIBILITY) newScheme else scheme)
      else newScheme :: schemeResultsIn.schemes

    schemeResultsIn.copy(schemes = newList)
  }

}

object SchemeResultsBuilder extends SchemeResultsBuilder
