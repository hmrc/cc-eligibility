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

package service

import connectors.CalculatorConnector
import eligibility.{ESCEligibility, TCEligibility, TFCEligibility}
import javax.inject.Inject
import models.Household
import models.input.CalculatorOutput
import models.mappings._
import models.output.{CalculatorInput, SchemeResults}
import uk.gov.hmrc.http.HeaderCarrier
import utils.{CCConfig, ESCConfig}

import scala.concurrent.{ExecutionContext, Future}

class EligibilityService @Inject()(calcConnector: CalculatorConnector,
                                   esc: ESCEligibility,
                                   tc: TCEligibility,
                                   tfc: TFCEligibility,
                                   TCEligibilityInput: HHToTCEligibilityInput,
                                   TFCEligibilityInput: HHToTFCEligibilityInput,
                                   ESCEligibilityInput: HHToESCEligibilityInput,
                                   eSCConfig: ESCConfig, cCConfig: CCConfig)
                                  (implicit ec: ExecutionContext){

  def eligibility(request: Household)(implicit hc: HeaderCarrier): Future[SchemeResults] = {
    for {
      tcEligibility <- tc.eligibility(TCEligibilityInput.convert(request))
      tfcEligibility <- tfc.eligibility(TFCEligibilityInput.convert(request))
      escEligibility <- esc.eligibility(ESCEligibilityInput.convert(request), eSCConfig, cCConfig)

      calcInput = CalculatorInput(
        if (tcEligibility.eligible) Some(tcEligibility) else None,
        if (tfcEligibility.householdEligibility) Some(tfcEligibility) else None,
        if (escEligibility.eligibility) Some(escEligibility) else None
      )


      calcOutput <- {
        if (calcInput.esc.isDefined || calcInput.tc.isDefined || calcInput.tfc.isDefined) {
          calcConnector.getCalculatorResult(calcInput)
        } else {
          Future(CalculatorOutput())
        }
      }

    } yield {

      val escResult: SchemeResults = SchemeResultsBuilder.buildESCResults(escEligibility, Some(calcOutput), SchemeResults(List()))
      val tcResult: SchemeResults = SchemeResultsBuilder.buildTCResults(tcEligibility, Some(calcOutput), escResult)
      val tfcResult: SchemeResults = SchemeResultsBuilder.buildTFCResults(Some(calcOutput), tcResult)
      tfcResult
    }

  }

}
