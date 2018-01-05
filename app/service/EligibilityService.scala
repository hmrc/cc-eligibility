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

package service

import connectors.CalculatorConnector
import eligibility.{ESCEligibility, FreeEntitlementEligibility, TCEligibility, TFCEligibility}
import models.Household
import models.input.CalculatorOutput
import models.mappings._
import models.output.{CalculatorInput, SchemeResults}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait EligibilityService {

  val calcConnector: CalculatorConnector

  val esc: ESCEligibility
  val tc: TCEligibility
  val tfc: TFCEligibility
  val thirtyHours: FreeEntitlementEligibility

  val TCEligibilityInput: HHToTCEligibilityInput
  val TFCEligibilityInput: HHToTFCEligibilityInput
  val ESCEligibilityInput: HHToESCEligibilityInput
  val FreeEntitlementEligibilityInput: HHToFree30hoursEligibilityInput

  def eligibility(request: Household)(implicit req: play.api.mvc.Request[_], hc: HeaderCarrier): Future[SchemeResults] = {
    for {
      tcEligibility <- tc.eligibility(TCEligibilityInput.convert(request))
      tfcEligibility <- tfc.eligibility(TFCEligibilityInput.convert(request))
      escEligibility <- esc.eligibility(ESCEligibilityInput.convert(request))
      thirtyHoursEligibility <- thirtyHours.thirtyHours(TFCEligibilityInput.convert(request))

      calcInput = CalculatorInput(if (tcEligibility.eligible) Some(tcEligibility) else None,
        if (tfcEligibility.householdEligibility) Some(tfcEligibility) else None,
        if (escEligibility.eligibility) Some(escEligibility) else None)

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
      val tfcResult: SchemeResults = SchemeResultsBuilder.buildTFCResults(tfcEligibility, Some(calcOutput), tcResult)

      tfcResult.copy(thirtyHrsRollout = thirtyHoursEligibility.rollout)
    }

  }

}

object EligibilityService extends EligibilityService
{
  override val calcConnector: CalculatorConnector = CalculatorConnector

  override val esc: ESCEligibility = ESCEligibility
  override val tc: TCEligibility = TCEligibility
  override val tfc: TFCEligibility = TFCEligibility
  override val thirtyHours: FreeEntitlementEligibility = FreeEntitlementEligibility

  override val TCEligibilityInput: HHToTCEligibilityInput = HHToTCEligibilityInput
  override val TFCEligibilityInput: HHToTFCEligibilityInput = HHToTFCEligibilityInput
  override val ESCEligibilityInput: HHToESCEligibilityInput = HHToESCEligibilityInput
  override val FreeEntitlementEligibilityInput: HHToFree30hoursEligibilityInput = HHToFree30hoursEligibilityInput
}
