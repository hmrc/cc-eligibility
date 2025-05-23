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
import controllers.FakeCCEligibilityApplication
import eligibility.{ESCEligibility, TFCEligibility}
import models.input.CalculatorOutput
import models.mappings.{HHToESCEligibilityInput, HHToTFCEligibilityInput}
import models.output.esc.{ESCEligibilityOutput, ESCTaxYear}
import models.output.tfc._
import models.output.{EscClaimantEligibility, Scheme, SchemeResults}
import models.{Claimant, Household, LocationEnum, SchemeEnum}
import java.time.LocalDate
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.mockito.MockitoSugar
import uk.gov.hmrc.http.HeaderCarrier
import utils.{CCConfig, ESCConfig}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class EligibilityServiceSpec extends AnyWordSpec with FakeCCEligibilityApplication with MockitoSugar {

  val mockCalc      = mock[CalculatorConnector]
  val mockESC       = mock[ESCEligibility]
  val mockTFC       = mock[TFCEligibility]
  val mockHHToTFC   = mock[HHToTFCEligibilityInput]
  val mockHHToESC   = mock[HHToESCEligibilityInput]
  val mockESCConfig = mock[ESCConfig]
  val mockCCConfig  = mock[CCConfig]

  class ServiceTest
      extends EligibilityService(
        mockCalc,
        mockESC,
        mockTFC,
        mockHHToTFC,
        mockHHToESC,
        mockESCConfig,
        mockCCConfig
      ) {

    override def eligibility(request: Household)(implicit hc: HeaderCarrier): Future[SchemeResults] =
      super.eligibility(request)

  }

  val SUT = new ServiceTest

  "EligibilityService" must {
    "return SchemesResult" when {
      "household request is received" in {

        val request = Household(children = Nil, parent = Claimant(), partner = None)

        val tfcSchemeOutput = Scheme(SchemeEnum.TFCELIGIBILITY, 1000, None)
        val escSchemeOutput = Scheme(SchemeEnum.ESCELIGIBILITY, 1000, Some(EscClaimantEligibility(true, true)))

        val expectedResult = SchemeResults(List(tfcSchemeOutput, escSchemeOutput))

        when(mockESC.eligibility(any(), any(), any())).thenReturn(Future(escEligibilityOutputAllTrue))
        when(mockTFC.eligibility(any())(any())).thenReturn(Future(tfcEligibilityOutputRolloutTrue))
        when(mockCalc.getCalculatorResult(any())(any())).thenReturn(Future(calcOutputValueAll))

        Await.result(SUT.eligibility(request), Duration(2, "seconds")) shouldBe expectedResult
      }

      "household request is received and only ESC Eligibility is true" in {

        val request         = Household(children = Nil, parent = Claimant(), partner = None)
        val tfcSchemeOutput = Scheme(SchemeEnum.TFCELIGIBILITY, 0, None)
        val escSchemeOutput = Scheme(SchemeEnum.ESCELIGIBILITY, 1000, Some(EscClaimantEligibility(true, true)))

        val expectedResult = SchemeResults(List(tfcSchemeOutput, escSchemeOutput))

        when(mockESC.eligibility(any(), any(), any())).thenReturn(Future(escEligibilityOutputAllTrue))
        when(mockTFC.eligibility(any())(any())).thenReturn(Future(mock[TFCEligibilityOutput]))
        when(mockCalc.getCalculatorResult(any())(any())).thenReturn(Future(calcOutputValueOnlyESC))

        Await.result(SUT.eligibility(request), Duration(2, "seconds")) shouldBe expectedResult
      }

      "household request is received and only TFC Eligibility is true" in {

        val request         = Household(children = Nil, parent = Claimant(), partner = None)
        val tfcSchemeOutput = Scheme(SchemeEnum.TFCELIGIBILITY, 1000, None)
        val escSchemeOutput = Scheme(SchemeEnum.ESCELIGIBILITY, 0, Some(EscClaimantEligibility(false, false)))

        val expectedResult = SchemeResults(List(tfcSchemeOutput, escSchemeOutput))

        when(mockESC.eligibility(any(), any(), any())).thenReturn(Future(mock[ESCEligibilityOutput]))
        when(mockTFC.eligibility(any())(any())).thenReturn(Future(tfcEligibilityOutputTrue))
        when(mockCalc.getCalculatorResult(any())(any())).thenReturn(Future(calcOutputValueOnlyTFC))

        Await.result(SUT.eligibility(request), Duration(2, "seconds")) shouldBe expectedResult
      }

      "household request is received and TFC Eligibility, ESC Eligibility, TC Eligibility are false" in {

        val request         = Household(children = Nil, parent = Claimant(), partner = None)
        val tfcSchemeOutput = Scheme(SchemeEnum.TFCELIGIBILITY, 0, None)
        val escSchemeOutput = Scheme(SchemeEnum.ESCELIGIBILITY, 0, Some(EscClaimantEligibility(false, false)))

        val expectedResult = SchemeResults(List(tfcSchemeOutput, escSchemeOutput))

        when(mockESC.eligibility(any(), any(), any())).thenReturn(Future(mock[ESCEligibilityOutput]))
        when(mockTFC.eligibility(any())(any())).thenReturn(mock[TFCEligibilityOutput])
        when(mockCalc.getCalculatorResult(any())(any())).thenReturn(Future(calcOutputValueNone))

        Await.result(SUT.eligibility(request), Duration(2, "seconds")) shouldBe expectedResult
      }
    }
  }

  // Values from eligibility
  val escEligibilityOutputAllTrue = ESCEligibilityOutput(
    taxYears = List[ESCTaxYear](),
    eligibility = true,
    parentEligibility = true,
    partnerEligibility = true,
    location = Some(LocationEnum.ENGLAND)
  )

  val tfcOutputparent  = TFCOutputClaimant(qualifying = true, isPartner = false)
  val tfcOutputpartner = TFCOutputClaimant(qualifying = true, isPartner = true)

  lazy val tfcOutputCChild = TFCOutputChild(
    id = 0,
    qualifying = true,
    from = None,
    until = None,
    childcareCost = BigDecimal(0),
    disability = TFCDisability()
  )

  lazy val tfcEligibilityOutputTrue = TFCEligibilityOutput(
    from = LocalDate.now(),
    until = LocalDate.now(),
    householdEligibility = true,
    periods = List(
      TFCPeriod(
        from = LocalDate.now(),
        until = LocalDate.now(),
        periodEligibility = true,
        claimants = List(tfcOutputparent, tfcOutputpartner),
        children = List(tfcOutputCChild)
      )
    )
  )

  lazy val tfcEligibilityOutputRolloutTrue = TFCEligibilityOutput(
    from = LocalDate.now(),
    until = LocalDate.now(),
    householdEligibility = true,
    periods = List(
      TFCPeriod(
        from = LocalDate.now(),
        until = LocalDate.now(),
        periodEligibility = true,
        claimants = List(tfcOutputparent, tfcOutputpartner),
        children = List(tfcOutputCChild)
      )
    )
  )

  val calcOutputValueAll: CalculatorOutput = CalculatorOutput(Some(BigDecimal(1000)), Some(BigDecimal(1000)))

  val calcOutputValueOnlyESC: CalculatorOutput = CalculatorOutput(None, Some(BigDecimal(1000)))
  val calcOutputValueOnlyTFC: CalculatorOutput = CalculatorOutput(Some(BigDecimal(1000)), None)
  val calcOutputValueNone: CalculatorOutput    = CalculatorOutput(None, None)

}
