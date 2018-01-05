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
import controllers.FakeCCEligibilityApplication
import eligibility.{ESCEligibility, FreeEntitlementEligibility, TCEligibility, TFCEligibility}
import models.input.CalculatorOutput
import models.mappings.{HHToESCEligibilityInput, HHToFree30hoursEligibilityInput, HHToTCEligibilityInput, HHToTFCEligibilityInput}
import models.output.esc.{ESCEligibilityOutput, ESCTaxYear}
import models.output.freeEntitlement.ThirtyHoursEligibilityModel
import models.output.tc.{TCEligibilityOutput, TCTaxYear}
import models.output.tfc._
import models.output.{EscClaimantEligibility, Scheme, SchemeResults, TaxCreditsEligibility}
import models.{Claimant, Household, LocationEnum, SchemeEnum}
import org.joda.time.LocalDate
import org.mockito.Matchers.any
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import play.api.mvc.Request
import play.api.test.FakeRequest
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class EligibilityServiceSpec extends UnitSpec with FakeCCEligibilityApplication with MockitoSugar {


  class ServiceTest extends EligibilityService{
    override val calcConnector: CalculatorConnector = mock[CalculatorConnector]
    override val esc: ESCEligibility = mock[ESCEligibility]
    override val tc: TCEligibility = mock[TCEligibility]
    override val tfc: TFCEligibility = mock[TFCEligibility]
    override val thirtyHours: FreeEntitlementEligibility = mock[FreeEntitlementEligibility]
    override val TCEligibilityInput: HHToTCEligibilityInput = mock[HHToTCEligibilityInput]
    override val TFCEligibilityInput: HHToTFCEligibilityInput = mock[HHToTFCEligibilityInput]
    override val ESCEligibilityInput: HHToESCEligibilityInput = mock[HHToESCEligibilityInput]
    override val FreeEntitlementEligibilityInput: HHToFree30hoursEligibilityInput = mock[HHToFree30hoursEligibilityInput]
    override def eligibility(request: Household)(implicit req: Request[_], hc: HeaderCarrier): Future[SchemeResults] = super.eligibility(request)
  }

  val SUT = new ServiceTest

  "EligibilityService" should {
    "return SchemesResult" when {
      "household request is received" in {

        implicit val req = FakeRequest()

        val request = Household(children = Nil, parent = Claimant(), partner = None)

        val tfcSchemeOutput = Scheme(SchemeEnum.TFCELIGIBILITY, 1000, None, None)
        val tcSchemeOutput = Scheme(SchemeEnum.TCELIGIBILITY, 1000, None, Some(TaxCreditsEligibility(true,true)))
        val escSchemeOutput = Scheme(SchemeEnum.ESCELIGIBILITY, 1000, Some(EscClaimantEligibility(true,true)), None)

        val expectedResult = SchemeResults(List(tfcSchemeOutput, tcSchemeOutput, escSchemeOutput), true, true)

        when(SUT.esc.eligibility(any())).thenReturn(Future(escEligibilityOutputAllTrue))
        when(SUT.tc.eligibility(any())).thenReturn(Future(tcEligibilityOutputAllTrue))
        when(SUT.tfc.eligibility(any())(any(), any())).thenReturn(Future(tfcEligibilityOutputRolloutTrue))
        when(SUT.thirtyHours.thirtyHours(any())(any(), any())).thenReturn(Future(thirtyHoursEligibilityOutput))
        when(SUT.calcConnector.getCalculatorResult(any())(any())).thenReturn(Future(calcOutputValueAll))

        Await.result(SUT.eligibility(request), Duration(2, "seconds")) shouldBe expectedResult
      }

      "household request is received and only ESC Eligibility is true" in {

        implicit val req = FakeRequest()

        val request = Household(children = Nil, parent = Claimant(), partner = None)
        val tfcSchemeOutput = Scheme(SchemeEnum.TFCELIGIBILITY, 0, None, None)
        val tcSchemeOutput = Scheme(SchemeEnum.TCELIGIBILITY, 0, None, Some(TaxCreditsEligibility(false,false)))
        val escSchemeOutput = Scheme(SchemeEnum.ESCELIGIBILITY, 1000, Some(EscClaimantEligibility(true,true)), None)

        val expectedResult = SchemeResults(List(tfcSchemeOutput, tcSchemeOutput, escSchemeOutput), false, false)

        when(SUT.esc.eligibility(any())).thenReturn(Future(escEligibilityOutputAllTrue))
        when(SUT.tc.eligibility(any())).thenReturn(Future(mock[TCEligibilityOutput]))
        when(SUT.tfc.eligibility(any())(any(), any())).thenReturn(Future(mock[TFCEligibilityOutput]))
        when(SUT.thirtyHours.thirtyHours(any())(any(), any())).thenReturn(Future(mock[ThirtyHoursEligibilityModel]))
        when(SUT.calcConnector.getCalculatorResult(any())(any())).thenReturn(Future(calcOutputValueOnlyESC))

        Await.result(SUT.eligibility(request), Duration(2, "seconds")) shouldBe expectedResult
      }

      "household request is received and only TC Eligibility is true" in {

        implicit val req = FakeRequest()

        val request = Household(children = Nil, parent = Claimant(), partner = None)
        val tfcSchemeOutput = Scheme(SchemeEnum.TFCELIGIBILITY, 0, None, None)
        val tcSchemeOutput = Scheme(SchemeEnum.TCELIGIBILITY, 1000, None, Some(TaxCreditsEligibility(true,true)))
        val escSchemeOutput = Scheme(SchemeEnum.ESCELIGIBILITY, 0, Some(EscClaimantEligibility(false,false)), None)

        val expectedResult = SchemeResults(List(tfcSchemeOutput, tcSchemeOutput, escSchemeOutput), false, false)

        when(SUT.esc.eligibility(any())).thenReturn(Future(mock[ESCEligibilityOutput]))
        when(SUT.tc.eligibility(any())).thenReturn(Future(tcEligibilityOutputAllTrue))
        when(SUT.tfc.eligibility(any())(any(), any())).thenReturn(Future(mock[TFCEligibilityOutput]))
        when(SUT.thirtyHours.thirtyHours(any())(any(), any())).thenReturn(Future(mock[ThirtyHoursEligibilityModel]))
        when(SUT.calcConnector.getCalculatorResult(any())(any())).thenReturn(Future(calcOutputValueOnlyTC))

        Await.result(SUT.eligibility(request), Duration(2, "seconds")) shouldBe expectedResult
      }

      "household request is received and only TFC Eligibility is true" in {

        implicit val req = FakeRequest()

        val request = Household(children = Nil, parent = Claimant(), partner = None)
        val tfcSchemeOutput = Scheme(SchemeEnum.TFCELIGIBILITY, 1000, None, None)
        val tcSchemeOutput = Scheme(SchemeEnum.TCELIGIBILITY, 0, None, Some(TaxCreditsEligibility(false,false)))
        val escSchemeOutput = Scheme(SchemeEnum.ESCELIGIBILITY, 0, Some(EscClaimantEligibility(false,false)), None)

        val expectedResult = SchemeResults(List(tfcSchemeOutput, tcSchemeOutput, escSchemeOutput), false, false)

        when(SUT.esc.eligibility(any())).thenReturn(Future(mock[ESCEligibilityOutput]))
        when(SUT.tc.eligibility(any())).thenReturn(Future(mock[TCEligibilityOutput]))
        when(SUT.tfc.eligibility(any())(any(), any())).thenReturn(Future(tfcEligibilityOutputTrue))
        when(SUT.thirtyHours.thirtyHours(any())(any(), any())).thenReturn(Future(mock[ThirtyHoursEligibilityModel]))
        when(SUT.calcConnector.getCalculatorResult(any())(any())).thenReturn(Future(calcOutputValueOnlyTFC))

        Await.result(SUT.eligibility(request), Duration(2, "seconds")) shouldBe expectedResult
      }

      "household request is received and TFC Eligibility, ESC Eligibility, TC Eligibility are false" in {

        implicit val req = FakeRequest()

        val request = Household(children = Nil, parent = Claimant(), partner = None)
        val tfcSchemeOutput = Scheme(SchemeEnum.TFCELIGIBILITY, 0, None, None)
        val tcSchemeOutput = Scheme(SchemeEnum.TCELIGIBILITY, 0, None, Some(TaxCreditsEligibility(false,false)))
        val escSchemeOutput = Scheme(SchemeEnum.ESCELIGIBILITY, 0, Some(EscClaimantEligibility(false,false)), None)

        val expectedResult = SchemeResults(List(tfcSchemeOutput, tcSchemeOutput, escSchemeOutput), false, false)

        when(SUT.esc.eligibility(any())).thenReturn(Future(mock[ESCEligibilityOutput]))
        when(SUT.tc.eligibility(any())).thenReturn(Future(mock[TCEligibilityOutput]))
        when(SUT.tfc.eligibility(any())(any(), any())).thenReturn(mock[TFCEligibilityOutput])
        when(SUT.thirtyHours.thirtyHours(any())(any(), any())).thenReturn(Future(mock[ThirtyHoursEligibilityModel]))
        when(SUT.calcConnector.getCalculatorResult(any())(any())).thenReturn(Future(calcOutputValueNone))

        Await.result(SUT.eligibility(request), Duration(2, "seconds")) shouldBe expectedResult
      }
    }
  }

  //Values from eligibility
  val escEligibilityOutputAllTrue = ESCEligibilityOutput(taxYears = List[ESCTaxYear](), eligibility  = true, parentEligibility  = true, partnerEligibility  = true, location = Some(LocationEnum.ENGLAND))
  val tcEligibilityOutputAllTrue = TCEligibilityOutput(taxYears =List[TCTaxYear](), eligible  = true, wtc  = true, ctc  = true)

  val tfcOutputparent = TFCOutputClaimant(qualifying = true, isPartner = false)
  val tfcOutputpartner = TFCOutputClaimant(qualifying = true, isPartner = true)
  val tfcOutputCChild = TFCOutputChild( id = 0,
    qualifying = true,
    from = None,
    until = None,
    tfcRollout = false,
    childcareCost = BigDecimal(0),
    disability = TFCDisability())

  val tfcEligibilityOutputTrue = TFCEligibilityOutput(from = LocalDate.now(),
    until  = LocalDate.now(),
    householdEligibility  = true,
    tfcRollout  = false,
    periods = List(TFCPeriod(from = LocalDate.now(),
      until  = LocalDate.now(),
      periodEligibility = true,
      claimants = List(tfcOutputparent, tfcOutputpartner),
      children = List(tfcOutputCChild))))

  val tfcEligibilityOutputRolloutTrue = TFCEligibilityOutput(from = LocalDate.now(),
    until  = LocalDate.now(),
    householdEligibility  = true,
    tfcRollout  = true,
    periods = List(TFCPeriod(from = LocalDate.now(),
      until  = LocalDate.now(),
      periodEligibility = true,
      claimants = List(tfcOutputparent, tfcOutputpartner),
      children = List(tfcOutputCChild))))

  val thirtyHoursEligibilityOutput = ThirtyHoursEligibilityModel(true, true)

  val calcOutputValueAll: CalculatorOutput = CalculatorOutput(Some(BigDecimal(1000)),
    Some(BigDecimal(1000)), Some(BigDecimal(1000)))

  val calcOutputValueOnlyESC: CalculatorOutput = CalculatorOutput(None, None, Some(BigDecimal(1000)))
  val calcOutputValueOnlyTC: CalculatorOutput = CalculatorOutput(Some(BigDecimal(1000)), None)
  val calcOutputValueOnlyTFC: CalculatorOutput = CalculatorOutput(None, Some(BigDecimal(1000)), None)
  val calcOutputValueNone: CalculatorOutput = CalculatorOutput(None, None, None)

}
