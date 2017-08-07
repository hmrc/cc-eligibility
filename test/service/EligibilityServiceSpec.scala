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

package service

import connectors.CalculatorConnector
import controllers.FakeCCEligibilityApplication
import eligibility.{ESCEligibility, FreeEntitlementEligibility, TCEligibility, TFCEligibility}
import models.mappings.{HHToESCEligibilityInput, HHToFree30hoursEligibilityInput, HHToTCEligibilityInput, HHToTFCEligibilityInput}
import models.output.{EscClaimantEligibility, Scheme, SchemeResults, TaxCreditsEligibility}
import models.output.esc.{ESCEligibilityOutput, ESCTaxYear}
import models.output.tc.{TCEligibilityOutput, TCTaxYear}
import models.output.tfc._
import models.{Claimant, Household, SchemeEnum}
import org.joda.time.LocalDate
import org.scalatest.mock.MockitoSugar
import org.mockito.Matchers.any
import org.mockito.Mockito._
import play.api.mvc.Request
import play.api.test.FakeRequest
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

class EligibilityServiceSpec extends UnitSpec with FakeCCEligibilityApplication with MockitoSugar{


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

        when(SUT.esc.eligibility(any())).thenReturn(Future(escEligibilityOutputAllTrue))
        Await.result(SUT.eligibility(request), Duration(2, "seconds")) shouldBe escSchemeOutput
      }
    }
  }


  //Values from eligibility
  val escEligibilityOutputAllTrue = ESCEligibilityOutput(taxYears = List[ESCTaxYear](), eligibility  = true, parentEligibility  = true, partnerEligibility  = true, location = "england")
  val tcEligibilityOutputAllTrue = TCEligibilityOutput(taxYears =List[TCTaxYear](), eligible  = true, wtc  = true, ctc  = true)

  val tfcOutputparent = TFCOutputClaimant(qualifying = true, isPartner = false)
  val tfcOutputpartner = TFCOutputClaimant(qualifying = true, isPartner = true)
  val tfcOutputCChild = TFCOutputChild( id = 0,
    qualifying = true,
    from = None,
    until = None,
    tfcRollout = false, //Not required in frontend
    childcareCost = BigDecimal(0),
    disability = TFCDisability())

  val tfcEligibilityOutput = TFCEligibilityOutput(from = LocalDate.now(),
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

  //values for expected outputs
  val escSchemeOutputZero = Scheme(name = SchemeEnum.ESCELIGIBILITY, amount = BigDecimal(0.0), escClaimantEligibility = Some(EscClaimantEligibility(true,true)))
  val escSchemeOutput = Scheme(name = SchemeEnum.ESCELIGIBILITY, amount = BigDecimal(1000), escClaimantEligibility = Some(EscClaimantEligibility(true,true)))

  val tfcSchemeOutputZero = Scheme(name = SchemeEnum.TFCELIGIBILITY, amount = BigDecimal(0.0), None, None)
  val tfcSchemeOutput = Scheme(name = SchemeEnum.TFCELIGIBILITY, amount = BigDecimal(1000), None, None)

  val tcSchemeOutputZero = Scheme(name = SchemeEnum.TCELIGIBILITY, amount = BigDecimal(0.0), taxCreditsEligibility = Some(TaxCreditsEligibility(true,true)))
  val tcSchemeOutput = Scheme(name = SchemeEnum.TCELIGIBILITY, amount = BigDecimal(1000), taxCreditsEligibility = Some(TaxCreditsEligibility(true,true)))


}
