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

package models.mapping

import models._
import models.input.CalculatorOutput
import models.mappings.SchemeResultsBuilder
import models.output.esc.{ESCEligibilityOutput, ESCTaxYear}
import models.output.tc.{TCEligibilityOutput, TCTaxYear}
import org.scalatest.mock.MockitoSugar
import utils.CCConfigSpec

class SchemeResultsBuilderSpec extends CCConfigSpec with MockitoSugar {

  val SUT = SchemeResultsBuilder

  "SchemeResultsBuilder" should {

    " throw an exception" when {
      "trying to build a scheme object and ESC claimant is missing" in {

        intercept[Exception] {
          Scheme(name =  SchemeEnum.ESCELIGIBILITY,
            amount = BigDecimal(0.00) ,
            escClaimantEligibility = None,
            taxCreditsEligibility = None
          )
        }
      }
      "trying to build a scheme object and tax credits claimant is missing" in {

        intercept[Exception] {
          Scheme(name =  SchemeEnum.TCELIGIBILITY,
            amount = BigDecimal(0.00) ,
            escClaimantEligibility = None,
            taxCreditsEligibility = None
          )
        }
      }
    }

    "build a SchemaResults Object for ESC" when {
      "result already exists" in {
        SUT.buildESCResults(escEligibilityOutputAllTrue, calcOutputValueAll, schemeResultsESCOnlyExistsInput) shouldBe SchemeResults(List(escSchemeOutput),false,false)
      }
      "no result exists" in {
        SUT.buildESCResults(escEligibilityOutputAllTrue, calcOutputValueAll, schemeResultsEmptyInput) shouldBe SchemeResults(List(escSchemeOutput),false,false)
      }
      "other results exists" in {
        SUT.buildESCResults(escEligibilityOutputAllTrue, calcOutputValueAll, schemeResultsFullInput) shouldBe SchemeResults(List(escSchemeOutput, tcSchemeInput, tfcSchemeInput),false,false)
      }
      "amount is missing" in {
        SUT.buildESCResults(escEligibilityOutputAllTrue, calcOutputNoESCValue, schemeResultsFullInput) shouldBe SchemeResults(List(escSchemeOutputZero, tcSchemeInput, tfcSchemeInput),false,false)
      }
    }

    "build a SchemaResults Object for TC" when {
      "result already exists" in {
        SUT.buildTCResults(tcEligibilityOutputAllTrue, calcOutputValueAll, schemeResultsTCOnlyExistsInput) shouldBe SchemeResults(List(tcSchemeOutput),false,false)
      }
      "no result exists" in {
        SUT.buildTCResults(tcEligibilityOutputAllTrue, calcOutputValueAll, schemeResultsEmptyInput) shouldBe SchemeResults(List(tcSchemeOutput),false,false)
      }
      "other results exists" in {
        SUT.buildTCResults(tcEligibilityOutputAllTrue, calcOutputValueAll, schemeResultsFullInput) shouldBe SchemeResults(List(escSchemeInput, tcSchemeOutput, tfcSchemeInput),false,false)
      }
      "amount is missing" in {
        SUT.buildTCResults(tcEligibilityOutputAllTrue, calcOutputNoTCValue, schemeResultsFullInput) shouldBe SchemeResults(List(escSchemeInput, tcSchemeOutputZero, tfcSchemeInput),false,false)
      }
    }
  }

  val calcOutputNoTCValue: CalculatorOutput = CalculatorOutput(None, Some(BigDecimal(1000)), Some(BigDecimal(1000)))
  val calcOutputNoTFCValue: CalculatorOutput = CalculatorOutput(Some(BigDecimal(1000)), None, Some(BigDecimal(1000)))
  val calcOutputNoESCValue: CalculatorOutput = CalculatorOutput(Some(BigDecimal(1000)), Some(BigDecimal(1000)), None)
  val calcOutputValueAll: CalculatorOutput = CalculatorOutput(Some(BigDecimal(1000)), Some(BigDecimal(1000)), Some(BigDecimal(1000)))

  val escEligibilityOutputAllFalse = ESCEligibilityOutput(taxYears =List[ESCTaxYear](), eligibility  = false, parentEligibility  = false, partnerEligibility  = false, location = "england")
  val escEligibilityOutputAllTrue = ESCEligibilityOutput(taxYears =List[ESCTaxYear](), eligibility  = true, parentEligibility  = true, partnerEligibility  = true, location = "england")

  val tcEligibilityOutputAllTrue = TCEligibilityOutput(taxYears =List[TCTaxYear](), eligible  = true, wtc  = true, ctc  = true)

  val escSchemeInput = Scheme(name = SchemeEnum.ESCELIGIBILITY, amount = BigDecimal(10), escClaimantEligibility = Some(EscClaimantEligibility(true,true)), taxCreditsEligibility = None)
  val tcSchemeInput = Scheme(name = SchemeEnum.TCELIGIBILITY, amount = BigDecimal(10), escClaimantEligibility = None, taxCreditsEligibility = Some(TaxCreditsEligibility(true,true)))
  val tfcSchemeInput = Scheme(name = SchemeEnum.TFCELIGIBILITY, amount = BigDecimal(10))

  val escSchemeOutputZero = Scheme(name = SchemeEnum.ESCELIGIBILITY, amount = BigDecimal(0.0), escClaimantEligibility = Some(EscClaimantEligibility(true,true)))
  val escSchemeOutput = Scheme(name = SchemeEnum.ESCELIGIBILITY, amount = BigDecimal(1000), escClaimantEligibility = Some(EscClaimantEligibility(true,true)))
  val tcSchemeOutputZero = Scheme(name = SchemeEnum.TCELIGIBILITY, amount = BigDecimal(0.0), taxCreditsEligibility = Some(TaxCreditsEligibility(true,true)))
  val tcSchemeOutput = Scheme(name = SchemeEnum.TCELIGIBILITY, amount = BigDecimal(1000), taxCreditsEligibility = Some(TaxCreditsEligibility(true,true)))
  val tfcSchemeOutput = Scheme(name = SchemeEnum.TFCELIGIBILITY, amount = BigDecimal(1000))

  val schemeResultsEmptyInput = SchemeResults(schemes = List[Scheme](), tfcRollout = false, thirtyHrsRollout = false)
  val schemeResultsESCOnlyExistsInput = SchemeResults(schemes = List[Scheme](escSchemeInput), tfcRollout = false, thirtyHrsRollout = false)
  val schemeResultsTCOnlyExistsInput = SchemeResults(schemes = List[Scheme](tcSchemeInput), tfcRollout = false, thirtyHrsRollout = false)
  val schemeResultsFullInput = SchemeResults(schemes = List[Scheme](escSchemeInput, tcSchemeInput, tfcSchemeInput), tfcRollout = false, thirtyHrsRollout = false)
}
