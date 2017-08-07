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
import models.output.tfc._
import org.joda.time.LocalDate
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
        val test = SUT.buildESCResults(escEligibilityOutputAllTrue, calcOutputValueAll, schemeResultsESCOnlyExistsInput)
        val expectedResult = SchemeResults(List(escSchemeOutput),false,false)
        test shouldBe expectedResult
      }
      "no result exists" in {
        val test = SUT.buildESCResults(escEligibilityOutputAllTrue, calcOutputValueAll, schemeResultsEmptyInput)
        val expectedResult = SchemeResults(List(escSchemeOutput),false,false)
        test shouldBe expectedResult
      }
      "other results exists" in {
        val test = SUT.buildESCResults(escEligibilityOutputAllTrue, calcOutputValueAll, schemeResultsFullInput)
        val expectedResult = SchemeResults(List(escSchemeOutput, tcSchemeInput, tfcSchemeInput),false,false)
        test shouldBe expectedResult
      }
      "amount is missing" in {
        val test = SUT.buildESCResults(escEligibilityOutputAllTrue, calcOutputNoESCValue, schemeResultsFullInput)
        val expectedResult = SchemeResults(List(escSchemeOutputZero, tcSchemeInput, tfcSchemeInput),false,false)
        test shouldBe expectedResult
      }
    }

    "build a SchemaResults Object for TFC" when {
      "result already exists" in {
        val test = SUT.buildTFCResults(tfcEligibilityOutput, calcOutputValueAll, schemeResultsTFCCOnlyExistsInput)
        val expectedResult = SchemeResults(List(tfcSchemeOutput),false,false)
        test shouldBe expectedResult
      }
      "no result exists" in {
        val test = SUT.buildTFCResults(tfcEligibilityOutput, calcOutputValueAll, schemeResultsEmptyInput)
        val expectedResult = SchemeResults(List(tfcSchemeOutput),false,false)
        test shouldBe expectedResult
      }
      "other results exists" in {
        val test = SUT.buildTFCResults(tfcEligibilityOutput, calcOutputValueAll, schemeResultsFullInput)
        val expectedResult = SchemeResults(List(escSchemeOutput, tcSchemeInput, tfcSchemeInput),false,false)
        test shouldBe expectedResult
      }
      "amount is missing" in {
        val test = SUT.buildTFCResults(tfcEligibilityOutput, calcOutputNoTFCValue, schemeResultsFullInput)
        val expectedResult = SchemeResults(List(escSchemeOutputZero, tcSchemeInput, tfcSchemeInput),false,false)
        test shouldBe expectedResult
      }
    }

  }

  //Values from the calculator
  val calcOutputNoTCValue: CalculatorOutput = CalculatorOutput(None, Some(BigDecimal(1000)), Some(BigDecimal(1000)))
  val calcOutputNoTFCValue: CalculatorOutput = CalculatorOutput(Some(BigDecimal(1000)), None, Some(BigDecimal(1000)))
  val calcOutputNoESCValue: CalculatorOutput = CalculatorOutput(Some(BigDecimal(1000)), Some(BigDecimal(1000)), None)
  val calcOutputValueAll: CalculatorOutput = CalculatorOutput(Some(BigDecimal(1000)), Some(BigDecimal(1000)), Some(BigDecimal(1000)))


//Values from eligibility
  //val escEligibilityOutputAllFalse = ESCEligibilityOutput(taxYears = List[ESCTaxYear](), eligibility  = false, parentEligibility  = false, partnerEligibility  = false, location = "england")
  val escEligibilityOutputAllTrue = ESCEligibilityOutput(taxYears = List[ESCTaxYear](), eligibility  = true, parentEligibility  = true, partnerEligibility  = true, location = "england")

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


//Values for schemeResults that have already been calculated
  val escSchemeInput = Scheme(name = SchemeEnum.ESCELIGIBILITY, amount = BigDecimal(10), escClaimantEligibility = Some(EscClaimantEligibility(true,true)), taxCreditsEligibility = None)
  val tcSchemeInput = Scheme(name = SchemeEnum.TCELIGIBILITY, amount = BigDecimal(10), escClaimantEligibility = None, taxCreditsEligibility = Some(TaxCreditsEligibility(true,true)))
  val tfcSchemeInput = Scheme(name = SchemeEnum.TFCELIGIBILITY, amount = BigDecimal(10))

  val schemeResultsEmptyInput = SchemeResults(schemes = List[Scheme](), tfcRollout = false, threeHrsRollout = false)
  val schemeResultsESCOnlyExistsInput = SchemeResults(schemes = List[Scheme](escSchemeInput), tfcRollout = false, threeHrsRollout = false)
  val schemeResultsTFCCOnlyExistsInput = SchemeResults(schemes = List[Scheme](tfcSchemeInput), tfcRollout = false, threeHrsRollout = false)
  val schemeResultsFullInput = SchemeResults(schemes = List[Scheme](escSchemeInput, tcSchemeInput, tfcSchemeInput), tfcRollout = false, threeHrsRollout = false)

//values for expected outputs
  val escSchemeOutputZero = Scheme(name = SchemeEnum.ESCELIGIBILITY, amount = BigDecimal(0.0), escClaimantEligibility = Some(EscClaimantEligibility(true,true)), taxCreditsEligibility = None)
  val escSchemeOutput = Scheme(name = SchemeEnum.ESCELIGIBILITY, amount = BigDecimal(1000), escClaimantEligibility = Some(EscClaimantEligibility(true,true)), taxCreditsEligibility = None)

  val tfcSchemeOutput = Scheme(name = SchemeEnum.TFCELIGIBILITY, amount = BigDecimal(1000), None, None)
}
