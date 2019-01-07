/*
 * Copyright 2019 HM Revenue & Customs
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
import models.output.tfc._
import models.output.{EscClaimantEligibility, Scheme, SchemeResults, TaxCreditsEligibility}
import org.joda.time.LocalDate
import org.scalatest.mockito.MockitoSugar
import utils.{CCConfigSpec, Periods}

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
        val expectedResult = SchemeResults(List(escSchemeInput, tcSchemeInput, tfcSchemeOutput),false,false)
        test shouldBe expectedResult
      }
      "amount is missing" in {
        val test = SUT.buildTFCResults(tfcEligibilityOutput, calcOutputNoTFCValue, schemeResultsFullInput)
        val expectedResult = SchemeResults(List(escSchemeInput, tcSchemeInput, tfcSchemeOutputZero),false,false)
        test shouldBe expectedResult
      }

      "tfcRollout is true"in {
        val test = SUT.buildTFCResults(tfcEligibilityOutputRolloutTrue, calcOutputValueAll, schemeResultsFullInput)
        val expectedResult = SchemeResults(List(escSchemeInput, tcSchemeInput, tfcSchemeOutput),true,false)
        test shouldBe expectedResult
      }
    }

    "build a SchemaResults Object for TC" when {
      "result already exists" in {
        val test = SUT.buildTCResults(tcEligibilityOutputAllTrue, calcOutputValueAll, schemeResultsTCOnlyExistsInput)
        val expectedResult = SchemeResults(List(tcSchemeOutput),false,false)
        test shouldBe expectedResult
      }

      "no result exists" in {
        val test = SUT.buildTCResults(tcEligibilityOutputAllTrue, calcOutputValueAll, schemeResultsEmptyInput)
        val expectedResult = SchemeResults(List(tcSchemeOutput),false,false)
        test shouldBe expectedResult
      }

      "other results exists" in {
        val test = SUT.buildTCResults(tcEligibilityOutputAllTrue, calcOutputValueAll, schemeResultsFullInput)
        val expectedResult = SchemeResults(List(escSchemeInput, tcSchemeOutput, tfcSchemeInput),false,false)
        test shouldBe expectedResult
      }

      "amount is missing" in {
        val test = SUT.buildTCResults(tcEligibilityOutputAllTrue, calcOutputNoTCValue, schemeResultsFullInput)
        val expectedResult = SchemeResults(List(escSchemeInput, tcSchemeOutputZero, tfcSchemeInput),false,false)
        test shouldBe expectedResult
      }
    }

  }

  //Values from the calculator
  val calcOutputNoTCValue: Option[CalculatorOutput] = Some(CalculatorOutput(None, Some(BigDecimal(1000)), Some(BigDecimal(1000))))
  val calcOutputNoTFCValue: Option[CalculatorOutput] = Some(CalculatorOutput(Some(BigDecimal(1000)), None, Some(BigDecimal(1000))))
  val calcOutputNoESCValue: Option[CalculatorOutput] = Some(CalculatorOutput(Some(BigDecimal(1000)), Some(BigDecimal(1000)), None))
  val calcOutputValueAll: Option[CalculatorOutput] = Some(CalculatorOutput(Some(BigDecimal(1000)), Some(BigDecimal(1000)), Some(BigDecimal(1000))))

//Values from eligibility
  val escEligibilityOutputAllTrue = ESCEligibilityOutput(taxYears = List[ESCTaxYear](), eligibility  = true, parentEligibility  = true, partnerEligibility  = true, location = Some(LocationEnum.ENGLAND))
  val tcEligibilityOutputAllTrue = TCEligibilityOutput(taxYears =List[TCTaxYear](), eligible  = true, wtc  = true, ctc  = true)

  val tfcOutputparent = TFCOutputClaimant(qualifying = true, isPartner = false)
  val tfcOutputpartner = TFCOutputClaimant(qualifying = true, isPartner = true)
  val tfcOutputCChild = TFCOutputChild( id = 0,
                                        qualifying = true,
                                        from = None,
                                        until = None,
                                        tfcRollout = false, //Not required in frontend
                                        childcareCost = BigDecimal(0),
                                        childcareCostPeriod = Periods.Monthly,
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

//Values for schemeResults that have already been calculated
  val escSchemeInput = Scheme(name = SchemeEnum.ESCELIGIBILITY, amount = BigDecimal(10), escClaimantEligibility = Some(EscClaimantEligibility(true,true)))
  val tcSchemeInput = Scheme(name = SchemeEnum.TCELIGIBILITY, amount = BigDecimal(10), taxCreditsEligibility = Some(TaxCreditsEligibility(true,true)))
  val tfcSchemeInput = Scheme(name = SchemeEnum.TFCELIGIBILITY, amount = BigDecimal(10))

  val schemeResultsEmptyInput = SchemeResults(schemes = List[Scheme](), tfcRollout = false, thirtyHrsRollout = false)
  val schemeResultsESCOnlyExistsInput = SchemeResults(schemes = List[Scheme](escSchemeInput), tfcRollout = false, thirtyHrsRollout = false)
  val schemeResultsTFCCOnlyExistsInput = SchemeResults(schemes = List[Scheme](tfcSchemeInput), tfcRollout = false, thirtyHrsRollout = false)
  val schemeResultsTCOnlyExistsInput = SchemeResults(schemes = List[Scheme](tcSchemeInput), tfcRollout = false, thirtyHrsRollout = false)
  val schemeResultsFullInput = SchemeResults(schemes = List[Scheme](escSchemeInput, tcSchemeInput, tfcSchemeInput), tfcRollout = false, thirtyHrsRollout = false)

//values for expected outputs
  val escSchemeOutputZero = Scheme(name = SchemeEnum.ESCELIGIBILITY, amount = BigDecimal(0.0), escClaimantEligibility = Some(EscClaimantEligibility(true,true)))
  val escSchemeOutput = Scheme(name = SchemeEnum.ESCELIGIBILITY, amount = BigDecimal(1000), escClaimantEligibility = Some(EscClaimantEligibility(true,true)))

  val tfcSchemeOutputZero = Scheme(name = SchemeEnum.TFCELIGIBILITY, amount = BigDecimal(0.0), None, None)
  val tfcSchemeOutput = Scheme(name = SchemeEnum.TFCELIGIBILITY, amount = BigDecimal(1000), None, None)

  val tcSchemeOutputZero = Scheme(name = SchemeEnum.TCELIGIBILITY, amount = BigDecimal(0.0), taxCreditsEligibility = Some(TaxCreditsEligibility(true,true)))
  val tcSchemeOutput = Scheme(name = SchemeEnum.TCELIGIBILITY, amount = BigDecimal(1000), taxCreditsEligibility = Some(TaxCreditsEligibility(true,true)))

}
