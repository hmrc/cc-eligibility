package models.output

import models.output.esc.ESCEligibilityOutput
import models.output.tc.TCEligibilityOutput
import models.output.tfc.TFCEligibilityOutput
import play.api.libs.json.{Json, Writes}

case class CalculatorInput(tc: Option[TCEligibilityOutput], tfc: Option[TFCEligibilityOutput], esc: Option[ESCEligibilityOutput])

object CalculatorInput {
  implicit val calculatorInput: Writes[CalculatorInput] = Json.writes[CalculatorInput]
}