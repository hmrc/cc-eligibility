/*
 * Copyright 2020 HM Revenue & Customs
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

package models.input.tfc

import config.ConfigConstants
import models.input.BaseChild
import org.joda.time.LocalDate
import play.api.i18n.Lang
import play.api.libs.functional.syntax._
import play.api.libs.json.JodaReads._
import play.api.libs.json._
import uk.gov.hmrc.http.HeaderCarrier
import utils._

/*
This is the Payload input class from cc-frontend to cc-eligibility
 */

case class TFCEligibilityInput(
                                from: LocalDate,
                                numberOfPeriods: Short,
                                location: String,
                                claimants: List[TFCClaimant],
                                children: List[TFCChild]
                ) {
  def validMaxEarnings(implicit req: play.api.mvc.Request[_], hc: HeaderCarrier): Boolean = {
    val parent = claimants.head
    val maxEarningsParent = parent.maximumEarnings

    if(claimants.length > 1) {
      val partner = claimants.last
      val maxEarningsPartner = partner.maximumEarnings

      (maxEarningsParent, maxEarningsPartner) match {
        case (Some(false), Some(false)) => true
        case (None, None) => true // to ensure existing live application satisfy
        case (Some(false), None) => true
        case (None, Some(false)) => true
        case (_, _) => false
      }
    } else {
      !maxEarningsParent.getOrElse(false) // default should return true to ensure existing live application satisfy
    }
  }

}

object TFCEligibilityInput extends CCFormat with MessagesObject {

  implicit val lang: Lang = Lang("en")

  def maxChildValidation(noOfChild: List[TFCChild]): Boolean = {
    noOfChild.length <= 25
  }

  def claimantValidation(noOfClaimant: List[TFCClaimant]): Boolean = {
    noOfClaimant.nonEmpty && noOfClaimant.length < 3
  }

  implicit val tfcReads: Reads[TFCEligibilityInput] = (
    (JsPath \ "from").read[LocalDate](jodaLocalDateReads(datePattern)) and
      (JsPath \ "numberOfPeriods").read[Short].orElse(Reads.pure(1)) and
        (JsPath \ "location").read[String] and
          (JsPath \ "claimants").read[List[TFCClaimant]].filter(JsonValidationError(messages("cc.elig.claimant.max.min")))(x => claimantValidation(x)) and
            (JsPath \ "children").read[List[TFCChild]].filter(JsonValidationError(messages("cc.elig.children.max.25")))(x => maxChildValidation(x))
    )(TFCEligibilityInput.apply _)
}

case class TFCIncome(
                   employmentIncome: Option[BigDecimal],
                   pension: Option[BigDecimal],
                   otherIncome: Option[BigDecimal]
                 )
object TFCIncome {
  implicit val formats = Json.format[TFCIncome]
}

case class TFCClaimant(
                        previousIncome: Option[TFCIncome] = None,
                        currentIncome: Option[TFCIncome] = None,
                        hoursPerWeek: Double = 0.00,
                        isPartner: Boolean = false,
                        disability: TFCDisability,
                        carersAllowance: Boolean = false,
                        minimumEarnings: TFCMinimumEarnings,
                        age: Option[String],
                        employmentStatus: Option[String] = None,
                        selfEmployedSelection: Option[Boolean] = None,
                        maximumEarnings: Option[Boolean] = None
                     ) {
  def totalIncome: BigDecimal = {
    val (currentEmployment, currentvOther, currentPension) = getIncomeElements(previousIncome, currentIncome)
    getTotalTFCIncome(currentEmployment.getOrElse(ConfigConstants.defaultAmount),
      currentvOther.getOrElse(ConfigConstants.defaultAmount),
      currentPension.getOrElse(ConfigConstants.defaultAmount))
  }

  private def determineIncomeElems(income: Option[TFCIncome]) = income  match {
    case Some(x) => (x.employmentIncome, x.otherIncome, x.pension)
    case _ => (None, None, None)
  }

  private def getIncomeElements(previousIncome: Option[TFCIncome], currentIncome: Option[TFCIncome] ) = {

    val (empPrevious, otherPrevious, pensionPrevious) = determineIncomeElems(previousIncome)
    val (emp, other, pension) = determineIncomeElems(currentIncome)

    (if(emp.isDefined) emp else empPrevious,
    if(other.isDefined) other else otherPrevious,
    if(pension.isDefined) pension else pensionPrevious)
  }

  private def getTotalTFCIncome(employmentIncome: BigDecimal,
                                 otherIncome: BigDecimal,
                                 pension: BigDecimal) = {
    employmentIncome + otherIncome - pension * ConfigConstants.noOfMonths
  }

  def getNWMPerAge(taxYearConfig: TFCTaxYearConfig): (Int, String) = age match {
    case Some("under-18") => (taxYearConfig.nmwUnder18, "under-18")
    case Some("18-20") => (taxYearConfig.nmw18To20, "18-20")
    case Some("21-24") => (taxYearConfig.nmw21To24, "21-24")
    case _ => (taxYearConfig.nmw25Over, "25 or over") //25 or over
  }
}

object TFCClaimant {

  implicit val claimantReads: Reads[TFCClaimant] = (
    (JsPath \ "previousIncome").readNullable[TFCIncome] and
      (JsPath \ "currentIncome").readNullable[TFCIncome] and
        (JsPath \ "hoursPerWeek").read[Double].orElse(Reads.pure(0.00)) and
          (JsPath \ "isPartner").read[Boolean].orElse(Reads.pure(false)) and
            (JsPath \ "disability").read[TFCDisability] and
              (JsPath \ "carersAllowance").read[Boolean].orElse(Reads.pure(false)) and
                (JsPath \ "minimumEarnings").read[TFCMinimumEarnings] and
                  (JsPath \ "age").readNullable[String] and
                    (JsPath \ "employmentStatus").readNullable[String] and
                      (JsPath \ "selfEmployedSelection").readNullable[Boolean] and
                        (JsPath \ "maximumEarnings").readNullable[Boolean]
    )(TFCClaimant.apply _)
}

case class TFCMinimumEarnings(
                          selection: Boolean = true,
                          amount: BigDecimal = 0.00
                          )

object TFCMinimumEarnings {
  implicit val minEarningsRead: Reads[TFCMinimumEarnings] = (
    (JsPath \ "selection").read[Boolean].orElse(Reads.pure(true)) and
      (JsPath \ "amount").read[BigDecimal].orElse(Reads.pure(0.00))
  )(TFCMinimumEarnings.apply _)
}

case class TFCDisability(
                       disabled: Boolean = false,
                       severelyDisabled: Boolean = false
                       )

object TFCDisability {
  implicit val disabilityReads: Reads[TFCDisability] = (
    (JsPath \ "disabled").read[Boolean].orElse(Reads.pure(false)) and
      (JsPath \ "severelyDisabled").read[Boolean].orElse(Reads.pure(false))
    )(TFCDisability.apply _)
}

case class TFCChild(
                    id: Short,
                    childcareCost: BigDecimal = BigDecimal(0.00),
                    childcareCostPeriod: Periods.Period = Periods.Monthly,
                    dob: LocalDate,
                    disability: TFCDisability
                    ) extends BaseChild {

  def isDisabled: Boolean = {
    disability.severelyDisabled || disability.disabled
  }
}

object TFCChild extends CCFormat with MessagesObject {

  implicit val lang: Lang = Lang("en")

  def validID(id: Short): Boolean = {
    id >= 0
  }

  def childSpendValidation(cost: BigDecimal): Boolean = {
    cost >= BigDecimal(0.00)
  }

  implicit val childReads: Reads[TFCChild] = (
    (JsPath \ "id").read[Short].filter(JsonValidationError(messages("cc.elig.id.should.not.be.less.than.0")))(x => validID(x)) and
      (JsPath \ "childcareCost").read[BigDecimal].filter(JsonValidationError(messages("cc.elig.childcare.spend.too.low")))(x => childSpendValidation(x)) and
        (JsPath \ "childcareCostPeriod").read[Periods.Period] and
          (JsPath \ "dob").read[LocalDate](jodaLocalDateReads(datePattern)) and
            (JsPath \ "disability").read[TFCDisability]
    )(TFCChild.apply _)
}
