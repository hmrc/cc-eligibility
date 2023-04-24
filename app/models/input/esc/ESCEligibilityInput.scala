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

package models.input.esc

import javax.inject.Inject
import models.LocationEnum.LocationEnum
import models.input.BaseTaxYear
import org.joda.time.LocalDate
import play.api.Play
import play.api.i18n.Lang
import play.api.libs.functional.syntax._
import play.api.libs.json.JodaReads._
import play.api.libs.json.JodaWrites._
import play.api.libs.json._
import utils.Periods.Period
import utils.{CCConfig, CCFormat, ESCConfig, Periods}

case class ESCEligibilityInput(escTaxYears: List[ESCTaxYear],
                               location: Option[LocationEnum] = None)

object ESCEligibilityInput {
  implicit val requestFormat: Reads[ESCEligibilityInput] = Json.reads[ESCEligibilityInput]
}

case class ESCTaxYear(
                       from: LocalDate,
                       until: LocalDate,
                       claimants: List[ESCClaimant],
                       children: List[ESCChild]
                  ) extends BaseTaxYear

object ESCTaxYear extends CCFormat {

  implicit val lang: Lang = Lang("en")

  def maxChildValidation(noOfChild: List[ESCChild]): Boolean = {
    noOfChild.length <= 25
  }

  def claimantValidation(noOfClaimant: List[ESCClaimant]): Boolean = {
    noOfClaimant.nonEmpty && noOfClaimant.length < 3
  }

  implicit val taxYearReads: Reads[ESCTaxYear] = (
    (JsPath \ "from").read[LocalDate](jodaLocalDateReads(datePattern)) and
      (JsPath \ "until").read[LocalDate](jodaLocalDateReads(datePattern)) and
        (JsPath \ "claimants").read[List[ESCClaimant]].filter(JsonValidationError("At least one claimant or at max 2 claimants allowed"))(x => claimantValidation(x)) and
          (JsPath \ "children").read[List[ESCChild]].filter(JsonValidationError("Max 25 children allowed"))(x => maxChildValidation(x))
    )(ESCTaxYear.apply _)
}

case class ESCIncome(
                      employmentIncome : Option[BigDecimal] = None,
                      pension : Option[BigDecimal] = None,
                      taxCode: Option[String] = None
                    )

object ESCIncome {
  implicit val incomeFormat = Json.format[ESCIncome]
}

case class ESCClaimant(
                     isPartner: Boolean = false,
                     employerProvidesESC : Boolean = false,
                     previousIncome: Option[ESCIncome] = None,
                     currentIncome: Option[ESCIncome] = None
                   ) {
  def isClaimantQualifyingForESC : Boolean = {
    employerProvidesESC
  }
}

object ESCClaimant extends CCFormat {
  implicit val claimantReads: Reads[ESCClaimant] = (
    (JsPath \ "isPartner").read[Boolean].orElse(Reads.pure(false)) and
      (JsPath \ "employerProvidesESC").read[Boolean].orElse(Reads.pure(false)) and
        (JsPath \ "previousIncome").readNullable[ESCIncome] and
          (JsPath \ "currentIncome").readNullable[ESCIncome]
    )(ESCClaimant.apply _)
}

case class ESCChild @Inject() (
                   id: Short,
                   dob: LocalDate,
                   childCareCost: BigDecimal,
                   childCareCostPeriod: Periods.Period = Periods.Monthly,
                   disability: ESCDisability
                  ) (eSCConfig: Option[ESCConfig], ccConfig: Option[CCConfig]) extends models.input.BaseChild(ccConfig) {

  def createWithConfig(escChild: ESCChild, eSCConfig: ESCConfig, ccConfig: CCConfig): ESCChild = {
    new ESCChild(escChild.id, escChild.dob, escChild.childCareCost, escChild.childCareCostPeriod, escChild.disability)(Some(eSCConfig), Some(ccConfig))
  }

  def isTurning16Before1September(periodStart: LocalDate, periodUntil : LocalDate) : (Boolean, LocalDate) = {
    val escTaxYearConfig = eSCConfig.get.getConfig(periodStart)
    val ageIncrease = escTaxYearConfig.childAgeLimitDisabled
    isSplittingPeriodOn1stSeptemberForYear(periodStart, periodUntil, ageIncrease)
  }

  def isTurning15Before1September(periodStart: LocalDate, periodUntil : LocalDate ) : (Boolean, LocalDate) = {
    val escTaxYearConfig = eSCConfig.get.getConfig(periodStart)
    val ageIncrease = escTaxYearConfig.childAgeLimit
    isSplittingPeriodOn1stSeptemberForYear(periodStart, periodUntil, ageIncrease)
  }

  def isDisabled : Boolean = {
    disability.severelyDisabled || disability.disabled
  }

  def qualifiesForESC(now : LocalDate = LocalDate.now) : Boolean = {
    val escTaxYearConfig = eSCConfig.get.getConfig(now)
    val threshold15 = escTaxYearConfig.childAgeLimit
    val threshold16 = escTaxYearConfig.childAgeLimitDisabled
    val childAge = age(now)

    val child15Birthday = childsBirthdayDateForAge(years = threshold15)
    val child16Birthday = childsBirthdayDateForAge(years = threshold16)

    val september1stFollowing15thBirthday = eSCConfig.get.config.september1stFollowingChildBirthday(LocalDate.fromDateFields(child15Birthday))
    val september1stFollowing16thBirthday = eSCConfig.get.config.september1stFollowingChildBirthday(LocalDate.fromDateFields(child16Birthday))

    val child15Rule = now.toDate.before(september1stFollowing15thBirthday.toDate) && !isDisabled
    val child16Rule = now.toDate.before(september1stFollowing16thBirthday.toDate) && isDisabled

    childAge > -1 && (child15Rule || child16Rule)
  }

}

object ESCChild extends CCFormat {
  def validID(id: Short): Boolean = {
    id >= 0
  }

  def apply(id: Short, dob: LocalDate, childCareCost: BigDecimal,
            childCareCostPeriod: Period, disability: ESCDisability): ESCChild = {
    new ESCChild(id, dob, childCareCost, childCareCostPeriod, disability)(None, None)
  }

  def apply(id: Short, dob: LocalDate, childCareCost: BigDecimal,
            childCareCostPeriod: Period, disability: ESCDisability, dummyValue: Option[Boolean]): ESCChild = {
    new ESCChild(id, dob, childCareCost, childCareCostPeriod, disability)(None, None)
  }

  implicit val childReads: Reads[ESCChild] = (
    (JsPath \ "id").read[Short].filter(JsonValidationError("Child ID should not be less than 0"))(x => validID(x)) and
      (JsPath \ "dob").read[LocalDate](jodaLocalDateReads(datePattern)) and
        (JsPath \ "childCareCost").read[BigDecimal] and
          (JsPath \ "childCareCostPeriod").read[Periods.Period] and
            (JsPath \ "disability").read[ESCDisability]
    )(ESCChild.apply(_,_,_,_,_))
}

case class ESCDisability(
                       disabled: Boolean = false,
                       severelyDisabled: Boolean = false
                       )

object ESCDisability {
  implicit val disabilityReads: Reads[ESCDisability] = (
    (JsPath \ "disabled").read[Boolean].orElse(Reads.pure(false)) and
      (JsPath \ "severelyDisabled").read[Boolean].orElse(Reads.pure(false))
    )(ESCDisability.apply _)
}
