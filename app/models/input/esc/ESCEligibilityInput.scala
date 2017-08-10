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

package models.input.esc

import models.LocationEnum.LocationEnum
import models.input.BaseTaxYear
import org.joda.time.LocalDate
import play.api.data.validation.ValidationError
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import utils.{CCFormat, ESCConfig, MessagesObject, Periods}

case class ESCEligibilityInput(
                                escTaxYears: List[ESCTaxYear],
                                location: Option[LocationEnum] = None
                  )

object ESCEligibilityInput {
  implicit val requestFormat: Reads[ESCEligibilityInput] = Json.reads[ESCEligibilityInput]
}

case class ESCTaxYear(
                       from: LocalDate,
                       until: LocalDate,
                       claimants: List[ESCClaimant],
                       children: List[ESCChild]
                  ) extends BaseTaxYear

object ESCTaxYear extends CCFormat with MessagesObject {

  def maxChildValidation(noOfChild: List[ESCChild]): Boolean = {
    noOfChild.length <= 25
  }

  def claimantValidation(noOfClaimant: List[ESCClaimant]): Boolean = {
    noOfClaimant.length > 0 && noOfClaimant.length < 3
  }

  implicit val taxYearReads: Reads[ESCTaxYear] = (
    (JsPath \ "from").read[LocalDate](jodaLocalDateReads(datePattern)) and
      (JsPath \ "until").read[LocalDate](jodaLocalDateReads(datePattern)) and
        (JsPath \ "claimants").read[List[ESCClaimant]].filter(ValidationError(messages("cc.elig.claimant.max.min")))(x => claimantValidation(x)) and
          (JsPath \ "children").read[List[ESCChild]].filter(ValidationError(messages("cc.elig.children.max.25")))(x => maxChildValidation(x))
    )(ESCTaxYear.apply _)
}

case class ESCClaimant(
                     isPartner: Boolean = false,
                     employerProvidesESC : Boolean = false
                   ) {
  def isClaimantQualifyingForESC : Boolean = {
    employerProvidesESC
  }
}

object ESCClaimant extends CCFormat {
  implicit val claimantReads: Reads[ESCClaimant] = (
    (JsPath \ "isPartner").read[Boolean].orElse(Reads.pure(false)) and
      (JsPath \ "employerProvidesESC").read[Boolean].orElse(Reads.pure(false))
    )(ESCClaimant.apply _)
}

case class ESCChild(
                   id: Short,
                   dob: LocalDate,
                   childCareCost: BigDecimal,
                   childCareCostPeriod: Periods.Period = Periods.Monthly,
                   disability: ESCDisability
                  ) extends models.input.BaseChild {

  def isTurning16Before1September(periodStart: LocalDate, periodUntil : LocalDate) : (Boolean, LocalDate) = {
    val escTaxYearConfig = ESCConfig.getConfig(periodStart)
    val ageIncrease = escTaxYearConfig.childAgeLimitDisabled
    isSplittingPeriodOn1stSeptemberForYear(periodStart, periodUntil, ageIncrease)
  }

  def isTurning15Before1September(periodStart: LocalDate, periodUntil : LocalDate ) : (Boolean, LocalDate) = {
    val escTaxYearConfig = ESCConfig.getConfig(periodStart)
    val ageIncrease = escTaxYearConfig.childAgeLimit
    isSplittingPeriodOn1stSeptemberForYear(periodStart, periodUntil, ageIncrease)
  }

  def isDisabled : Boolean = {
    disability.severelyDisabled || disability.disabled
  }

  def qualifiesForESC(now : LocalDate = LocalDate.now) : Boolean = {
    val escTaxYearConfig = ESCConfig.getConfig(now)
    val threshold15 = escTaxYearConfig.childAgeLimit
    val threshold16 = escTaxYearConfig.childAgeLimitDisabled
    val childAge = age(now)

    val child15Birthday = childsBirthdayDateForAge(years = threshold15)
    val child16Birthday = childsBirthdayDateForAge(years = threshold16)

    val september1stFollowing15thBirthday = ESCConfig.september1stFollowingChildBirthday(LocalDate.fromDateFields(child15Birthday))
    val september1stFollowing16thBirthday = ESCConfig.september1stFollowingChildBirthday(LocalDate.fromDateFields(child16Birthday))

    val child15Rule = now.toDate.before(september1stFollowing15thBirthday.toDate) && !isDisabled
    val child16Rule = now.toDate.before(september1stFollowing16thBirthday.toDate) && isDisabled

    childAge > -1 && (child15Rule || child16Rule)
  }

}

object ESCChild extends CCFormat with MessagesObject {
  def validID(id: Short): Boolean = {
    id >= 0
  }

  implicit val childReads: Reads[ESCChild] = (
    (JsPath \ "id").read[Short].filter(ValidationError(messages("cc.elig.id.should.not.be.less.than.0")))(x => validID(x)) and
      (JsPath \ "dob").read[LocalDate](jodaLocalDateReads(datePattern)) and
      (JsPath \ "childCareCost").read[BigDecimal] and
      (JsPath \ "childCareCostPeriod").read[Periods.Period] and
      (JsPath \ "disability").read[ESCDisability]
    )(ESCChild.apply _)
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
