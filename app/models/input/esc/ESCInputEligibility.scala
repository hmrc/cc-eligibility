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

import org.joda.time.LocalDate
import play.api.data.validation.ValidationError
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import utils.{MessagesObject, CCFormat, ESCConfig}

case class Request(
                    payload: Payload
                  ) extends models.input.BaseRequest

object Request {
  implicit val requestFormat: Reads[Request] =
    (JsPath \ "payload").read[Payload].map { payload => Request(payload)}
}

case class Payload(
                    taxYears: List[TaxYear]
                  ) extends models.input.BasePayload

object Payload extends MessagesObject {
  def validateTaxYear(taxYears: List[TaxYear]): Boolean = {
    taxYears.length >= 1
  }

  implicit val payloadReads: Reads[Payload] =
    (JsPath \ "taxYears").read[List[TaxYear]].filter(ValidationError(messages("cc.elig.tax.year.min")))(x => validateTaxYear(x)).map { ty => Payload(ty)}
}

case class TaxYear(
                    from: LocalDate,
                    until: LocalDate,
                    claimants: List[Claimant],
                    children: List[Child]
                  ) extends models.input.BaseTaxYear

object TaxYear extends CCFormat with MessagesObject {

  def maxChildValidation(noOfChild: List[Child]): Boolean = {
    noOfChild.length <= 25
  }

  def claimantValidation(noOfClaimant: List[Claimant]): Boolean = {
    noOfClaimant.length > 0 && noOfClaimant.length < 3
  }

  implicit val taxYearReads: Reads[TaxYear] = (
    (JsPath \ "from").read[LocalDate](jodaLocalDateReads(datePattern)) and
      (JsPath \ "until").read[LocalDate](jodaLocalDateReads(datePattern)) and
        (JsPath \ "claimants").read[List[Claimant]].filter(ValidationError(messages("cc.elig.claimant.max.min")))(x => claimantValidation(x)) and
          (JsPath \ "children").read[List[Child]].filter(ValidationError(messages("cc.elig.children.max.25")))(x => maxChildValidation(x))
    )(TaxYear.apply _)
}

case class Claimant(
                     liveOrWork:  Boolean = false,
                     isPartner: Boolean = false,
                     employerProvidesESC : Boolean = false,
                     elements: ClaimantsElements
                   ) extends models.input.BaseClaimant {

  def isClaimantQualifyingForESC : Boolean = {
     elements.vouchers && employerProvidesESC && liveOrWork
  }

}

object Claimant extends CCFormat {
  implicit val claimantReads: Reads[Claimant] = (
    (JsPath \ "liveOrWork").read[Boolean].orElse(Reads.pure(false)) and
      (JsPath \ "isPartner").read[Boolean].orElse(Reads.pure(false)) and
        (JsPath \ "employerProvidesESC").read[Boolean].orElse(Reads.pure(false)) and
          (JsPath \ "elements").read[ClaimantsElements]
    )(Claimant.apply _)
}

case class ClaimantsElements(
                             // claimants qualification is determined by employer providing esc and
                             // children's qualification (if there is at least 1 qualifying child)
                             vouchers : Boolean = false
                             )

object ClaimantsElements {
  implicit val claimantReads : Reads[ClaimantsElements] =
    (JsPath \ "vouchers").read[Boolean].orElse(Reads.pure(false)).map {
      vouchers => ClaimantsElements(vouchers)
    }
}

case class Child (
                   id: Short,
                   name: Option[String],
                   dob: LocalDate,
                   disability: Disability
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

object Child extends CCFormat with MessagesObject {
  val nameLength = 25
  def validID(id: Short): Boolean = {
    id >= 0
  }

  implicit val childReads: Reads[Child] = (
    (JsPath \ "id").read[Short].filter(ValidationError(messages("cc.elig.id.should.not.be.less.than.0")))(x => validID(x)) and
      (JsPath \ "name").readNullable[String](maxLength[String](nameLength)) and
        (JsPath \ "dob").read[LocalDate](jodaLocalDateReads(datePattern)) and
          (JsPath \ "disability").read[Disability]
    )(Child.apply _)
}

case class Disability(
                       disabled: Boolean = false,
                       severelyDisabled: Boolean = false
                       )

object Disability {
  implicit val disabilityReads: Reads[Disability] = (
    (JsPath \ "disabled").read[Boolean].orElse(Reads.pure(false)) and
      (JsPath \ "severelyDisabled").read[Boolean].orElse(Reads.pure(false))
    )(Disability.apply _)
}
