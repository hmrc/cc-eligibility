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

package models.input.tc

import org.joda.time.LocalDate
import play.api.data.validation.ValidationError
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import utils.{MessagesObject, CCFormat, Periods, TCConfig}

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
                    totalIncome: BigDecimal,
                    previousTotalIncome: BigDecimal,
                    claimants: List[Claimant],
                    children: List[Child]
                    ) extends models.input.BaseTaxYear {

  def isCouple: Boolean = claimants.length > 1 && claimants.length < 3

  def claimantsGetChildcareElement(periodStart: LocalDate) : Boolean = {
    def isClaimantDisabledOrCarer(person: Claimant) = {
      determineClaimantDisabilityOrSeverity(person) || determineCarer(person)
    }

    val parent = claimants.head
    isCouple match {
      case true =>
        val partner = claimants.last
          (
            parent.isWorkingAtLeast16HoursPerWeek(periodStart) && (partner.isWorkingAtLeast16HoursPerWeek(periodStart) || isClaimantDisabledOrCarer(partner))
          ) || (
              partner.isWorkingAtLeast16HoursPerWeek(periodStart) && isClaimantDisabledOrCarer(parent)
          )
      case false => parent.isWorkingAtLeast16HoursPerWeek(periodStart)
    }
  }

  def getTotalHouseholdWorkingHours: Double = {
    isCouple match {
      case true => claimants.head.hours + claimants.tail.head.hours
      case false => claimants.head.hours
    }
  }

  def householdHasChildOrYoungPerson(now: LocalDate = LocalDate.now, isFamily: Boolean = false) : Boolean = {
    //if called from getsFamilyElement isFamily is true and also checks for the period start date is before 6th April 2017
    children.exists(child => (isFamily && child.dob.isBefore(TCConfig.childDate6thApril2017) && (child.isChild(now) || child.getsYoungAdultElement(now)))
                            || (!isFamily && (child.isChild(now) || child.getsYoungAdultElement(now))))
  }

  private def determineClaimantDisabilityOrSeverity(claimant : Claimant) : Boolean = {
    claimant.disability.disabled || claimant.disability.severelyDisabled
  }

  private def determineCarer(person: Claimant): Boolean = person.otherSupport.carersAllowance

  private def doesHouseHoldQualify(periodStart: LocalDate): Boolean = {

    def determineWorking16hours(person: Claimant): Boolean =
      person.isWorkingAtLeast16HoursPerWeek(periodStart)

    val minimumHours: Double = TCConfig.getConfig(periodStart).minimumHoursWorkedIfCouple

    val parent = claimants.head
    val partner = claimants.last
    def isCoupleWorking24Hours: Boolean = getTotalHouseholdWorkingHours >= minimumHours

    def isOneOfCoupleWorking16h = determineWorking16hours(parent) || determineWorking16hours(partner)
    def isOneOfCoupleDisabled = determineClaimantDisabilityOrSeverity(parent) || determineClaimantDisabilityOrSeverity(partner)
    def isOneOfCoupleCarer = determineCarer(parent) || determineCarer(partner)

    isOneOfCoupleWorking16h && (isOneOfCoupleDisabled || isOneOfCoupleCarer || isCoupleWorking24Hours)
  }

  def isHouseholdQualifyingForWTC(periodStart: LocalDate): Boolean = {
    if(isCouple) {
      doesHouseHoldQualify(periodStart)
    } else {
      claimants.head.isWorkingAtLeast16HoursPerWeek(periodStart)
    }
  }

  def isHouseholdQualifyingForCTC(periodStart: LocalDate): Boolean = {
    children.exists(child => child.isChild(periodStart) || child.getsYoungAdultElement(periodStart))
  }

  def getBasicElement(periodStart: LocalDate): Boolean = {
    isHouseholdQualifyingForWTC(periodStart) && householdHasChildOrYoungPerson(periodStart)
  }

  def householdGetsChildcareElement(periodStart : LocalDate) : Boolean = {
    claimantsGetChildcareElement(periodStart) && children.exists(_.getsChildcareElement(periodStart))
  }

  def gets2ndAdultElement(now : LocalDate = LocalDate.now) : Boolean = isCouple && getBasicElement(now)

  def getsLoneParentElement(now : LocalDate = LocalDate.now): Boolean = !isCouple && householdHasChildOrYoungPerson(now)

  def gets30HoursElement(periodStart : LocalDate): Boolean = {
    val taxYearConfig = TCConfig.getConfig(periodStart)
    val hours30 : Double = taxYearConfig.hours30Worked

    (householdHasChildOrYoungPerson(periodStart) && getTotalHouseholdWorkingHours >= hours30) &&
    isCouple match {
      case true => claimants.head.isWorkingAtLeast16HoursPerWeek(periodStart) || claimants.tail.head.isWorkingAtLeast16HoursPerWeek(periodStart)
      case false => true
    }
  }

  def getsFamilyElement(now: LocalDate = LocalDate.now) : Boolean = {
    householdHasChildOrYoungPerson(now, true)
  }

  def isOneOfClaimantsWorking16h(periodStart : LocalDate) : Boolean = {
    isCouple match {
      case true =>
        claimants.head.isWorkingAtLeast16HoursPerWeek(periodStart) || claimants.last.isWorkingAtLeast16HoursPerWeek(periodStart)
      case false =>
        claimants.head.isWorkingAtLeast16HoursPerWeek(periodStart)
    }
  }

}

object TaxYear extends CCFormat with MessagesObject {

  def maxChildValidation(noOfChild: List[Child]): Boolean = {
    noOfChild.length <= 25
  }

  def claimantValidation(noOfClaimant: List[Claimant]): Boolean = {
    noOfClaimant.length > 0 && noOfClaimant.length < 3
  }

  def validateIncome(income: BigDecimal): Boolean = {
    income >= BigDecimal(0.00)
  }

  implicit val taxYearReads: Reads[TaxYear] = (
    (JsPath \ "from").read[LocalDate](jodaLocalDateReads(datePattern)) and
      (JsPath \ "until").read[LocalDate](jodaLocalDateReads(datePattern)) and
        (JsPath \ "totalIncome").read[BigDecimal].filter(ValidationError(messages("cc.elig.income.less.than.0")))(validateIncome(_)) and
          (JsPath \ "previousTotalIncome").read[BigDecimal].filter(ValidationError(messages("cc.elig.income.less.than.0")))(validateIncome(_)) and
            (JsPath \ "claimants").read[List[Claimant]].filter(ValidationError(messages("cc.elig.claimant.max.min")))(x => claimantValidation(x)) and
              (JsPath \ "children").read[List[Child]].filter(ValidationError(messages("cc.elig.children.max.25")))(x => maxChildValidation(x))
    )(TaxYear.apply _)
}

case class OtherSupport(
                         carersAllowance: Boolean = false
                         )
object OtherSupport {
  implicit val otherSupportFormat = Json.format[OtherSupport]
}

case class Claimant(
                     hours: Double = 0.00,
                     isPartner: Boolean = false,
                     disability: Disability,
                     otherSupport: OtherSupport
                     ) extends models.input.BaseClaimant {

  def getDisabilityElement(periodStart : LocalDate) : Boolean = {
    isWorkingAtLeast16HoursPerWeek(periodStart) && (disability.disabled || disability.severelyDisabled)
  }

  def isClaimantQualifyingForSevereDisabilityElement : Boolean = {
    disability.severelyDisabled
  }

  def isWorkingAtLeast16HoursPerWeek(periodStart : LocalDate) : Boolean = {
    val taxYearConfig = TCConfig.getConfig(periodStart)
    val minimum : Double = taxYearConfig.minimumHoursWorked
    hours >= minimum
  }

}

object Claimant extends CCFormat {

  implicit val claimantReads: Reads[Claimant] = (
    (JsPath \ "hoursPerWeek").read[Double].orElse(Reads.pure(0.00)) and
      (JsPath \ "isPartner").read[Boolean].orElse(Reads.pure(false)) and
         (JsPath \ "disability").read[Disability] and
            (JsPath \ "otherSupport").read[OtherSupport]
    )(Claimant.apply _)
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


case class Child(
                  id: Short,
                  name: Option[String],
                  childcareCost: BigDecimal = BigDecimal(0.00),
                  childcareCostPeriod: Periods.Period,
                  dob: LocalDate,
                  disability: Disability,
                  education: Option[Education]
                  ) extends models.input.BaseChild {

  def isTurning16Before1September(periodStart: LocalDate, periodUntil : LocalDate) : (Boolean, LocalDate) = {
    val taxYearConfig = TCConfig.getConfig(periodStart)
    val ageIncrease = taxYearConfig.childAgeLimitDisabled
    isSplittingPeriodOn1stSeptemberForYear(periodStart,periodUntil, ageIncrease)
  }

  def isTurning15Before1September(periodStart: LocalDate, periodUntil : LocalDate) : (Boolean, LocalDate) = {
    val taxYearConfig = TCConfig.getConfig(periodStart)
    val ageIncrease = taxYearConfig.childAgeLimit
    isSplittingPeriodOn1stSeptemberForYear(periodStart, periodUntil ,ageIncrease)
  }

  def isTurning20InTaxYear(taxYear : TaxYear) : (Boolean, LocalDate) = {
    val taxYearConfig = TCConfig.getConfig(taxYear.from)
    val ageIncrease = taxYearConfig.youngAdultAgeLimit
    val childsBirthday = childsBirthdayDateForAge(ageIncrease)

    val requiresSplit = childsBirthday.after(taxYear.from.toDate) && childsBirthday.before(taxYear.until.toDate)

    (requiresSplit, LocalDate.fromDateFields(childsBirthday))
  }

  def inEducation(periodStart : LocalDate) : Boolean = {
    val taxYearConfig = TCConfig.getConfig(periodStart)
    education match {
      case Some(x) =>
        if (x.inEducation) {
          val childsStartEducationAgeLimit: Int = taxYearConfig.childAgeLimitEducation
          val childs19thBirthday = childsBirthdayDateForAge(years = childsStartEducationAgeLimit)
          val educationStartDate = x.startDate
          educationStartDate.toDate.before(childs19thBirthday)
        } else {
          false
        }
      case None => false
    }
  }

  def isDisabled : Boolean = {
    disability.severelyDisabled || disability.disabled
  }

  def isChild(periodStart: LocalDate) : Boolean = {
    val taxYearConfig = TCConfig.getConfig(periodStart)
    val threshold16 = taxYearConfig.childAgeLimitDisabled
    val childAge = age(periodStart)

    val child16Birthday = childsBirthdayDateForAge(years = threshold16)
    val september1stFollowing16thBirthday = TCConfig.september1stFollowingChildBirthday(LocalDate.fromDateFields(child16Birthday))
    val septemberRule = periodStart.toDate.before(september1stFollowing16thBirthday.toDate)

    // child is born > -1 && hasn't passed their 16th birthday september checkpoint
    childAge > -1 && septemberRule
  }

  def getsYoungAdultElement(periodStart: LocalDate) : Boolean = {
    val taxYearConfig = TCConfig.getConfig(periodStart)
    val childLimit = taxYearConfig.childAgeLimitDisabled
    val youngAdultLimit = taxYearConfig.youngAdultAgeLimit

    val childAge = age(periodStart)
    (childAge >= childLimit) && inEducation(periodStart) && childAge < youngAdultLimit
  }

  def getsDisabilityElement(periodStart: LocalDate) : Boolean = {
    (isChild(periodStart) || getsYoungAdultElement(periodStart)) && (disability.disabled || disability.severelyDisabled)
  }

  def getsSevereDisabilityElement(periodStart: LocalDate) : Boolean = {
    (isChild(periodStart) || getsYoungAdultElement(periodStart)) && disability.severelyDisabled
  }

  def getsChildcareElement(periodStart: LocalDate) : Boolean = {
    val taxYearConfig = TCConfig.getConfig(periodStart)
    val threshold16 = taxYearConfig.childAgeLimitDisabled
    val threshold15 = taxYearConfig.childAgeLimit
    val childAge = age(periodStart)

    val childcareRule = childcareCost > BigDecimal(0.00)
    val child16Birthday = childsBirthdayDateForAge(years = threshold16)
    val child15Birthday = childsBirthdayDateForAge(years = threshold15)

    val september1stFollowing16thBirthday = TCConfig.september1stFollowingChildBirthday(LocalDate.fromDateFields(child16Birthday))
    val september1stFollowing15thBirthday = TCConfig.september1stFollowingChildBirthday(LocalDate.fromDateFields(child15Birthday))

    val disabledBirthdayRule = periodStart.toDate.before(september1stFollowing16thBirthday.toDate)
    val disabledAgeRule = childAge > -1 && childAge <= threshold16

    val birthdayRule = periodStart.toDate.before(september1stFollowing15thBirthday.toDate)
    val abledAgeRule = childAge > -1 && childAge <= threshold15

    if (isDisabled) {
      disabledAgeRule && childcareRule && disabledBirthdayRule
    } else {
      abledAgeRule && childcareRule && birthdayRule
    }
  }
}

object Child extends CCFormat with MessagesObject {
  val nameLength = 25
  def validID(id: Short): Boolean = {
    id >= 0
  }

  def childSpendValidation(cost: BigDecimal) : Boolean = {
    cost >= BigDecimal(0.00)
  }

  implicit val childReads: Reads[Child] = (
    (JsPath \ "id").read[Short].filter(ValidationError(messages("cc.elig.id.should.not.be.less.than.0")))(x => validID(x)) and
      (JsPath \ "name").readNullable[String](maxLength[String](nameLength)) and
        (JsPath \ "childcareCost").read[BigDecimal].filter(ValidationError(messages("cc.elig.childcare.spend.too.low")))(x => childSpendValidation(x)) and
          (JsPath \ "childcareCostPeriod").read[Periods.Period] and
            (JsPath \ "dob").read[LocalDate](jodaLocalDateReads(datePattern)) and
              (JsPath \ "disability").read[Disability] and
                (JsPath \ "education").readNullable[Education]
    )(Child.apply _)
}

case class Education(
                      inEducation: Boolean = false,
                      startDate: LocalDate
                      )

object Education extends CCFormat {
  implicit val educationReads : Reads[Education] = (
    (JsPath \ "inEducation").read[Boolean].orElse(Reads.pure(false)) and
      (JsPath \ "startDate").read[LocalDate](jodaLocalDateReads(datePattern))
    )(Education.apply _)
}
