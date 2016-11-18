/*
 * Copyright 2016 HM Revenue & Customs
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
import play.api.i18n.Messages
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import utils.{CCFormat, Periods, TCConfig}

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

object Payload {
  def validateTaxYear(taxYears: List[TaxYear]): Boolean = {
    taxYears.length >= 1
  }

  implicit val payloadReads: Reads[Payload] =
    (JsPath \ "taxYears").read[List[TaxYear]].filter(ValidationError(Messages("cc.elig.tax.year.min")))(x => validateTaxYear(x)).map { ty => Payload(ty)}
}

case class TaxYear(
                    from: LocalDate,
                    until: LocalDate,
                    claimants: List[Claimant],
                    children: List[Child]
                    ) extends models.input.BaseTaxYear {

  def isCouple : Boolean = claimants.length > 1 && claimants.length < 3

  def isCoupleQualifyingForTC : Boolean = {
    isCouple match {
      case true => claimants.head.isQualifyingForTC && claimants.tail.head.isQualifyingForTC
      case false => claimants.head.isQualifyingForTC
    }
  }

  private def getChildCareElementWhen1ClaimantWorks16hrsAnd1IsIncapacitated(periodStart : LocalDate) = {
    if(claimants.head.isWorkingAtLeast16HoursPerWeek(periodStart) && claimants.head.isIncapacitated) {
      false //claimant cannot work and be incapacitated at the same time
    }
    else {
      if (claimants.tail.head.isWorkingAtLeast16HoursPerWeek(periodStart) && claimants.tail.head.isIncapacitated) {
        false //partner cannot work and be incapacitated at the same time
      }
      else {
        isCoupleQualifyingForTC // one claimant is working, another one is incapacitated (don't care about disability)
      }
    }
  }

  private def getChildCareElementBasedOnWorkingHoursAndIncapacitation(resultTuple : Tuple2[Int, Int], periodStart : LocalDate) : Boolean = {
    resultTuple match {
      case (2, 1) => isCoupleQualifyingForTC// one claimant cannot be incapacitated and both working
      case (2, 0) => isCoupleQualifyingForTC // both claimants are working, none incapacitated (don't care about disability)
      case (1, 1) => getChildCareElementWhen1ClaimantWorks16hrsAnd1IsIncapacitated(periodStart)
      case (_, 2) => false // both claimants cannot be working and incapacitated
      case (1, 0) => false // one partner is working, another does not qualify
      case (0, _) => false // no claimants working at least 16 hours (don't care about incapacitated and disability)
    }
  }

  def claimantsGetChildcareElement(periodStart : LocalDate) : Boolean = {
    isCouple match {
      case true =>
        val numberOfClaimants16Hours = claimants.foldLeft(0)((acc, claimant) => if(claimant.isWorkingAtLeast16HoursPerWeek(periodStart)) acc + 1 else acc)
        val numberOfClaimantsIncapacitated = claimants.foldLeft(0)((acc, claimant) => if (claimant.isIncapacitated) acc + 1 else acc)
        val resultTuple = (numberOfClaimants16Hours, numberOfClaimantsIncapacitated)
        getChildCareElementBasedOnWorkingHoursAndIncapacitation(resultTuple, periodStart)

      case false =>
        val claimant = claimants.head
        claimant.isQualifyingForTC && (claimant.isWorkingAtLeast16HoursPerWeek(periodStart) && !claimant.isIncapacitated)
    }
  }

  def getTotalHouseholdWorkingHours : Double = {
    isCouple match {
      case true => claimants.head.hours + claimants.tail.head.hours
      case false => claimants.head.hours
    }
  }

  def householdHasChildOrYoungPerson(now : LocalDate = LocalDate.now) : Boolean = {
    val numberOfYoungPersonsOrChildren = children.foldLeft(0)((acc, child) =>
      if(child.getsChildElement(now) || child.getsYoungAdultElement(now)) {
      acc + 1
    } else {
        acc
      })
    numberOfYoungPersonsOrChildren > 0
  }

  private def determineClaimantDisabled(claimant : Claimant) : Boolean = {
    claimant.isQualifyingForTC && (claimant.disability.disabled || claimant.disability.severelyDisabled)
  }

  private def doesHouseHoldQualify(periodStart: LocalDate, householdQualifies : Boolean) : Boolean = {

    // TODO: Remove conditions for incapacitated
    def determineIncapacitated(person: Claimant): Boolean =
      person.isQualifyingForTC && person.isIncapacitated

    // claimant can't work and being incapacitated
    def determineWorking16hours(person: Claimant): Boolean =
      person.isQualifyingForTC && person.isWorkingAtLeast16HoursPerWeek(periodStart) && !person.isIncapacitated

    def determineCarer(person: Claimant): Boolean =
      person.isQualifyingForTC && person.otherSupport.carersAllowance

    val taxYearConfig = TCConfig.getConfig(periodStart)
    val minimumHours: Double = taxYearConfig.minimumHoursWorkedIfCouple

    val claimant = claimants.head
    val partner = claimants.last

    val isCoupleWorking24Hours: Boolean =
      claimant.isQualifyingForTC && partner.isQualifyingForTC && (getTotalHouseholdWorkingHours >= minimumHours)

    val isOneOfCoupleWorking16h = determineWorking16hours(claimant) || determineWorking16hours(partner)
    val isOneOfCoupleDisabled = determineClaimantDisabled(claimant) || determineClaimantDisabled(partner)
    val isOneOfCoupleCarer = determineCarer(claimant) || determineCarer(partner)
    val isOneOfCoupeIncapacitated = determineIncapacitated(claimant) || determineIncapacitated(partner)

    if(
      isOneOfCoupleWorking16h && (
        isOneOfCoupleDisabled || isOneOfCoupleCarer || isCoupleWorking24Hours || isOneOfCoupeIncapacitated
        )
    ) {
      householdQualifies
    }
    else {
      false
    }
  }

  def getBasicElement(periodStart: LocalDate) : Boolean = {
    isCouple match {
      case true =>
        val householdQualifies : Boolean = isCoupleQualifyingForTC && householdHasChildOrYoungPerson(periodStart)
        doesHouseHoldQualify(periodStart, householdQualifies)

      case false =>
        val claimant = claimants.head
        val claimantRules = claimant.isQualifyingForTC && (claimant.isWorkingAtLeast16HoursPerWeek(periodStart) && !claimant.isIncapacitated)
        val childrenRules = householdHasChildOrYoungPerson(periodStart)
        claimantRules && childrenRules
    }
  }

  def householdGetsChildcareElement(periodStart : LocalDate) : Boolean = {
    val childrenGetChildcareCount = children.foldLeft(0)((acc, child) => if(child.getsChildcareElement(periodStart)) acc + 1 else acc)
    val childrenGetChildcareElement : Boolean = childrenGetChildcareCount > 0

    isCoupleQualifyingForTC && claimantsGetChildcareElement(periodStart) && childrenGetChildcareElement
  }

  def gets2ndAdultElement(now : LocalDate = LocalDate.now) : Boolean = isCouple && isCoupleQualifyingForTC && getBasicElement(now)

  def getsLoneParentElement(now : LocalDate = LocalDate.now): Boolean = !isCouple && isCoupleQualifyingForTC && householdHasChildOrYoungPerson(now)

  def gets30HoursElement(periodStart : LocalDate): Boolean = {
    val taxYearConfig = TCConfig.getConfig(periodStart)
    val hours30 : Double = taxYearConfig.hours30Worked

    isCouple match {
      case true => (isCoupleQualifyingForTC && householdHasChildOrYoungPerson(periodStart)
        && (getTotalHouseholdWorkingHours >= hours30)
        && (claimants.head.isWorkingAtLeast16HoursPerWeek(periodStart)
          || claimants.tail.head.isWorkingAtLeast16HoursPerWeek(periodStart)))
      case false => claimants.head.isQualifyingForTC && householdHasChildOrYoungPerson(periodStart) && getTotalHouseholdWorkingHours >= hours30
    }
  }

  def getsFamilyElement(now : LocalDate = LocalDate.now) : Boolean = {
    isCouple match {
      case true => isCoupleQualifyingForTC && householdHasChildOrYoungPerson(now)
      case false => claimants.head.isQualifyingForTC && householdHasChildOrYoungPerson(now)
    }
  }

  def getTotalHouseholdIncome : (BigDecimal, BigDecimal) = {
    isCouple match {
      case true =>
        (claimants.head.totalIncome + claimants.tail.head.totalIncome, claimants.head.previousTotalIncome + claimants.tail.head.previousTotalIncome)
      case false =>
        (claimants.head.totalIncome, claimants.head.previousTotalIncome)
    }
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

object TaxYear extends CCFormat {

  def maxChildValidation(noOfChild: List[Child]): Boolean = {
    noOfChild.length <= 25
  }

  def claimantValidation(noOfClaimant: List[Claimant]): Boolean = {
    noOfClaimant.length > 0 && noOfClaimant.length < 3
  }

  implicit val taxYearReads: Reads[TaxYear] = (
    (JsPath \ "from").read[LocalDate](jodaLocalDateReads(datePattern)) and
      (JsPath \ "until").read[LocalDate](jodaLocalDateReads(datePattern)) and
        (JsPath \ "claimants").read[List[Claimant]].filter(ValidationError(Messages("cc.elig.claimant.max.min")))(x => claimantValidation(x)) and
          (JsPath \ "children").read[List[Child]].filter(ValidationError(Messages("cc.elig.children.max.25")))(x => maxChildValidation(x))
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
                     liveOrWork:  Boolean = false,
                     isPartner: Boolean = false,
                     totalIncome: BigDecimal = BigDecimal(0.00),
                     previousTotalIncome: BigDecimal = BigDecimal(0.00),
                     disability: Disability,
                     schemesClaiming: SchemesClaiming,
                     otherSupport: OtherSupport
                     ) extends models.input.BaseClaimant {

  // TODO THIS MAY NEED EXPANDED FOR KNOCKOUT CONDITIONS FOR TC
  def isQualifyingForTC : Boolean = {
    liveOrWork
  }

  def getDisabilityElement(periodStart : LocalDate) : Boolean = {
    isQualifyingForTC && isWorkingAtLeast16HoursPerWeek(periodStart) && (disability.disabled || disability.severelyDisabled)
  }

  def isClaimantQualifyingForSevereDisabilityElement : Boolean = {
    isQualifyingForTC && disability.severelyDisabled
  }

  def isWorkingAtLeast16HoursPerWeek(periodStart : LocalDate) : Boolean = {
    val taxYearConfig = TCConfig.getConfig(periodStart)
    val minimum : Double = taxYearConfig.minimumHoursWorked
    hours >= minimum
  }

  def isWorkingAtLeast24HoursPerWeek(periodStart : LocalDate)  : Boolean = {
    val taxYearConfig = TCConfig.getConfig(periodStart)
    val minimum : Double = taxYearConfig.minimumHoursWorkedIfCouple
    hours >= minimum
  }

  def isIncapacitated : Boolean = {
    disability.incapacitated
  }

}

object Claimant extends CCFormat {

  def validateIncome(income: BigDecimal): Boolean = {
    income >= BigDecimal(0.00)
  }

  implicit val claimantReads: Reads[Claimant] = (
    (JsPath \ "hoursPerWeek").read[Double].orElse(Reads.pure(0.00)) and
      (JsPath \ "liveOrWork").read[Boolean].orElse(Reads.pure(false)) and
        (JsPath \ "isPartner").read[Boolean].orElse(Reads.pure(false)) and
          (JsPath \ "totalIncome").read[BigDecimal].filter(ValidationError(Messages("cc.elig.income.less.than.0")))(x => validateIncome(x)) and
            (JsPath \ "previousTotalIncome").read[BigDecimal].filter(ValidationError(Messages("cc.elig.income.less.than.0")))(x => validateIncome(x)) and
              (JsPath \ "disability").read[Disability] and
                (JsPath \ "schemesClaiming").read[SchemesClaiming] and
                  (JsPath \ "otherSupport").read[OtherSupport]
    )(Claimant.apply _)
}

case class Disability(
                       disabled: Boolean = false,
                       severelyDisabled: Boolean = false,
                       incapacitated : Boolean = false
                       )

object Disability {
  implicit val disabilityReads: Reads[Disability] = (
    (JsPath \ "disabled").read[Boolean].orElse(Reads.pure(false)) and
      (JsPath \ "severelyDisabled").read[Boolean].orElse(Reads.pure(false)) and
        (JsPath \ "incapacitated").read[Boolean].orElse(Reads.pure(false))
    )(Disability.apply _)
}

case class SchemesClaiming(
                            tfc: Boolean = false,
                            esc: Boolean = false,
                            tc: Boolean = false
                            )

object SchemesClaiming{
  implicit val disabilityReads: Reads[SchemesClaiming] = (
    (JsPath \ "tfc").read[Boolean].orElse(Reads.pure(false)) and
      (JsPath \ "esc").read[Boolean].orElse(Reads.pure(false)) and
      (JsPath \ "tc").read[Boolean].orElse(Reads.pure(false))
    )(SchemesClaiming.apply _)
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

  def getsChildElement(periodStart: LocalDate) : Boolean = {
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
    (getsChildElement(periodStart) || getsYoungAdultElement(periodStart)) && (disability.disabled || disability.severelyDisabled)
  }

  def getsSevereDisabilityElement(periodStart: LocalDate) : Boolean = {
    (getsChildElement(periodStart) || getsYoungAdultElement(periodStart)) && disability.severelyDisabled
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
    val ageRule = childAge > -1 && childAge <= threshold15

    if (isDisabled) {
      disabledAgeRule && childcareRule && disabledBirthdayRule
    } else {
      ageRule && childcareRule && birthdayRule
    }
  }
}


object Child extends CCFormat {
  val nameLength = 25
  def validID(id: Short): Boolean = {
    id >= 0
  }

  def childSpendValidation(cost: BigDecimal) : Boolean = {
    cost >= BigDecimal(0.00)
  }

  implicit val childReads: Reads[Child] = (
    (JsPath \ "id").read[Short].filter(ValidationError(Messages("cc.elig.id.should.not.be.less.than.0")))(x => validID(x)) and
      (JsPath \ "name").readNullable[String](maxLength[String](nameLength)) and
        (JsPath \ "childcareCost").read[BigDecimal].filter(ValidationError(Messages("cc.elig.childcare.spend.too.low")))(x => childSpendValidation(x)) and
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
