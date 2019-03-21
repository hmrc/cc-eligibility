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

package models.input.tc

import models.input.BaseChild
import org.joda.time.LocalDate
import play.api.Play
import play.api.i18n.Lang
import play.api.libs.functional.syntax._
import play.api.libs.json.JodaReads._
import play.api.libs.json.JodaWrites._
import play.api.libs.json._
import utils.{MessagesObject, Periods, TCConfig}

case class TCDisability(
                         disabled: Boolean,
                         severelyDisabled: Boolean
                         )

object TCDisability {
  implicit val disabilityReads: Reads[TCDisability] = (
    (JsPath \ "disabled").read[Boolean].orElse(Reads.pure(false)) and
      (JsPath \ "severelyDisabled").read[Boolean].orElse(Reads.pure(false))
    ) (TCDisability.apply _)
}

case class TCClaimant(
                     hoursPerWeek: Double,
                     isPartner: Boolean,
                     disability: TCDisability,
                     carersAllowance: Boolean,
                     incomeBenefits: Boolean
                   ) {

  val tcConfig: TCConfig = Play.current.injector.instanceOf[TCConfig]

  def getDisabilityElement(periodStart: LocalDate): Boolean = {
    isWorkingAtLeast16HoursPerWeek(periodStart) && (disability.disabled || disability.severelyDisabled)
  }

  def isWorkingAtLeast16HoursPerWeek(periodStart: LocalDate): Boolean = {
    val taxYearConfig = tcConfig.getConfig(periodStart)
    val minimum: Double = taxYearConfig.minimumHoursWorked
    hoursPerWeek >= minimum
  }

}

object TCClaimant {

  implicit val claimantReads: Reads[TCClaimant] = (
    (JsPath \ "hoursPerWeek").read[Double].orElse(Reads.pure(0.00)) and
      (JsPath \ "isPartner").read[Boolean].orElse(Reads.pure(false)) and
        (JsPath \ "disability").read[TCDisability] and
          (JsPath \ "carersAllowance").read[Boolean].orElse(Reads.pure(false)) and
      (JsPath \ "incomeBenefits").read[Boolean].orElse(Reads.pure(false))
    ) (TCClaimant.apply _)

}

case class TCEducation(
                        inEducation: Boolean,
                        startDate: LocalDate
                        )

object TCEducation {
  implicit val educationReads: Reads[TCEducation] = (
    (JsPath \ "inEducation").read[Boolean].orElse(Reads.pure(false)) and
      (JsPath \ "startDate").read[LocalDate]
    ) (TCEducation.apply _)
}

case class TCChild(
                    id: Short,
                    childcareCost: BigDecimal,
                    childcareCostPeriod: Periods.Period = Periods.Monthly,
                    dob: LocalDate,
                    disability: TCDisability,
                    education: Option[TCEducation]
                ) extends BaseChild {

  lazy val tCConfig: TCConfig = Play.current.injector.instanceOf[TCConfig]

  def isTurning16Before1September(periodStart: LocalDate, periodUntil: LocalDate): (Boolean, LocalDate) = {
    val taxYearConfig = tCConfig.getConfig(periodStart)
    val ageIncrease = taxYearConfig.childAgeLimitDisabled
    isSplittingPeriodOn1stSeptemberForYear(periodStart, periodUntil, ageIncrease)
  }

  def isTurning15Before1September(periodStart: LocalDate, periodUntil: LocalDate): (Boolean, LocalDate) = {
    val taxYearConfig = tCConfig.getConfig(periodStart)
    val ageIncrease = taxYearConfig.childAgeLimit
    isSplittingPeriodOn1stSeptemberForYear(periodStart, periodUntil, ageIncrease)
  }

  def isTurning20InTaxYear(taxYear: TCTaxYear): (Boolean, LocalDate) = {
    val taxYearConfig = tCConfig.getConfig(taxYear.from)
    val ageIncrease = taxYearConfig.youngAdultAgeLimit
    val childsBirthday = childsBirthdayDateForAge(ageIncrease)

    val requiresSplit = childsBirthday.after(taxYear.from.toDate) && childsBirthday.before(taxYear.until.toDate)

    (requiresSplit, LocalDate.fromDateFields(childsBirthday))
  }

  def getsDisabilityElement(periodStart: LocalDate): Boolean = {
    (isChild(periodStart) || getsYoungAdultElement(periodStart)) && (disability.disabled || disability.severelyDisabled)
  }

  def getsSevereDisabilityElement(periodStart: LocalDate): Boolean = {
    (isChild(periodStart) || getsYoungAdultElement(periodStart)) && disability.severelyDisabled
  }

  def isChild(periodStart: LocalDate): Boolean = {
    val taxYearConfig = tCConfig.getConfig(periodStart)
    val threshold16 = taxYearConfig.childAgeLimitDisabled
    val childAge = age(periodStart)

    val child16Birthday = childsBirthdayDateForAge(years = threshold16)
    val september1stFollowing16thBirthday = tCConfig.config.september1stFollowingChildBirthday(LocalDate.fromDateFields(child16Birthday))
    val septemberRule = periodStart.toDate.before(september1stFollowing16thBirthday.toDate)

    // child is born > -1 && hasn't passed their 16th birthday september checkpoint
    childAge > -1 && septemberRule
  }

  def getsYoungAdultElement(periodStart: LocalDate): Boolean = {
    val taxYearConfig = tCConfig.getConfig(periodStart)
    val childLimit = taxYearConfig.childAgeLimitDisabled
    val youngAdultLimit = taxYearConfig.youngAdultAgeLimit

    val childAge = age(periodStart)
    (childAge >= childLimit) && inEducation(periodStart) && childAge < youngAdultLimit
  }

  def inEducation(periodStart: LocalDate): Boolean = {
    val taxYearConfig = tCConfig.getConfig(periodStart)
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

  def getsChildcareElement(periodStart: LocalDate): Boolean = {
    val taxYearConfig = tCConfig.getConfig(periodStart)
    val threshold16 = taxYearConfig.childAgeLimitDisabled
    val threshold15 = taxYearConfig.childAgeLimit
    val childAge = age(periodStart)

    val childcareRule = childcareCost > BigDecimal(0.00)
    val child16Birthday = childsBirthdayDateForAge(years = threshold16)
    val child15Birthday = childsBirthdayDateForAge(years = threshold15)

    val september1stFollowing16thBirthday = tCConfig.config.september1stFollowingChildBirthday(LocalDate.fromDateFields(child16Birthday))
    val september1stFollowing15thBirthday = tCConfig.config.september1stFollowingChildBirthday(LocalDate.fromDateFields(child15Birthday))

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

  def isDisabled: Boolean = {
    disability.severelyDisabled || disability.disabled
  }
}

object TCChild extends MessagesObject {
  implicit val lang: Lang = Lang("en")

  def validID(id: Short): Boolean = {
    id >= 0
  }

  def childSpendValidation(cost: BigDecimal): Boolean = {
    cost >= BigDecimal(0.00)
  }

  implicit val childReads: Reads[TCChild] = (
    (JsPath \ "id").read[Short].filter(JsonValidationError(messages("cc.elig.id.should.not.be.less.than.0")))(x => validID(x)) and
      (JsPath \ "childcareCost").read[BigDecimal].filter(JsonValidationError(messages("cc.elig.childcare.spend.too.low")))(x => childSpendValidation(x)) and
        (JsPath \ "childcareCostPeriod").read[Periods.Period] and
          (JsPath \ "dob").read[LocalDate] and
            (JsPath \ "disability").read[TCDisability] and
              (JsPath \ "education").readNullable[TCEducation]
    ) (TCChild.apply _)
}

case class TCIncome(
                     employment: Option[List[BigDecimal]] = None,
                     pension: Option[List[BigDecimal]] = None,
                     other: Option[List[BigDecimal]] = None,
                     benefits: Option[List[BigDecimal]] = None,
                     statutory: Option[List[TCStatutoryIncome]] = None
                     )

object TCIncome {
  implicit val incomeFormat: Format[TCIncome] = Json.format[TCIncome]
}

case class TCStatutoryIncome(
                              weeks: Double = 0,
                              amount: BigDecimal = 0
                              )
object TCStatutoryIncome {
  implicit val statutoryIncomeFormat: Format[TCStatutoryIncome] = Json.format[TCStatutoryIncome]
}

case class TCTaxYear(
                      from: LocalDate,
                      until: LocalDate,
                      previousHouseholdIncome: Option[TCIncome] = None,
                      currentHouseholdIncome: Option[TCIncome] = None,
                      claimants: List[TCClaimant],
                      children: List[TCChild]
                      ) extends models.input.BaseTaxYear {

  lazy val tCConfig: TCConfig = Play.current.injector.instanceOf[TCConfig]

  def isHouseholdQualifyingForCTC(periodStart: LocalDate): Boolean = {
    children.exists(child => child.isChild(periodStart) || child.getsYoungAdultElement(periodStart))
  }

  def householdGetsChildcareElement(periodStart: LocalDate): Boolean = {
    claimantsGetChildcareElement(periodStart) && children.exists(_.getsChildcareElement(periodStart))
  }

  def claimantsGetChildcareElement(periodStart: LocalDate): Boolean = {
    def isClaimantDisabledOrCarer(person: TCClaimant) = {
      determineClaimantDisabilityOrSeverity(person) || person.carersAllowance
    }

    val parent = claimants.head
    if (isCouple) {
      val partner = claimants.last
      (
        parent.isWorkingAtLeast16HoursPerWeek(periodStart) && (partner.isWorkingAtLeast16HoursPerWeek(periodStart) || isClaimantDisabledOrCarer(partner))
        ) || (
        partner.isWorkingAtLeast16HoursPerWeek(periodStart) && isClaimantDisabledOrCarer(parent)
        )
    } else {
      parent.isWorkingAtLeast16HoursPerWeek(periodStart)
    }
  }

  def gets2ndAdultElement(now: LocalDate = LocalDate.now): Boolean = isCouple && getBasicElement(now)

  def isCouple: Boolean = claimants.length > 1 && claimants.length < 3

  def getBasicElement(periodStart: LocalDate): Boolean = {
    isHouseholdQualifyingForWTC(periodStart) && householdHasChildOrYoungPerson(periodStart)
  }

  def isHouseholdQualifyingForWTC(periodStart: LocalDate): Boolean = {
    if (isCouple) {
      doesHouseHoldQualify(periodStart)
    } else {
      claimants.head.isWorkingAtLeast16HoursPerWeek(periodStart)
    }
  }

  private def doesHouseHoldQualify(periodStart: LocalDate): Boolean = {

    def determineWorking16hours(person: TCClaimant): Boolean =
      person.isWorkingAtLeast16HoursPerWeek(periodStart)

    val minimumHours: Double = tCConfig.getConfig(periodStart).minimumHoursWorkedIfCouple

    val parent = claimants.head
    val partner = claimants.last

    def isCoupleWorking24Hours: Boolean = getTotalHouseholdWorkingHours >= minimumHours

    def isOneOfCoupleWorking16h = determineWorking16hours(parent) || determineWorking16hours(partner)

    def isOneOfCoupleDisabled = determineClaimantDisabilityOrSeverity(parent) || determineClaimantDisabilityOrSeverity(partner)

    def isOneOfCoupleCarer = parent.carersAllowance || partner.carersAllowance

    isOneOfCoupleWorking16h && (isOneOfCoupleDisabled || isOneOfCoupleCarer || isCoupleWorking24Hours)
  }

  private def determineClaimantDisabilityOrSeverity(claimant: TCClaimant): Boolean = {
    claimant.disability.disabled || claimant.disability.severelyDisabled
  }

  def getTotalHouseholdWorkingHours: Double = {
    if (isCouple) {
      claimants.head.hoursPerWeek + claimants.tail.head.hoursPerWeek
    } else {
      claimants.head.hoursPerWeek
    }
  }

  def householdHasChildOrYoungPerson(now: LocalDate = LocalDate.now, isFamily: Boolean = false): Boolean = {
    //if called from getsFamilyElement isFamily is true and also checks for the period start date is before 6th April 2017
    children.exists(child => (isFamily && child.dob.isBefore(tCConfig.childDate6thApril2017) && (child.isChild(now) || child.getsYoungAdultElement(now)))
      || (!isFamily && (child.isChild(now) || child.getsYoungAdultElement(now))))
  }

  def getsLoneParentElement(now: LocalDate = LocalDate.now): Boolean = !isCouple && householdHasChildOrYoungPerson(now)

  def gets30HoursElement(periodStart: LocalDate): Boolean = {
    val taxYearConfig = tCConfig.getConfig(periodStart)
    val hours30: Double = taxYearConfig.hours30Worked

    (householdHasChildOrYoungPerson(periodStart) && getTotalHouseholdWorkingHours >= hours30) && (
      if (isCouple) {
        claimants.head.isWorkingAtLeast16HoursPerWeek(periodStart) || claimants.tail.head.isWorkingAtLeast16HoursPerWeek(periodStart)
      } else {
        true
      }
      )
  }

  def getsFamilyElement(now: LocalDate = LocalDate.now): Boolean = {
    householdHasChildOrYoungPerson(now, isFamily = true)
  }

  def isOneOfClaimantsWorking16h(periodStart: LocalDate): Boolean = {
    if (isCouple) {
      claimants.head.isWorkingAtLeast16HoursPerWeek(periodStart) || claimants.last.isWorkingAtLeast16HoursPerWeek(periodStart)
    } else {
      claimants.head.isWorkingAtLeast16HoursPerWeek(periodStart)
    }
  }

}

object TCTaxYear extends MessagesObject {
  implicit val lang: Lang = Lang("en")

  def maxChildValidation(noOfChild: List[TCChild]): Boolean = {
    noOfChild.length <= 25
  }

  def claimantValidation(noOfClaimant: List[TCClaimant]): Boolean = {
    noOfClaimant.nonEmpty && noOfClaimant.length < 3
  }

  implicit val taxYearReads: Reads[TCTaxYear] = (
    (JsPath \ "from").read[LocalDate] and
      (JsPath \ "until").read[LocalDate] and
        (JsPath \ "previousHouseholdIncome").readNullable[TCIncome] and
          (JsPath \ "currentHouseholdIncome").readNullable[TCIncome] and
            (JsPath \ "claimants").read[List[TCClaimant]].filter(JsonValidationError(messages("cc.elig.claimant.max.min")))(x => claimantValidation(x)) and
              (JsPath \ "children").read[List[TCChild]].filter(JsonValidationError(messages("cc.elig.children.max.25")))(x => maxChildValidation(x))
    ) (TCTaxYear.apply _)
}

case class TCEligibilityInput(
                               taxYears: List[TCTaxYear]
                               )

object TCEligibilityInput extends MessagesObject {
  def validateTaxYear(taxYears: List[TCTaxYear]): Boolean = taxYears.nonEmpty

  implicit val tcEligibilityReads: Reads[TCEligibilityInput] =
    (JsPath \ "taxYears").read[List[TCTaxYear]]
      .filter(JsonValidationError(messages("cc.elig.tax.year.min")(Lang("en"))))(x => validateTaxYear(x))
      .map { ty => TCEligibilityInput(ty) }
}
