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

package models.input.tfc


import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

import org.joda.time.LocalDate
import play.api.data.validation.ValidationError
import play.api.i18n.Messages
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import utils.{CCFormat, Periods, TFCConfig}
import play.api.i18n.Messages.Implicits._
import play.api.Play.current

case class Request (
                     payload: Payload
                     )

object Request {
  implicit val requestFormat: Reads[Request] =
    (JsPath \ "payload").read[Payload].map { payload => Request(payload)}
}

case class Payload(
                    tfc: TFC
                    )

object Payload {
  implicit val payloadReads: Reads[Payload] =
    (JsPath \ "tfc").read[TFC].map { tfc => Payload(tfc)}
}

case class TFC(
                from: LocalDate,
                numberOfPeriods: Short,
                claimants: List[Claimant],
                children: List[Child]
                ) {

  def validHouseholdHours  : Boolean = {
    if(claimants.length > 1) {
      ((claimants.head.isWorkingAtLeast16HoursPerWeek(from)), (claimants.last.isWorkingAtLeast16HoursPerWeek(from))) match {
        case (true,true) => true
        case (true, false) => claimants.last.otherSupport.carersAllowance
        case (false, true) =>   claimants.head.otherSupport.carersAllowance
        case _ =>   false
      }
    }
    else {
      (claimants.head.isWorkingAtLeast16HoursPerWeek(from))
    }
  }
}

object TFC extends CCFormat {

  def maxChildValidation(noOfChild: List[Child]): Boolean = {
    noOfChild.length <= 25
  }

  def claimantValidation(noOfClaimant: List[Claimant]): Boolean = {
    noOfClaimant.length > 0 && noOfClaimant.length < 3
  }

  implicit val tfcReads: Reads[TFC] = (
    (JsPath \ "from").read[LocalDate](jodaLocalDateReads(datePattern)) and
      (JsPath \ "numberOfPeriods").read[Short].orElse(Reads.pure(1)) and
        (JsPath \ "claimants").read[List[Claimant]].filter(ValidationError(Messages("cc.elig.claimant.max.min")))(x => claimantValidation(x)) and
          (JsPath \ "children").read[List[Child]].filter(ValidationError(Messages("cc.elig.children.max.25")))(x => maxChildValidation(x))
    )(TFC.apply _)
}

case class Claimant(
                     liveOrWork:  Boolean = false,
                     totalIncome: BigDecimal = BigDecimal(0.00),
                     earnedIncome: BigDecimal = BigDecimal(0.00),
                     hoursPerWeek: Double = 0.00,
                     isPartner: Boolean = false,
                     disability: Disability,
                     schemesClaiming: SchemesClaiming,
                     otherSupport: OtherSupport
                     ) extends models.input.BaseClaimant {

  def isWorkingAtLeast16HoursPerWeek (periodStart:LocalDate) : Boolean = {
    val taxYearConfig = TFCConfig.getConfig(periodStart)
    val minimum : Double = taxYearConfig.minimumHoursWorked
    hoursPerWeek >= minimum
  }

  def isTotalIncomeLessThan100000(periodStart:LocalDate) : Boolean = {
    val taxYearConfig = TFCConfig.getConfig(periodStart)
    val maximumTotalIncome : Double = taxYearConfig.maxIncomePerClaimant
    totalIncome <= maximumTotalIncome
  }

  def isQualifyingForTFC(periodStart : LocalDate) : Boolean = {
      liveOrWork && isTotalIncomeLessThan100000(periodStart)
  }

}

object Claimant extends CCFormat {

  def validateIncome(income: BigDecimal): Boolean = {
    income >= BigDecimal(0.00)
  }

  implicit val claimantReads: Reads[Claimant] = (
    (JsPath \ "liveOrWork").read[Boolean].orElse(Reads.pure(false)) and
      (JsPath \ "totalIncome").read[BigDecimal].filter(ValidationError(Messages("cc.elig.income.less.than.0")))(x => validateIncome(x)) and
        (JsPath \ "earnedIncome").read[BigDecimal].filter(ValidationError(Messages("cc.elig.income.less.than.0")))(x => validateIncome(x)) and
          (JsPath \ "hoursPerWeek").read[Double].orElse(Reads.pure(0.00)) and
            (JsPath \ "isPartner").read[Boolean].orElse(Reads.pure(false)) and
              (JsPath \ "disability").read[Disability] and
                (JsPath \ "schemesClaiming").read[SchemesClaiming] and
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

case class SchemesClaiming(
                            esc: Boolean = false,
                            tc: Boolean = false,
                            uc: Boolean = false,
                            cg: Boolean = false
                            )

object SchemesClaiming{
  implicit val schemesClaimingReads: Reads[SchemesClaiming] = (
    (JsPath \ "esc").read[Boolean].orElse(Reads.pure(false)) and
      (JsPath \ "tc").read[Boolean].orElse(Reads.pure(false)) and
        (JsPath \ "uc").read[Boolean].orElse(Reads.pure(false)) and
          (JsPath \ "cg").read[Boolean].orElse(Reads.pure(false))
    )(SchemesClaiming.apply _)
}

case class OtherSupport(
                         disabilityBenefitsOrAllowances: Boolean = false,
                         severeDisabilityBenefitsOrAllowances: Boolean = false,
                         incomeBenefitsOrAllowances: Boolean = false,
                         carersAllowance: Boolean = false
                         )

object OtherSupport {
  implicit val otherSupportReads: Reads[OtherSupport] = (
    (JsPath \ "disabilityBenefitsOrAllowances").read[Boolean].orElse(Reads.pure(false)) and
      (JsPath \ "severeDisabilityBenefitsOrAllowances").read[Boolean].orElse(Reads.pure(false)) and
        (JsPath \ "incomeBenefitsOrAllowances").read[Boolean].orElse(Reads.pure(false)) and
          (JsPath \ "carersAllowance").read[Boolean].orElse(Reads.pure(false))
    )(OtherSupport.apply _)
}

case class Child  (
                    id: Short,
                    name: Option[String],
                    childcareCost: BigDecimal = BigDecimal(0.00),
                    childcareCostPeriod: Periods.Period,
                    dob: LocalDate,
                    disability: Disability
                    ) extends models.input.BaseChild {

  def isDisabled : Boolean = {
    disability.severelyDisabled || disability.disabled
  }

  def getChildBirthday(periodStart : LocalDate) : Date = {
    isDisabled match {
      case true => getChild16Birthday(periodStart)
      case _ => getChild11Birthday(periodStart)
    }
  }

  def getChild16Birthday(periodStart : LocalDate) : Date = {
    val taxYearConfig = TFCConfig.getConfig(periodStart)
    val ageIncrease = taxYearConfig.childAgeLimitDisabled
    childsBirthdayDateForAge(ageIncrease)
  }

  def getChild11Birthday(periodStart : LocalDate) : Date = {
    val taxYearConfig = TFCConfig.getConfig(periodStart)
    val ageIncrease = taxYearConfig.childAgeLimit
    childsBirthdayDateForAge(ageIncrease)
  }

  def getWeekEnd(calendar : Calendar, weekStart : Int) : Date = {
    while (calendar.get(Calendar.DAY_OF_WEEK) != weekStart || calendar.get(Calendar.DAY_OF_MONTH) == 1) {
      calendar.add(Calendar.DATE, 1)
    }
    calendar.getTime
  }

  def firstOfSeptember(septemberCalendar : Calendar, childBirthday: Date, childBirthdayCalendar: Calendar) : Date = {
    septemberCalendar.setFirstDayOfWeek(Calendar.SUNDAY)
    septemberCalendar.setTime(childBirthday) // today
    septemberCalendar.set(Calendar.MONTH, Calendar.SEPTEMBER) // september in calendar year
    septemberCalendar.set(Calendar.DAY_OF_MONTH, 1)
    septemberCalendar.set(Calendar.YEAR, childBirthdayCalendar.get(Calendar.YEAR))
    septemberCalendar.getTime

  }

  def endWeek1stOfSeptemberDate(periodStart : LocalDate) : Date = {
    val childBirthday = getChildBirthday(periodStart)  //child's 11th or 16th Birthday
    val childBirthdayCalendar : Calendar = Calendar.getInstance()  // todays date
    childBirthdayCalendar.setTime(childBirthday) // childs date of birth
    val septemberCalendar = Calendar.getInstance()
    septemberCalendar.clear()
    var endWeekOf1stSeptember = firstOfSeptember(septemberCalendar, childBirthday, childBirthdayCalendar) // end date of first week of 1st september

    if (endWeekOf1stSeptember.before(childBirthday) || childBirthday.equals(endWeekOf1stSeptember)) { // end week is before today
      septemberCalendar.add(Calendar.YEAR, 1) // must be next year (september now+1)
    }

    endWeekOf1stSeptember = getWeekEnd(septemberCalendar, Calendar.SUNDAY)
    val dateFormatter = new SimpleDateFormat("E MMM dd HH:mm:ss z yyyy")
    dateFormatter.parse(endWeekOf1stSeptember.toString)
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
              (JsPath \ "disability").read[Disability]
    )(Child.apply _)
}
