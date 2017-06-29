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

import config.ConfigConstants
import org.joda.time.LocalDate
import play.api.data.validation.ValidationError
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import service.AuditEvents
import uk.gov.hmrc.play.http.HeaderCarrier
import utils._

/*
This is the Payload input class from cc-frontend to cc-eligibility
 */

case class TFCEligibilityInput(
                from: LocalDate,
                numberOfPeriods: Short,
                location: String,
                claimants: List[Claimant],
                children: List[Child]
                ) {

  def validHouseholdMinimumEarnings(implicit req: play.api.mvc.Request[_], hc: HeaderCarrier): (Boolean) = {
    val parent = claimants.head
    if(claimants.length > 1) {
      val partner = claimants.last
      val minEarningsParent = parent.satisfyMinimumEarnings(from, parent = true, location)
      val minEarningsPartner = partner.satisfyMinimumEarnings(from, parent = false, location)
      val auditMinEarns = minEarningsParent && minEarningsPartner
      if(auditMinEarns == false) {
        AuditEvents.auditMinEarnings(auditMinEarns)
      }
      (minEarningsParent, minEarningsPartner) match {
        case (true, true) => true
        case (true, false) => partner.carersAllowance
        case (false, true) => parent.carersAllowance
        case _ => false
      }
    } else {
      val parentSatisfy = parent.satisfyMinimumEarnings(from, parent = true, location)
      if(parentSatisfy == false) {
        AuditEvents.auditMinEarnings(parentSatisfy)
      }
      parentSatisfy
    }
  }

  def validHouseholdHours  : Boolean = {
    val parent = claimants.head
    if(claimants.length > 1) {
      val partner = claimants.last
      (parent.isWorkingAtLeast16HoursPerWeek(from, location), partner.isWorkingAtLeast16HoursPerWeek(from, location)) match {
        case (true,true) => true
        case (true, false) => partner.carersAllowance
        case (false, true) => parent.carersAllowance
        case _ =>   false
      }
    } else {
      parent.isWorkingAtLeast16HoursPerWeek(from, location)
    }
  }
}

object TFCEligibilityInput extends CCFormat with MessagesObject {

  def maxChildValidation(noOfChild: List[Child]): Boolean = {
    noOfChild.length <= 25
  }

  def claimantValidation(noOfClaimant: List[Claimant]): Boolean = {
    noOfClaimant.length > 0 && noOfClaimant.length < 3
  }

  implicit val tfcReads: Reads[TFCEligibilityInput] = (
    (JsPath \ "from").read[LocalDate](jodaLocalDateReads(datePattern)) and
      (JsPath \ "numberOfPeriods").read[Short].orElse(Reads.pure(1)) and
        (JsPath \ "location").read[String] and
          (JsPath \ "claimants").read[List[Claimant]].filter(ValidationError(messages("cc.elig.claimant.max.min")))(x => claimantValidation(x)) and
            (JsPath \ "children").read[List[Child]].filter(ValidationError(messages("cc.elig.children.max.25")))(x => maxChildValidation(x))
    )(TFCEligibilityInput.apply _)
}

case class Income(
                   employmentIncome : Option[BigDecimal],
                   pension : Option[BigDecimal],
                   otherIncome : Option[BigDecimal],
                   benefits : Option[BigDecimal]
                 )
object Income {
  implicit val formats = Json.format[Income]
}

case class Claimant(
                     previousIncome: Option[Income] = None,
                     currentIncome: Option[Income] = None,
                     hoursPerWeek: Double = 0.00,
                     isPartner: Boolean = false,
                     disability: Disability,
                     carersAllowance: Boolean = false,
                     minimumEarnings: MinimumEarnings,
                     age: Option[String],
                     employmentStatus: Option[String] = None,
                     selfEmployedSelection: Option[Boolean] = None
                     ) extends models.input.BaseClaimant {

  def totalIncome: BigDecimal = {
    val (currentEmployment, currentvOther, currentPension) = getIncomeElements(previousIncome, currentIncome)
    getTotalTFCIncome(currentEmployment.getOrElse(ConfigConstants.defaultAmount),
      currentvOther.getOrElse(ConfigConstants.defaultAmount),
      currentPension.getOrElse(ConfigConstants.defaultAmount))
  }

  private def determineIncomeElems(income: Option[Income]) = income  match {
    case Some(x) => (x.employmentIncome, x.otherIncome, x.pension)
    case _ => (None, None, None)
  }

  private def getIncomeElements(previousIncome: Option[Income], currentIncome: Option[Income] ) = {

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

  def isWorkingAtLeast16HoursPerWeek (periodStart:LocalDate, location:String) : Boolean = {
    val taxYearConfig = TFCConfig.getConfig(periodStart, location)
    val minimum : Double = taxYearConfig.minimumHoursWorked
    hoursPerWeek >= minimum
  }

  def isTotalIncomeLessThan100000(periodStart:LocalDate, location: String) : Boolean = {
    val taxYearConfig = TFCConfig.getConfig(periodStart, location)
    val maximumTotalIncome : Double = taxYearConfig.maxIncomePerClaimant
    (totalIncome - taxYearConfig.personalAllowancePerClaimant) <= maximumTotalIncome
  }

  def isQualifyingForTFC(periodStart : LocalDate, location: String) : Boolean = {
      isTotalIncomeLessThan100000(periodStart, location)
  }

  def satisfyMinimumEarnings(periodStart: LocalDate, parent: Boolean, location:String)(implicit req: play.api.mvc.Request[_], hc: HeaderCarrier): Boolean = {
    val user = if(parent) {
      "Parent"
    } else {
      "Partner"
    }
    def getNWMPerAge(taxYearConfig: TFCTaxYearConfig): (Int, String) = age match {
      case Some("under-18") => (taxYearConfig.nmwUnder18, "under-18")
      case Some("18-20") => (taxYearConfig.nmw18To20, "18-20")
      case Some("21-24") => (taxYearConfig.nmw21To24, "21-24")
      case _ => (taxYearConfig.nmw25Over, "25 or over") //25 or over
    }

    val taxYearConfig = TFCConfig.getConfig(periodStart, location)
    if(minimumEarnings.selection) {
      true
    } else {
      val nmw = getNWMPerAge(taxYearConfig)
      if(minimumEarnings.amount >= nmw._1) {
        true
      } else {
        AuditEvents.auditAgeGroup(user, nmw._2)
        employmentStatus match {
          case Some("selfEmployed") =>
            AuditEvents.auditSelfEmploymentStatus(user, employmentStatus.get)
            if (selfEmployedSelection.get) {
              AuditEvents.auditSelfEmployedin1st(user, selfEmployedSelection.get)
              true
            } else {
              false
            }
          case Some("apprentice") => minimumEarnings.amount >= taxYearConfig.nmwApprentice
          case _ => false
        }
      }
    }
  }

}

object Claimant {

  implicit val claimantReads: Reads[Claimant] = (
    (JsPath \ "previousIncome").readNullable[Income] and
      (JsPath \ "currentIncome").readNullable[Income] and
        (JsPath \ "hoursPerWeek").read[Double].orElse(Reads.pure(0.00)) and
          (JsPath \ "isPartner").read[Boolean].orElse(Reads.pure(false)) and
              (JsPath \ "disability").read[Disability] and
                (JsPath \ "carersAllowance").read[Boolean].orElse(Reads.pure(false)) and
                  (JsPath \ "minimumEarnings").read[MinimumEarnings] and
                    (JsPath \ "age").readNullable[String] and
                      (JsPath \ "employmentStatus").readNullable[String] and
                        (JsPath \ "selfEmployedSelection").readNullable[Boolean]
    )(Claimant.apply _)
}

case class MinimumEarnings(
                          selection: Boolean = true,
                          amount: BigDecimal = 0.00
                          )

object MinimumEarnings {
  implicit val minEarningsRead: Reads[MinimumEarnings] = (
    (JsPath \ "selection").read[Boolean].orElse(Reads.pure(true)) and
      (JsPath \ "amount").read[BigDecimal].orElse(Reads.pure(0.00))
  )(MinimumEarnings.apply _)
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

case class Child  (
                    id: Short,
                    childcareCost: BigDecimal = BigDecimal(0.00),
                    childcareCostPeriod: Periods.Period,
                    dob: LocalDate,
                    disability: Disability
                    ) extends models.input.BaseChild {

  def isDisabled: Boolean = {
    disability.severelyDisabled || disability.disabled
  }

  def getChildBirthday(periodStart : LocalDate, location: String) : Date = {
    isDisabled match {
      case true => getChild16Birthday(periodStart, location)
      case _ => getChild11Birthday(periodStart, location)
    }
  }

  def getChild16Birthday(periodStart : LocalDate, location: String) : Date = {
    val taxYearConfig = TFCConfig.getConfig(periodStart, location)
    val ageIncrease = taxYearConfig.childAgeLimitDisabled
    childsBirthdayDateForAge(ageIncrease)
  }

  def getChild11Birthday(periodStart : LocalDate, location: String) : Date = {
    val taxYearConfig = TFCConfig.getConfig(periodStart, location)
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

  def endWeek1stOfSeptemberDate(periodStart : LocalDate, location: String) : Date = {
    val childBirthday = getChildBirthday(periodStart, location)  //child's 11th or 16th Birthday
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


object Child extends CCFormat with MessagesObject {

  def validID(id: Short): Boolean = {
    id >= 0
  }

  def childSpendValidation(cost: BigDecimal) : Boolean = {
    cost >= BigDecimal(0.00)
  }

  implicit val childReads: Reads[Child] = (
    (JsPath \ "id").read[Short].filter(ValidationError(messages("cc.elig.id.should.not.be.less.than.0")))(x => validID(x)) and
        (JsPath \ "childcareCost").read[BigDecimal].filter(ValidationError(messages("cc.elig.childcare.spend.too.low")))(x => childSpendValidation(x)) and
          (JsPath \ "childcareCostPeriod").read[Periods.Period] and
            (JsPath \ "dob").read[LocalDate](jodaLocalDateReads(datePattern)) and
              (JsPath \ "disability").read[Disability]
    )(Child.apply _)
}
