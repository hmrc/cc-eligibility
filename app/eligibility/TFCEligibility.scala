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

package eligibility

import models.input.tfc.{TFCChild, TFCClaimant, TFCEligibilityInput}
import models.output.tfc._
import service.AuditEvents
import uk.gov.hmrc.http.HeaderCarrier
import utils.TFCConfig

import java.time.LocalDate
import java.util.{Calendar, Date}
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class TFCEligibility @Inject() (auditEvent: AuditEvents, tFCConfig: TFCConfig)(implicit ec: ExecutionContext) {

  private def getWeekEnd(calendar: Calendar): Date = {
    while (calendar.get(Calendar.DAY_OF_WEEK) != Calendar.SUNDAY || calendar.get(Calendar.DAY_OF_MONTH) == 1)
      calendar.add(Calendar.DATE, 1)
    calendar.getTime
  }

  private def firstOfSeptember(
      septemberCalendar: Calendar,
      childBirthday: Date,
      childBirthdayCalendar: Calendar
  ): Date = {
    septemberCalendar.setFirstDayOfWeek(Calendar.SUNDAY)
    septemberCalendar.setTime(childBirthday)                  // today
    septemberCalendar.set(Calendar.MONTH, Calendar.SEPTEMBER) // september in calendar year
    septemberCalendar.set(Calendar.DAY_OF_MONTH, 1)
    septemberCalendar.set(Calendar.YEAR, childBirthdayCalendar.get(Calendar.YEAR))
    septemberCalendar.getTime

  }

  def endWeek1stOfSeptemberDate(periodStart: LocalDate, location: String, child: TFCChild): Date = {
    val childBirthday = getChildBirthday(periodStart, location, child) // child's 11th or 16th Birthday
    val childBirthdayCalendar: Calendar = Calendar.getInstance() // todays date
    childBirthdayCalendar.setTime(childBirthday) // childs date of birth
    val septemberCalendar               = Calendar.getInstance()
    septemberCalendar.clear()
    val endWeekOf1stSeptember = firstOfSeptember(septemberCalendar, childBirthday, childBirthdayCalendar) // end date of first week of 1st september

    if (endWeekOf1stSeptember.before(childBirthday) || childBirthday.equals(endWeekOf1stSeptember)) { // end week is before today
      septemberCalendar.add(Calendar.YEAR, 1) // must be next year (september now+1)
    }

    getWeekEnd(septemberCalendar)
  }

  def getChildBirthday(periodStart: LocalDate, location: String, child: TFCChild): Date = {
    val taxYearConfig = tFCConfig.getConfig(periodStart, location)
    val ageIncrease   = if (child.isDisabled) taxYearConfig.childAgeLimitDisabled else taxYearConfig.childAgeLimit
    child.childsBirthdayDateForAge(ageIncrease)
  }

  def determineChildStartDateInTFCPeriod(
      child: TFCChild,
      periodFrom: LocalDate,
      periodUntil: LocalDate,
      location: String
  ): Option[LocalDate] = {
    val childDob: Date              = tFCConfig.config.toDate(child.dob)
    val childBirthdaySeptDate: Date = endWeek1stOfSeptemberDate(periodFrom, location, child)

    childDob match {
      case dob if dob.before(tFCConfig.config.toDate(periodFrom)) =>
        childBirthdaySeptDate match {
          case septDate if septDate.after(tFCConfig.config.toDate(periodFrom)) => Some(periodFrom)
          case _                                                               => None
        }
      case dob if dob.before(tFCConfig.config.toDate(periodUntil)) => Some(child.dob)
      case _                                                       => None
    }
  }

  def determineChildEndDateInTFCPeriod(
      child: TFCChild,
      periodFrom: LocalDate,
      periodUntil: LocalDate,
      location: String
  ): Option[LocalDate] = {
    val childDob: Date              = tFCConfig.config.toDate(child.dob)
    val childBirthdaySeptDate: Date = endWeek1stOfSeptemberDate(periodFrom, location, child)

    childDob match {
      case dob if dob.after(tFCConfig.config.toDate(periodUntil)) || dob.equals(tFCConfig.config.toDate(periodUntil)) =>
        None
      case _ =>
        childBirthdaySeptDate match {
          case septDate if septDate.after(tFCConfig.config.toDate(periodUntil)) => Some(periodUntil)
          case septDate if septDate.after(tFCConfig.config.toDate(periodFrom)) =>
            Some(tFCConfig.config.toLocalDate(septDate))
          case _ => None
        }
    }
  }

  def determinePeriodEligibility(
      outputClaimants: List[TFCOutputClaimant],
      outputChildren: List[TFCOutputChild]
  ): Boolean = {
    val childEligibility = outputChildren.exists(child => child.qualifying)

    val periodEligibility = outputClaimants match {
      case claimantList if claimantList.length == 1 =>
        claimantList.head.qualifying && childEligibility
      case claimantList if claimantList.length == 2 =>
        claimantList.head.qualifying && claimantList.tail.head.qualifying && childEligibility
      case _ => false
    }
    periodEligibility
  }

  def determineTFCPeriods(tfcEligibilityInput: TFCEligibilityInput): List[TFCPeriod] = {

    val currentCalendar = Calendar.getInstance()
    currentCalendar.clear()
    currentCalendar.setTime(tFCConfig.config.toDate(tfcEligibilityInput.from))

    val periods = for (i <- 1 to tfcEligibilityInput.numberOfPeriods) yield {
      val startDate = tFCConfig.config.toLocalDate(currentCalendar.getTime)
      currentCalendar.add(Calendar.MONTH, 3)
      val untilDate = tFCConfig.config.toLocalDate(currentCalendar.getTime)
      val outputClaimants =
        determineClaimantsEligibility(tfcEligibilityInput.claimants, startDate, tfcEligibilityInput.location)
      val location          = if (tfcEligibilityInput.claimants.isEmpty) "default" else tfcEligibilityInput.location
      val outputChildren    = determineChildrenEligibility(tfcEligibilityInput.children, startDate, untilDate, location)
      val periodEligibility = determinePeriodEligibility(outputClaimants, outputChildren)

      TFCPeriod(
        from = startDate,
        until = untilDate,
        periodEligibility = periodEligibility,
        claimants = outputClaimants,
        children = outputChildren
      )
    }
    periods.toList
  }

  def determineChildrenEligibility(
      children: List[TFCChild],
      periodFrom: LocalDate,
      periodUntil: LocalDate,
      location: String
  ): List[TFCOutputChild] =

    for (child <- children) yield {
      val qualifyStartDate = determineChildStartDateInTFCPeriod(child, periodFrom, periodUntil, location)
      val qualifyEndDate   = determineChildEndDateInTFCPeriod(child, periodFrom, periodUntil, location)
      val childEligibility = qualifyStartDate.isDefined && qualifyEndDate.isDefined
      TFCOutputChild(
        id = child.id,
        qualifying = childEligibility,
        from = qualifyStartDate,
        until = qualifyEndDate,
        childcareCost = child.childcareCost,
        childcareCostPeriod = child.childcareCostPeriod,
        disability = models.output.tfc.TFCDisability(child.disability.disabled, child.disability.severelyDisabled)
      )
    }

  def determineClaimantsEligibility(
      claimants: List[TFCClaimant],
      periodStart: LocalDate,
      location: String
  ): List[TFCOutputClaimant] =
    for (claimant <- claimants)
      yield TFCOutputClaimant(
        qualifying = isTotalIncomeLessThan100000(periodStart, location, claimant),
        isPartner = claimant.isPartner
      )

  def eligibility(request: TFCEligibilityInput)(implicit hc: HeaderCarrier): Future[TFCEligibilityOutput] = {
    val outputPeriods = determineTFCPeriods(request)
    val householdEligibility = outputPeriods.exists(period =>
      period.periodEligibility
    ) && validHouseholdMinimumEarnings(request) && request.validMaxEarnings()
    Future {
      TFCEligibilityOutput(
        from = request.from,
        until = outputPeriods.last.until,
        householdEligibility = householdEligibility,
        periods = outputPeriods
      )
    }
  }

  private def validHouseholdMinimumEarnings(
      tfcEligibilityInput: TFCEligibilityInput
  )(implicit hc: HeaderCarrier): Boolean = {
    val parent            = tfcEligibilityInput.claimants.head
    val minEarningsParent = parent.minimumEarnings.selection
    if (tfcEligibilityInput.claimants.length > 1) {
      val partner            = tfcEligibilityInput.claimants.last
      val minEarningsPartner = partner.minimumEarnings.selection
      val auditMinEarns      = minEarningsParent && minEarningsPartner

      if (!auditMinEarns) {
        auditEvent.auditMinEarnings(auditMinEarns)
      }

      (minEarningsParent, minEarningsPartner) match {
        case (true, true)  => true
        case (true, false) => partner.carersAllowance
        case (false, true) => parent.carersAllowance
        case _             => false
      }
    } else {

      if (!minEarningsParent) {
        auditEvent.auditMinEarnings(minEarningsParent)
      }
      minEarningsParent
    }
  }

  private def isTotalIncomeLessThan100000(periodStart: LocalDate, location: String, claimant: TFCClaimant): Boolean = {
    val taxYearConfig              = tFCConfig.getConfig(periodStart, location)
    val maximumTotalIncome: Double = taxYearConfig.maxIncomePerClaimant
    claimant.totalIncome <= maximumTotalIncome
  }

}
