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

package eligibility



import java.util.{Calendar, Date}

import javax.inject.Inject
import models.input.tfc.{TFCChild, TFCClaimant, TFCEligibilityInput}
import models.output.tfc._
import org.joda.time.LocalDate
import play.api.Logger
import play.api.mvc.Request
import service.AuditEvents
import uk.gov.hmrc.http.HeaderCarrier
import utils.{TFCConfig, TFCRolloutSchemeConfig}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class TFCEligibility @Inject()(auditEvent: AuditEvents,
                               tfcRollOutConfig: TFCRolloutSchemeConfig,
                               tFCConfig: TFCConfig) {

  def determineChildStartDateInTFCPeriod(child: models.input.tfc.TFCChild, periodFrom: LocalDate, periodUntil: LocalDate, location: String):Option[LocalDate]={
    val childDob: Date = child.dob.toDate
    val childBirthdaySeptDate: Date = child.endWeek1stOfSeptemberDate(periodFrom, location)

    childDob match {
      case dob if dob.before(periodFrom.toDate) =>
        childBirthdaySeptDate match {
          case septDate if septDate.after(periodFrom.toDate) => Some(periodFrom)
          case _ => None
        }
      case dob if dob.before(periodUntil.toDate) => Some(child.dob)
      case _ => None
    }
  }

  def determineChildEndDateInTFCPeriod(child: models.input.tfc.TFCChild, periodFrom: LocalDate, periodUntil: LocalDate, location: String): Option[LocalDate]={
    val childDob: Date = child.dob.toDate
    val childBirthdaySeptDate: Date = child.endWeek1stOfSeptemberDate(periodFrom, location)

    childDob match {
      case dob if dob.after(periodUntil.toDate) || dob.equals(periodUntil.toDate) => None
      case _ =>
        childBirthdaySeptDate match {
          case septDate if septDate.after(periodUntil.toDate) => Some(periodUntil)
          case septDate if septDate.after(periodFrom.toDate) => Some(LocalDate.fromDateFields(septDate))
          case _ => None
        }
    }
  }

  def determinePeriodEligibility(outputClaimants: List[TFCOutputClaimant], outputChildren: List[TFCOutputChild]): Boolean = {
    val childEligibility = outputChildren.exists(child => child.qualifying)

    val periodEligibility =  outputClaimants match {
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
    currentCalendar.setTime(tfcEligibilityInput.from.toDate)

    val periods = for(i <- 1 to tfcEligibilityInput.numberOfPeriods) yield {
      val startDate = LocalDate.fromDateFields(currentCalendar.getTime)
      currentCalendar.add(Calendar.MONTH, 3)
      val untilDate = LocalDate.fromDateFields(currentCalendar.getTime)
      val outputClaimants = determineClaimantsEligibility(tfcEligibilityInput.claimants, startDate, tfcEligibilityInput.location)
      val location = if(tfcEligibilityInput.claimants.isEmpty) "default" else tfcEligibilityInput.location
      val outputChildren = determineChildrenEligibility(tfcEligibilityInput.children, startDate, untilDate, location)
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

  def determineChildrenEligibility(children: List[TFCChild], periodFrom: LocalDate, periodUntil: LocalDate, location: String): List[TFCOutputChild] = {

    for(child <- children) yield {
      val qualifyStartDate = determineChildStartDateInTFCPeriod(child, periodFrom, periodUntil, location)
      val qualifyEndDate = determineChildEndDateInTFCPeriod(child, periodFrom, periodUntil, location)
      val childEligibility = qualifyStartDate.isDefined && qualifyEndDate.isDefined
      TFCOutputChild(
        id = child.id,
        qualifying = childEligibility,
        from = qualifyStartDate,
        until = qualifyEndDate,
        tfcRollout = tfcRollOutConfig.isChildEligibleForTFCRollout(child, childEligibility),
        childcareCost = child.childcareCost,
        childcareCostPeriod = child.childcareCostPeriod,
        disability = models.output.tfc.TFCDisability(child.disability.disabled,child.disability.severelyDisabled)
      )
    }
  }

  def determineClaimantsEligibility(claimants: List[TFCClaimant], periodStart: LocalDate, location: String): List[TFCOutputClaimant] = {
    for(claimant <- claimants) yield {
      TFCOutputClaimant(
        qualifying = isTotalIncomeLessThan100000(periodStart, location, claimant),
        isPartner = claimant.isPartner
      )
    }
  }

  def eligibility(request: TFCEligibilityInput)(implicit req: play.api.mvc.Request[_], hc: HeaderCarrier): Future[TFCEligibilityOutput] = {
    val outputPeriods = determineTFCPeriods(request)
    val householdEligibility = outputPeriods.exists(period => period.periodEligibility) && validHouseholdMinimumEarnings(request) && request.validMaxEarnings
    Future {
      TFCEligibilityOutput(
        from = request.from,
        until = outputPeriods.last.until,
        householdEligibility = householdEligibility,
        periods = outputPeriods,
        tfcRollout = outputPeriods.exists(_.children.exists(_.tfcRollout))
      )
    }
  }

  def validHouseholdMinimumEarnings(tfceEligibilityInput: TFCEligibilityInput)(implicit req: play.api.mvc.Request[_], hc: HeaderCarrier): Boolean = {
    val parent = tfceEligibilityInput.claimants.head
    val minEarningsParent = satisfyMinimumEarnings(tfceEligibilityInput.from, tfceEligibilityInput.location, parent)
    if(tfceEligibilityInput.claimants.length > 1) {
      val partner = tfceEligibilityInput.claimants.last
      val minEarningsPartner = satisfyMinimumEarnings(tfceEligibilityInput.from, tfceEligibilityInput.location, partner)
      val auditMinEarns = minEarningsParent && minEarningsPartner

      if(!auditMinEarns) {
        tfceEligibilityInput.auditEvents.auditMinEarnings(auditMinEarns)
      }

      (minEarningsParent, minEarningsPartner) match {
        case (true, true) => true
        case (true, false) => partner.carersAllowance
        case (false, true) => parent.carersAllowance
        case _ => false
      }
    } else {

      if(!minEarningsParent) {
        tfceEligibilityInput.auditEvents.auditMinEarnings(minEarningsParent)
      }
      minEarningsParent
    }
  }


  def isTotalIncomeLessThan100000(periodStart: LocalDate, location: String, claimant: TFCClaimant): Boolean = {
    val taxYearConfig = tFCConfig.getConfig(periodStart, location)
    val maximumTotalIncome: Double = taxYearConfig.maxIncomePerClaimant
    claimant.totalIncome <= maximumTotalIncome
  }



  def satisfyMinimumEarnings(periodStart: LocalDate, location:String, claimant: TFCClaimant)
                            (implicit req: Request[_], hc: HeaderCarrier): Boolean = {

    val user = if(!claimant.isPartner) "Parent" else "Partner"

    val taxYearConfig = tFCConfig.getConfig(periodStart, location)
    if(claimant.minimumEarnings.selection) {
      true
    } else {
      val nmw = claimant.getNWMPerAge(taxYearConfig)
      if(claimant.minimumEarnings.amount >= nmw._1) {
        true
      } else {
        claimant.auditEvents.auditAgeGroup(user, nmw._2)
        claimant.employmentStatus match {
          case Some("selfEmployed") =>
            claimant.auditEvents.auditSelfEmploymentStatus(user, claimant.employmentStatus.get)
            claimant.auditEvents.auditSelfEmployedin1st(user, claimant.selfEmployedSelection.getOrElse(false))
            claimant.selfEmployedSelection.getOrElse(false)
          case Some("apprentice") => claimant.minimumEarnings.amount >= taxYearConfig.nmwApprentice
          case _ => false
        }
      }
    }
  }
}
