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

package eligibility

import java.util.Calendar

import models.input.tfc.{TFCChild, TFCClaimant, TFCEligibilityInput}
import models.output.tfc._
import org.joda.time.LocalDate
import service.AuditEvents
import uk.gov.hmrc.play.http.HeaderCarrier
import utils.TFCRolloutSchemeConfig

import scala.concurrent.Future

object TFCEligibility extends TFCEligibility

trait TFCEligibility extends TFCRolloutSchemeConfig {

    import scala.concurrent.ExecutionContext.Implicits.global
    val auditEvents: AuditEvents = AuditEvents

    def determineChildStartDateInTFCPeriod(child: models.input.tfc.TFCChild, periodFrom : LocalDate, periodUntil: LocalDate, location: String): Option[LocalDate] = {
      val childDob : java.util.Date = child.dob.toDate
      val childBirthdaySeptDate : java.util.Date = child.endWeek1stOfSeptemberDate(periodFrom, location)

      childDob match {
        case dob if(dob.before(periodFrom.toDate)) =>
          childBirthdaySeptDate match {
            case septDate if septDate.after(periodFrom.toDate) => Some(periodFrom)
            case _ => None
          }
        case dob if(dob.before(periodUntil.toDate)) => Some(child.dob)
        case _ => None
      }
    }

    def determineChildEndDateInTFCPeriod(child: models.input.tfc.TFCChild, periodFrom : LocalDate, periodUntil: LocalDate, location: String): Option[LocalDate] = {
      val childDob : java.util.Date = child.dob.toDate
      val childBirthdaySeptDate : java.util.Date = child.endWeek1stOfSeptemberDate(periodFrom, location)

      childDob match {
        case dob if(dob.after(periodUntil.toDate) || dob.equals(periodUntil.toDate)) => None
        case _ =>
          childBirthdaySeptDate match {
            case septDate if septDate.after(periodUntil.toDate) => Some(periodUntil)
            case septDate if septDate.after(periodFrom.toDate) => Some(LocalDate.fromDateFields(septDate))
            case _ => None
          }
      }
    }

    def determinePeriodEligibility(outputClaimants : List[TFCOutputClaimant], outputChildren : List[TFCOutputChild]) : Boolean = {
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

    def determineTFCPeriods(tfcEligibilityInput: TFCEligibilityInput) : List[TFCPeriod] = {
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

    def determineChildrenEligibility(children: List[TFCChild], periodFrom: LocalDate, periodUntil: LocalDate, location: String) : List[TFCOutputChild] = {
      for(child <- children) yield {
        val qualifyStartDate = determineChildStartDateInTFCPeriod(child, periodFrom, periodUntil, location)
        val qualifyEndDate = determineChildEndDateInTFCPeriod(child, periodFrom, periodUntil, location)
        val childEligibility = qualifyStartDate.isDefined && qualifyEndDate.isDefined
        TFCOutputChild(
          id = child.id,
          qualifying = childEligibility,
          from = qualifyStartDate,
          until = qualifyEndDate,
          tfcRollout = isChildEligibleForTFCRollout(child, childEligibility)
        )
      }
    }

    def determineClaimantsEligibility(claimants: List[TFCClaimant], periodStart : LocalDate, location : String) : List[TFCOutputClaimant] = {
      for(claimant <- claimants) yield {
        TFCOutputClaimant(
          qualifying = claimant.isQualifyingForTFC(periodStart, location),
          isPartner = claimant.isPartner
        )
      }
    }

    def eligibility(request : TFCEligibilityInput)(implicit req: play.api.mvc.Request[_], hc: HeaderCarrier): Future[TFCEligibilityOutput] = {
      val outputPeriods = determineTFCPeriods(request)
      val householdEligibility = outputPeriods.exists(period => period.periodEligibility) && request.validHouseholdMinimumEarnings

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
}
