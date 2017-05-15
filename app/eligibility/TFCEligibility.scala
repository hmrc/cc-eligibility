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

import models.input.tfc.{Child, Claimant, TFC}
import models.output.OutputAPIModel.Eligibility
import models.output.tfc._
import org.joda.time.LocalDate
import utils.TFCConfig

import scala.concurrent.Future

object TFCEligibility extends TFCEligibility

trait TFCEligibility extends CCTFCEligibility {

val eligibility = new TFCEligibilityService

  class TFCEligibilityService extends CCTFCEligibilityService {

    import scala.concurrent.ExecutionContext.Implicits.global

    def determineChildStartDateInTFCPeriod(child: models.input.tfc.Child, periodFrom : LocalDate, periodUntil: LocalDate) : LocalDate = {
      val childDob : java.util.Date = child.dob.toDate
      val childBirthdaySeptDate : java.util.Date = child.endWeek1stOfSeptemberDate(periodFrom)

      val dob = childDob match {
        case dob if(dob.before(periodFrom.toDate)) =>
          childBirthdaySeptDate match {
            case septDate if septDate.after(periodFrom.toDate) =>
              periodFrom
            case _ => null
          }
        case dob if(dob.before(periodUntil.toDate)) =>
          child.dob
        case _ => null
      }
      dob
    }

    def determineChildEndDateInTFCPeriod(child: models.input.tfc.Child, periodFrom : LocalDate, periodUntil: LocalDate) : LocalDate = {
      val childDob : java.util.Date = child.dob.toDate
      val childBirthdaySeptDate : java.util.Date = child.endWeek1stOfSeptemberDate(periodFrom)

      val dob = childDob match {
        case dob if(dob.after(periodUntil.toDate) || dob.equals(periodUntil.toDate)) =>
          null
        case _ =>
          childBirthdaySeptDate match {
            case septDate if septDate.after(periodUntil.toDate) =>
              periodUntil
            case septDate if septDate.after(periodFrom.toDate) =>
              LocalDate.fromDateFields(septDate)
            case _ =>
              null
          }
      }
      dob
    }

    def determinePeriodEligibility(outputClaimants : List[OutputClaimant],outputChildren : List[OutputChild]) : Boolean = {
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

    def determineTFCPeriods(tfc: TFC) : List[TFCPeriod] = {
      val currentCalendar = Calendar.getInstance()
      currentCalendar.clear()
      currentCalendar.setTime(tfc.from.toDate)

      val periods = for(i <- 1 to tfc.numberOfPeriods) yield {
        val startDate = LocalDate.fromDateFields(currentCalendar.getTime)
        currentCalendar.add(Calendar.MONTH, 3)
        val untilDate = LocalDate.fromDateFields(currentCalendar.getTime)
        val outputClaimants = determineClaimantsEligibility(tfc.claimants, startDate)
        val outputChildren = determineChildrenEligibility(tfc.children, startDate, untilDate)
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

    def determineChildrenEligibility(children: List[Child], periodFrom: LocalDate, periodUntil: LocalDate) : List[OutputChild] = {
      for(child <- children) yield {
        val qualifyStartDate = determineChildStartDateInTFCPeriod(child, periodFrom, periodUntil)
        val qualifyEndDate = determineChildEndDateInTFCPeriod(child, periodFrom, periodUntil)
        val childEligibility = !(qualifyStartDate == null) && !(qualifyEndDate == null)

        OutputChild(
          id = child.id,
          name = child.name,
          qualifying = childEligibility,
          from = qualifyStartDate,
          until = qualifyEndDate,
          //TODO - Populate children's failure list
          failures = List()
        )
      }
    }

    def determineClaimantsEligibility(claimants: List[Claimant], periodStart : LocalDate) : List[OutputClaimant] = {
      for(claimant <- claimants) yield {
        OutputClaimant(
          qualifying = claimant.isQualifyingForTFC(periodStart),
          isPartner = claimant.isPartner,
          //TODO - Populate claimant's failure list
          failures = List()
        )
      }
    }

    override def eligibility(request : models.input.tfc.Request) : Future[Eligibility] = {
      val outputPeriods = determineTFCPeriods(request.payload.tfc)
      val householdEligibility = if(TFCConfig.minimumEarningsEnabled) {
        outputPeriods.exists(period => period.periodEligibility) && request.payload.tfc.validHouseholdMinimumEarnings
      } else {
        outputPeriods.exists(period => period.periodEligibility) && request.payload.tfc.validHouseholdHours
      }

      Future {
        Eligibility(
          tfc = Some(
            TFCEligibilityModel(
              from = request.payload.tfc.from,
              until = outputPeriods.last.until,
              householdEligibility = householdEligibility,
              periods = outputPeriods
            )
          )
        )
      }
    }

  }
}
