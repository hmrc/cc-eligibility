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

import models.input.esc._
import models.output.OutputAPIModel.Eligibility
import models.output.esc.ESCEligibilityOutput
import org.joda.time.LocalDate
import utils.{ESCConfig, MessagesObject}

import scala.annotation.tailrec
import scala.concurrent.Future

object ESCEligibility extends ESCEligibility

trait ESCEligibility extends CCEligibilityHelpers with MessagesObject {

  import scala.concurrent.ExecutionContext.Implicits.global

  //TODO investigate if the end date should return 31st Aug instead of 1st Sept to get correct number of months
  private def splitDatesForChildren(taxYear: TaxYear): List[LocalDate] = {
    val dates: List[Option[LocalDate]] = for (child <- taxYear.children) yield {
      val isBeingBorn = child.isBeingBornInTaxYear(taxYear)
      val turns15 = child.isTurning15Before1September(taxYear.from, taxYear.until)
      val turns16 = child.isTurning16Before1September(taxYear.from, taxYear.until)

      if (isBeingBorn._1) {
        Some(isBeingBorn._2)
      } else if (turns15._1 && !child.isDisabled) {
        Some(turns15._2)
      } else if (turns16._1 && child.isDisabled) {
        Some(turns16._2)
      } else {
        None
      }
    }
    dates.flatten.distinct.sortBy(x => x.toDate.getTime)
  }

  private def splitDates(taxYear: TaxYear, sortedSplitDates: List[LocalDate]): List[LocalDate] = {
    val September1 = ESCConfig.september1stForDate(taxYear.from)
    sortedSplitDates.head match {
      case firstDateInList: LocalDate if firstDateInList.toDate.equals(September1.toDate) =>
        //                  return split date on 1st September and the date when child is being born
        val childBorn1stSept = taxYear.children.exists(ch => ch.isBeingBornOn1stSeptInTaxYear(taxYear))
        childBorn1stSept match {
          case true =>
            List(sortedSplitDates.head)
          case false =>
            sortedSplitDates.slice(0, 2)
        }
      case firstDateInList: LocalDate =>
        //return just the date when the child is being born either before or after 1st September.
        List(sortedSplitDates.head)
    }
  }

  def generateSplitDates(taxYear: TaxYear): List[LocalDate] = {

    def causesSplit(child: Child, taxYear : TaxYear) : Boolean = {
      val isBeingBorn = child.isBeingBornInTaxYear(taxYear)
      val turns15 = child.isTurning15Before1September(taxYear.from, taxYear.until)
      val turns16 = child.isTurning16Before1September(taxYear.from, taxYear.until)

      val causesSplit = isBeingBorn._1 || (turns15._1 && !child.isDisabled) || (turns16._1 && child.isDisabled)
      causesSplit
    }

    val numberOfChildrenEligibleInTaxYearWithNoSplits = taxYear.children.exists(ch => !causesSplit(ch, taxYear) && ch.qualifiesForESC(taxYear.from))

    val splitDateList = numberOfChildrenEligibleInTaxYearWithNoSplits match {
      case true =>
        List()
      case false =>
        //THERE ARE CHILDREN BEING BORN or CAUSE A SPLIT (TURNS 15 or 16)
        val sortedSplitDates: List[LocalDate] = splitDatesForChildren(taxYear)
        sortedSplitDates match {
          case date if date.length == 1 =>
            sortedSplitDates
          case date if date.length > 1 =>
            splitDates(taxYear, sortedSplitDates)
          case _ =>
            List()
        }
    }
    splitDateList

//    //THERE ARE CHILDREN BEING BORN or CAUSE A SPLIT (TURNS 15 or 16)
//    val sortedSplitDates: List[LocalDate] = splitDatesForChildren(taxYear)
//    sortedSplitDates match {
//      case date if date.length == 1 =>
//        sortedSplitDates
//      case date if date.length > 1 =>
//        splitDates(taxYear, sortedSplitDates)
//      case _ =>
//        List()
//    }
  }

  def determineStartDatesOfPeriodsInTaxYear(taxYear: TaxYear): List[LocalDate] = {
    val filtered: List[LocalDate] = generateSplitDates(taxYear)
    val taxYearStart: LocalDate = taxYear.from
    val inserted: List[LocalDate] = filtered.::(taxYearStart)
    inserted.distinct
  }

  def numberOfQualifyingMonthsForPeriod(qualifying: Boolean, periodStart: LocalDate, periodEnd: LocalDate): Int = {
    qualifying match {
      case true =>
        (periodEnd.getYear - periodStart.getYear) * 12 + (periodEnd.getMonthOfYear - periodStart.getMonthOfYear)
      case _ =>
        0
    }
  }

  def determineClaimantsEligibilityForPeriod(children: List[models.output.esc.OutputChild], claimants: List[Claimant], periodStart: LocalDate,
                                             periodEnd: LocalDate): List[models.output.esc.ESCOutputClaimant] = {
    for (claimant <- claimants) yield {
      val claimantQualifying = claimant.isClaimantQualifyingForESC
      val hasQualifyingChildren = children.exists(_.qualifying)
      val vouchers = claimantQualifying && hasQualifyingChildren

      val months = numberOfQualifyingMonthsForPeriod(vouchers, periodStart, periodEnd)

      models.output.esc.ESCOutputClaimant(
        qualifying = claimantQualifying,
        isPartner = claimant.isPartner,
        eligibleMonthsInPeriod = months,
        vouchers = vouchers
      )
    }
  }

  def determineChildrensEligibilityForPeriod(children: List[Child], periodStart: LocalDate): List[models.output.esc.OutputChild] = {
    for (child <- children) yield {
      models.output.esc.OutputChild(
        qualifying = child.qualifiesForESC(periodStart),
        childCareCost = child.childCareCost,
        childCareCostPeriod = child.childCareCostPeriod
      )
    }
  }

  def determinePeriodsForTaxYear(ty: TaxYear): List[models.output.esc.ESCPeriod] = {
    val datesOfChanges = determineStartDatesOfPeriodsInTaxYear(ty)

    for ((date, i) <- datesOfChanges.zipWithIndex) yield {
      val fromAndUntil = fromAndUntilDateForPeriod(date, i, datesOfChanges, ty)

      // determine child's qualification and claimants qualification
      val children = determineChildrensEligibilityForPeriod(ty.children, fromAndUntil._1)
      val claimants = determineClaimantsEligibilityForPeriod(children, ty.claimants, fromAndUntil._1, fromAndUntil._2)

      models.output.esc.ESCPeriod(
        from = fromAndUntil._1,
        until = fromAndUntil._2,
        claimants = claimants,
        children = children
      )
    }
  }

  def constructTaxYearsWithPeriods(taxYears: List[models.input.esc.TaxYear]): List[models.output.esc.TaxYear] = {

    @tailrec
    def generateTaxYearsHelper(taxYears: List[models.input.esc.TaxYear],
                               acc: List[models.output.esc.TaxYear]): List[models.output.esc.TaxYear] = {
      taxYears match {
        case Nil => acc.reverse
        case head :: tail =>
          val periods = determinePeriodsForTaxYear(head)
          val ty: models.output.esc.TaxYear = models.output.esc.TaxYear(
            from = head.from,
            until = head.until,
            periods = periods
          )
          generateTaxYearsHelper(tail, acc.::(ty))
      }
    }

    generateTaxYearsHelper(taxYears, List())
  }

  def determineESCEligibility(taxYears: List[models.output.esc.TaxYear]): (Boolean, Boolean, Boolean) = {

    def getClaimantEligibility(isPartner: Boolean) = taxYears.exists(
      _.periods.exists(
        _.claimants.exists(
          claimant => (claimant.isPartner == isPartner && claimant.qualifying)
        )
      )
    )

    val parentEligibility = getClaimantEligibility(isPartner = false)
    val partnerEligibility = getClaimantEligibility(isPartner = true)

    val escChildrenEligibilityResult: Boolean = taxYears.exists(
      _.periods.exists(
        _.children.exists(
          _.qualifying
        )
      )
    )

    val eligibility = (parentEligibility || partnerEligibility) && escChildrenEligibilityResult
    (eligibility, parentEligibility, partnerEligibility)
  }

  def eligibility(request: ESCEligibilityInput): Future[Eligibility] = {
    val constructTaxYears = constructTaxYearsWithPeriods(request.taxYears)
    val (eligibility, parentEligibility, partnerEligibility) = determineESCEligibility(constructTaxYears)
    Future {
      Eligibility(
        esc = Some(
          ESCEligibilityOutput(
            constructTaxYears,
            eligibility,
            parentEligibility,
            partnerEligibility
          )
        )
      )
    }
  }
}
