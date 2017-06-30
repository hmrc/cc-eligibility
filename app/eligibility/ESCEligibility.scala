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
import models.output.esc.ESCEligibilityOutput
import org.joda.time.LocalDate
import utils.{ESCConfig, MessagesObject}
import scala.annotation.tailrec
import scala.concurrent.Future

object ESCEligibility extends ESCEligibility

trait ESCEligibility extends CCEligibilityHelpers with MessagesObject {

    import scala.concurrent.ExecutionContext.Implicits.global

    private def splitDatesForChildren(taxYear: ESCTaxYear): List[LocalDate] = {
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

  private def causesSplit(child: ESCChild, taxYear: ESCTaxYear): Boolean = {
    val isBeingBorn = child.isBeingBornInTaxYear(taxYear)
    val turns15 = child.isTurning15Before1September(taxYear.from, taxYear.until)
    val turns16 = child.isTurning16Before1September(taxYear.from, taxYear.until)

    val causesSplit = isBeingBorn._1 || (turns15._1 && !child.isDisabled) || (turns16._1 && child.isDisabled)
    causesSplit
  }

  private def splitDates(taxYear: ESCTaxYear, sortedSplitDates: List[LocalDate]): List[LocalDate] = {
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

  def generateSplitDates(taxYear: ESCTaxYear): List[LocalDate] = {
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
  }

  def determineStartDatesOfPeriodsInTaxYear(taxYear: ESCTaxYear): List[LocalDate] = {
    val filtered: List[LocalDate] = generateSplitDates(taxYear)
    val taxYearStart: LocalDate = taxYear.from
    val inserted: List[LocalDate] = filtered.::(taxYearStart)
    inserted.distinct
  }

  def hasQualifyingChildForPeriod(children: List[ESCChild], periodStart: LocalDate): Boolean = {
    val qualifyingChild = children.exists(child => child.qualifiesForESC(periodStart))
    qualifyingChild
  }

  def numberOfQualifyingMonthsForPeriod(qualifying: Boolean, periodStart: LocalDate, periodEnd: LocalDate): Int = {
    qualifying match {
      case true =>
        (periodEnd.getYear - periodStart.getYear) * 12 + (periodEnd.getMonthOfYear - periodStart.getMonthOfYear)
      case _ =>
        0
    }
  }

  def determineClaimantsEligibilityForPeriod(children: List[ESCChild], claimants: List[ESCClaimant], periodStart: LocalDate,
                                             periodEnd: LocalDate): List[models.output.esc.ESCClaimant] = {
    val outputClaimants = for (claimant <- claimants) yield {
      val hasQualifyingChildren = hasQualifyingChildForPeriod(children, periodStart)
      val claimantQualifying = claimant.isClaimantQualifyingForESC
      val vouchers = claimantQualifying && hasQualifyingChildren

      val months = numberOfQualifyingMonthsForPeriod(vouchers, periodStart, periodEnd)

      models.output.esc.ESCClaimant(
        qualifying = claimantQualifying,
        isPartner = claimant.isPartner,
        eligibleMonthsInPeriod = months,
        vouchers = vouchers
      )
    }
    outputClaimants
  }

  def determineChildrensEligibilityForPeriod(children: List[ESCChild], periodStart: LocalDate): List[models.output.esc.ESCChild] = {
    for (child <- children) yield {
      val eligible = child.qualifiesForESC(periodStart)
      models.output.esc.ESCChild(
        id = child.id,
        qualifying = eligible
      )
    }
  }

  def determinePeriodsForTaxYear(ty: ESCTaxYear): List[models.output.esc.ESCPeriod] = {
    val datesOfChanges = determineStartDatesOfPeriodsInTaxYear(ty)

    val periods = for ((date, i) <- datesOfChanges.zipWithIndex) yield {
      val fromAndUntil = fromAndUntilDateForPeriod(date, i, datesOfChanges, ty)

      // determine child's qualification and claimants qualification
      val children = determineChildrensEligibilityForPeriod(ty.children, fromAndUntil._1)
      val claimants = determineClaimantsEligibilityForPeriod(ty.children, ty.claimants, fromAndUntil._1, fromAndUntil._2)

      models.output.esc.ESCPeriod(
        from = fromAndUntil._1,
        until = fromAndUntil._2,
        claimants = claimants
      )
    }
    periods
  }

  def determineChildrenEligibility(ty: ESCTaxYear) = {
    val datesOfChanges = determineStartDatesOfPeriodsInTaxYear(ty)

    val children = for ((date, i) <- datesOfChanges.zipWithIndex) yield {
      val fromAndUntil = fromAndUntilDateForPeriod(date, i, datesOfChanges, ty)

      // determine child's qualification and claimants qualification
      determineChildrensEligibilityForPeriod(ty.children, fromAndUntil._1)
    }
    children.flatten
  }

  def constructTaxYearsWithPeriods(request: models.input.esc.ESCEligibilityInput): List[models.output.esc.ESCTaxYear] = {
    generateTaxYears(request.escTaxYears).reverse
  }

  def generateTaxYears(taxYears: List[models.input.esc.ESCTaxYear]): List[models.output.esc.ESCTaxYear] = {
    @tailrec
    def generateTaxYearsHelper(taxYears: List[models.input.esc.ESCTaxYear],
                               acc: List[models.output.esc.ESCTaxYear], i: Int): List[models.output.esc.ESCTaxYear] = {
      taxYears match {
        case Nil => acc
        case head :: tail =>
          val periods = determinePeriodsForTaxYear(head)
          val ty: models.output.esc.ESCTaxYear = models.output.esc.ESCTaxYear(from = head.from, until = head.until, periods = periods)

          generateTaxYearsHelper(tail, acc.::(ty), i + 1)
      }
    }

    generateTaxYearsHelper(taxYears, List(), 0)
  }

  def constructChildrenWithPeriods(request: models.input.esc.ESCEligibilityInput): List[models.output.esc.ESCChild] = {
    generateChildren(request.escTaxYears).reverse
  }

  def generateChildren(taxYears: List[models.input.esc.ESCTaxYear]): List[models.output.esc.ESCChild] = {
    @tailrec
    def generateChildrenHelper(taxYears: List[models.input.esc.ESCTaxYear],
                               acc: List[models.output.esc.ESCChild]): List[models.output.esc.ESCChild] = {
      taxYears match {
        case Nil => acc
        case head :: tail =>
          val periods = determineChildrenEligibility(head)
          generateChildrenHelper(tail, acc ::: (periods))
      }
    }

    generateChildrenHelper(taxYears, List())
  }

  def determineESCEligibility(taxYears: List[models.output.esc.ESCTaxYear], children: List[models.output.esc.ESCChild]): (Boolean, Boolean, Boolean) = {

    def getClaimantEligibility(isPartner: Boolean) = taxYears.exists(taxYear => taxYear.periods.exists(
      periods => periods.claimants.exists(
        claimant => if (claimant.isPartner == isPartner && claimant.qualifying) {
          true
        }
        else {
          false
        })))

    val escClaimantEligibilityResult: (Boolean, Boolean) = (getClaimantEligibility(isPartner = false),
      getClaimantEligibility(isPartner = true))

    val escChildrenEligibilityResult: Boolean = children.exists(_.qualifying)

    val eligibility = (escClaimantEligibilityResult._1 || escClaimantEligibilityResult._2) && escChildrenEligibilityResult
    val parentEligibility = escClaimantEligibilityResult._1
    val partnerEligibility = escClaimantEligibilityResult._2
    (eligibility, parentEligibility, partnerEligibility)
  }

  def eligibility(request: ESCEligibilityInput): Future[ESCEligibilityOutput] = {
    val constructTaxYears = constructTaxYearsWithPeriods(request)
    val constructChildren = constructChildrenWithPeriods(request)
    val (eligibility, parentEligibility, partnerEligibility) = determineESCEligibility(constructTaxYears, constructChildren)
    Future {
      ESCEligibilityOutput(
        constructTaxYears,
        eligibility,
        parentEligibility,
        partnerEligibility
      )
    }
  }
}
