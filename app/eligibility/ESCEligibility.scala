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

import javax.inject.Inject
import models.input.esc._
import models.output
import models.output.esc
import models.output.esc.ESCEligibilityOutput
import org.joda.time.LocalDate
import utils.{CCConfig, ESCConfig}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ESCEligibility () extends CCEligibilityHelpers {

  def generateSplitDates(taxYear: ESCTaxYear): List[LocalDate] = {
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

  def determineStartDatesOfPeriodsInTaxYear(taxYear: ESCTaxYear): List[LocalDate] = {
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

  def determineClaimantsEligibilityForPeriod(children: List[output.esc.ESCChild], claimants: List[ESCClaimant], periodStart: LocalDate,
                                             periodEnd: LocalDate): List[models.output.esc.ESCClaimant] = {
    for (claimant <- claimants) yield {
      val claimantQualifying = claimant.isClaimantQualifyingForESC
      val hasQualifyingChildren = children.exists(_.qualifying)
      val vouchers = claimantQualifying && hasQualifyingChildren

      val months = numberOfQualifyingMonthsForPeriod(vouchers, periodStart, periodEnd)

      models.output.esc.ESCClaimant(
        qualifying = claimantQualifying,
        isPartner = claimant.isPartner,
        eligibleMonthsInPeriod = months,
        vouchers = vouchers,
        previousIncome = claimant.previousIncome.map(x => models.output.esc.ESCIncome(x.employmentIncome, x.pension, x.taxCode)),
        currentIncome = claimant.currentIncome.map(x => models.output.esc.ESCIncome(x.employmentIncome, x.pension, x.taxCode))
      )
    }
  }

  def determineChildrensEligibilityForPeriod(children: List[ESCChild], periodStart: LocalDate): List[models.output.esc.ESCChild] = {
    for (child <- children) yield {
      output.esc.ESCChild(
        qualifying = child.qualifiesForESC(periodStart),
        childCareCost = child.childCareCost,
        childCareCostPeriod = child.childCareCostPeriod
      )
    }
  }

  def determinePeriodsForTaxYear(ty: ESCTaxYear): List[models.output.esc.ESCPeriod] = {
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

  def constructTaxYearsWithPeriods(taxYears: List[models.input.esc.ESCTaxYear]): List[models.output.esc.ESCTaxYear] = {
    @tailrec
    def generateTaxYearsHelper(taxYears: List[models.input.esc.ESCTaxYear],
                               acc: List[models.output.esc.ESCTaxYear]): List[models.output.esc.ESCTaxYear] = {
      taxYears match {
        case Nil => acc.reverse
        case head :: tail =>
          val periods = determinePeriodsForTaxYear(head)
          val ty: models.output.esc.ESCTaxYear = models.output.esc.ESCTaxYear(
            from = head.from,
            until = head.until,
            periods = periods
          )
          generateTaxYearsHelper(tail, acc.::(ty))
      }
    }

    generateTaxYearsHelper(taxYears, List())
  }

  def determineESCEligibility(taxYears: List[models.output.esc.ESCTaxYear]): (Boolean, Boolean, Boolean) = {

    def getClaimantEligibility(isPartner: Boolean) = taxYears.exists(
      _.periods.exists(
        _.claimants.exists(
          claimant => claimant.isPartner == isPartner && claimant.qualifying
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

  def eligibility(request: ESCEligibilityInput, eSCConfig: ESCConfig, ccConfig: CCConfig): Future[ESCEligibilityOutput] = {
    if(request.escTaxYears.nonEmpty) {
      val childrenWithConfig = request.escTaxYears.head.children.map(x => x.createWithConfig(x, eSCConfig, ccConfig))
      val escTaxYearWithConfig = request.escTaxYears.map(x => new ESCTaxYear(x.from, x.until, x.claimants, childrenWithConfig))
      val escEligibilityInputWithConfig = ESCEligibilityInput(escTaxYearWithConfig, request.location)
      processEligibility(escEligibilityInputWithConfig)
    }else {
      processEligibility(request)
    }
  }

  private def processEligibility(request: ESCEligibilityInput) = {
    val constructTaxYears = constructTaxYearsWithPeriods(request.escTaxYears)
    val (eligibility, parentEligibility, partnerEligibility) = determineESCEligibility(constructTaxYears)
    Future {
      ESCEligibilityOutput(
        constructTaxYears,
        eligibility,
        parentEligibility,
        partnerEligibility,
        request.location
      )
    }
  }
}