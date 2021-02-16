/*
 * Copyright 2021 HM Revenue & Customs
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
import models.input.tc.{TCChild, TCEligibilityInput, TCIncome, TCTaxYear}
import models.output.tc.{TCChildElements, TCDisability, TCEligibilityOutput, TCOutputChild}
import org.joda.time.LocalDate
import utils.{CCConfig, TCConfig}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class TCEligibility @Inject()(tcConfig: TCConfig, ccConfig: CCConfig) extends CCEligibilityHelpers {

  private def determineStartDatesOfPeriodsInTaxYear(taxYearIn: models.input.tc.TCTaxYear): List[LocalDate] = {
    val taxYear = taxYearIn.createNewWithConfig(taxYearIn, tcConfig, ccConfig)
    val dates: List[Option[LocalDate]] = for (child <- taxYear.children) yield {
      val isBeingBorn = child.isBeingBornInTaxYear(taxYear)
      val turns15 = child.isTurning15Before1September(taxYear.from, taxYear.until)
      val turns16 = child.isTurning16Before1September(taxYear.from, taxYear.until)
      val turns20 = child.isTurning20InTaxYear(taxYear)

      if (isBeingBorn._1) {
        Some(isBeingBorn._2)
      } else if (turns15._1) {
        Some(turns15._2)
      } else if (turns16._1) {
        Some(turns16._2)
      } else if (turns20._1) {
        Some(turns20._2)
      } else {
        None
      }
    }

    val filtered = dates.flatten
    val inserted: List[LocalDate] = filtered.:::(List(taxYear.from))
    val sorted = inserted.sortBy(x => x.toDate.getTime)
    sorted.distinct
  }

  private def determinePeriodsForTaxYear(tyIn: models.input.tc.TCTaxYear): List[models.output.tc.TCPeriod] = {
    val ty = tyIn.createNewWithConfig(tyIn, tcConfig, ccConfig)
    // get all date ranges of splits for tax year
    val datesOfChanges = determineStartDatesOfPeriodsInTaxYear(ty)
    // multiple periods have been identified
    val periods = for ((date, i) <- datesOfChanges.zipWithIndex) yield {
      val fromAndUntil = fromAndUntilDateForPeriod(date, i, datesOfChanges, ty)
      val claimantsEligibility = determineClaimantsEligibilityForPeriod(ty)
      val childrenEligibility = determineChildrenEligibilityForPeriod(ty.children, periodStart = fromAndUntil._1)
      val householdEligibility = determineHouseholdEligibilityForPeriod(ty, periodStart = fromAndUntil._1)

      models.output.tc.TCPeriod(
        from = fromAndUntil._1,
        until = fromAndUntil._2,
        householdElements = householdEligibility,
        claimants = claimantsEligibility,
        children = childrenEligibility
      )
    }
    periods
  }

  private def constructTaxYearsWithPeriods(request: TCEligibilityInput): List[models.output.tc.TCTaxYear] = {
    for (ty <- request.taxYears) yield {
      models.output.tc.TCTaxYear(
        from = ty.from,
        until = ty.until,
        previousHouseholdIncome = ty.previousHouseholdIncome.getOrElse(TCIncome()),
        currentHouseholdIncome = ty.currentHouseholdIncome.getOrElse(TCIncome()),
        periods = determinePeriodsForTaxYear(ty)
      )
    }
  }

  def eligibility(request: TCEligibilityInput): Future[TCEligibilityOutput] = {

    val taxyears = constructTaxYearsWithPeriods(request)
    Future {
          TCEligibilityOutput(
            isEligibleForTC(taxyears),
            taxyears,
            determineWTCEligibility(taxyears),
            determineCTCEligibility(taxyears)
      )
    }
  }

  def determineHouseholdEligibilityForPeriod(ty: TCTaxYear, periodStart: LocalDate): models.output.tc.TCHouseHoldElements = {
    models.output.tc.TCHouseHoldElements(
      basic = ty.getBasicElement(periodStart),
      hours30 = ty.gets30HoursElement(periodStart),
      childcare = ty.householdGetsChildcareElement(periodStart),
      loneParent = ty.getsLoneParentElement(periodStart),
      secondParent = ty.gets2ndAdultElement(periodStart),
      family = ty.getsFamilyElement(periodStart),
      wtc = ty.isHouseholdQualifyingForWTC(periodStart),
      ctc = ty.isHouseholdQualifyingForCTC(periodStart)
    )
  }

  def determineClaimantsEligibilityForPeriod(ty: models.input.tc.TCTaxYear): List[models.output.tc.TCOutputClaimant] = {
    for (claimant <- ty.claimants) yield {
      val claimantIsPartner = claimant.isPartner
      val claimantIsDisabled = claimant.getDisabilityElement(ty.from)
      val claimantIsSeverelyDisabled = ty.isOneOfClaimantsWorking16h(ty.from) && claimant.disability.severelyDisabled

      val outputClaimant = models.output.tc.TCOutputClaimant(
        qualifying = true, //TODO - do we need this, verify in frontend and calculator
        isPartner = claimantIsPartner,
        claimantDisability = TCDisability(
          disability = claimantIsDisabled,
          severeDisability = claimantIsSeverelyDisabled
        ),claimant.incomeBenefits
      )
      outputClaimant
    }
  }

  def determineChildrenEligibilityForPeriod(children: List[TCChild], periodStart: LocalDate): List[TCOutputChild] = {

    def helper(children: List[TCChild], outputChildren: List[TCOutputChild], childrenWithChildElement: List[LocalDate]): List[TCOutputChild] = {
      if (children.isEmpty) {
        outputChildren
      }
      else {
        val child = children.head
        val isChild = child.isChild(periodStart)
        val getsChildElement: Boolean = (
          child.dob.isBefore(tcConfig.childDate6thApril2017) ||
            childrenWithChildElement.length < tcConfig.childElementLimit ||
            childrenWithChildElement.contains(child.dob)
          ) && isChild
        val modifiedChildrenWithChildElement = if (getsChildElement) {
          childrenWithChildElement :+ child.dob
        }
        else {
          childrenWithChildElement
        }
        val youngAdultElement = child.getsYoungAdultElement(periodStart)

        val outputChild = TCOutputChild(
          childcareCost = child.childcareCost,
          childcareCostPeriod = child.childcareCostPeriod,
          qualifying = isChild || youngAdultElement,
          childElements = TCChildElements(
            child = getsChildElement,
            youngAdult = youngAdultElement,
            disability = child.getsDisabilityElement(periodStart),
            severeDisability = child.getsSevereDisabilityElement(periodStart),
            childcare = child.getsChildcareElement(periodStart)
          )
        )
        helper(children.tail, outputChildren :+ outputChild, modifiedChildrenWithChildElement)
      }
    }

    helper(children.sortWith((child1, child2) => child1.dob.isBefore(child2.dob)), List.empty, List.empty)
  }


  def isEligibleForTC(listOfTaxYears: List[models.output.tc.TCTaxYear]): Boolean = {
    listOfTaxYears.exists(_.periods.exists(period => period.householdElements.wtc && period.householdElements.ctc))
  }

  def determineWTCEligibility(taxYears: List[models.output.tc.TCTaxYear]): Boolean =
    taxYears.exists(_.periods.exists(_.householdElements.wtc))

  def determineCTCEligibility(taxYears: List[models.output.tc.TCTaxYear]): Boolean =
    taxYears.exists(_.periods.exists(_.householdElements.ctc))

}
