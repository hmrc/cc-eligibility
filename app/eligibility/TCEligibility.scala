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

import models.input.tc.{Child, TaxYear}
import models.output.OutputAPIModel.Eligibility
import models.output.tc.{ChildElements, ClaimantDisability, TCEligibilityModel}
import org.joda.time.LocalDate
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import play.api.Logger
import play.api.i18n.Messages
import play.api.libs.json.Format
import play.data.format.Formats.DateFormatter
import utils.TCConfig
import play.api.i18n.Messages.Implicits._
import play.api.Play.current
import scala.concurrent.Future
import models.output.tc.OutputChild

/**
 * Created by adamconder on 24/07/15.
 */
object TCEligibility extends TCEligibility

trait TCEligibility extends CCEligibility {

  val eligibility = new TCEligibilityService

  class TCEligibilityService extends CCEligibilityService {

    import scala.concurrent.ExecutionContext.Implicits.global

    private def determineStartDatesOfPeriodsInTaxYear(taxYear: models.input.tc.TaxYear) : List[LocalDate] = {
      val dates : List[Option[LocalDate]] = for (child <- taxYear.children) yield {
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

    def determineHouseholdEligibilityForPeriod(ty: TaxYear, periodStart: LocalDate) : models.output.tc.HouseholdElements = {
      val householdElements = models.output.tc.HouseholdElements(
        basic = ty.getBasicElement(periodStart),
        hours30 = ty.gets30HoursElement(periodStart),
        childcare = ty.householdGetsChildcareElement(periodStart),
        loneParent = ty.getsLoneParentElement(periodStart),
        secondParent = ty.gets2ndAdultElement(periodStart),
        family = ty.getsFamilyElement(periodStart)
      )
      householdElements
    }

    def determineClaimantsEligibilityForPeriod(ty : models.input.tc.TaxYear) : List[models.output.tc.OutputClaimant] = {
      for (claimant <- ty.claimants) yield {
        val claimantIsQualifying = claimant.isQualifyingForTC
        val claimantIsPartner = claimant.isPartner
        val claimantIsDisabled = claimant.getDisabilityElement(ty.from)
        val claimantIsSeverelyDisabled = ty.isOneOfClaimantsWorking16h(ty.from) && claimant.isClaimantQualifyingForSevereDisabilityElement

       val outputClaimant =  models.output.tc.OutputClaimant(
          qualifying = claimantIsQualifying,
          isPartner = claimantIsPartner,
          claimantDisability = ClaimantDisability(
            disability = claimantIsDisabled,
            severeDisability = claimantIsSeverelyDisabled
          ),
          //TODO Implement failures
          failures = List()
        )
        outputClaimant
      }
    }

    def determineChildrenEligibilityForPeriod(children: List[Child], periodStart: LocalDate): List[OutputChild] = {

      val childLimit = TCConfig.childElementLimit
      val dtf = DateTimeFormat.forPattern("dd-mm-yyyy")
      val childDate = dtf.parseLocalDate(TCConfig.childElementDateConstraint)

      def helper(children: List[Child], outputChildren: List[OutputChild], childrenWithChildElement: List[LocalDate]): List[OutputChild] = {
        if(children.isEmpty) {
          outputChildren
        }
        else {
          val child = children.head
          val isChild = child.isChild(periodStart)
          val getsChildElement: Boolean = (
              child.dob.isBefore(childDate) ||
                childrenWithChildElement.length < childLimit ||
                childrenWithChildElement.contains(child.dob)
              ) && isChild
          val modifiedChildrenWithChildElement = if(getsChildElement) {
            childrenWithChildElement :+ child.dob
          }
          else {
            childrenWithChildElement
          }
          val youngAdultElement = child.getsYoungAdultElement(periodStart)

          val outputChild = OutputChild(
            id = child.id,
            name = child.name,
            childcareCost = child.childcareCost,
            childcareCostPeriod = child.childcareCostPeriod,
            qualifying = isChild || youngAdultElement,
            childElements = ChildElements(
              child = getsChildElement,
              youngAdult = youngAdultElement,
              disability = child.getsDisabilityElement(periodStart),
              severeDisability = child.getsSevereDisabilityElement(periodStart),
              childcare = child.getsChildcareElement(periodStart)
            ),
            failures = List()
          )
          helper(children.tail, outputChildren :+ outputChild, modifiedChildrenWithChildElement)
        }
      }

      helper(children.sortWith((child1, child2) => child1.dob.isBefore(child2.dob)), List.empty, List.empty)
    }

   private def determinePeriodsForTaxYear(ty: models.input.tc.TaxYear) : List[models.output.tc.TCPeriod] = {
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

    private def constructTaxYearsWithPeriods(request : models.input.tc.Request) : List[models.output.tc.TaxYear] = {
      val taxYears : List[models.input.tc.TaxYear] = request.payload.taxYears

      val constructedTaxYears : List[models.output.tc.TaxYear] = for (ty <- taxYears) yield {
        val incomeWithDisregard = calculateIncomeDisregard(ty.totalIncome, ty.previousTotalIncome, ty.from)
          models.output.tc.TaxYear(
            from = ty.from,
            until = ty.until,
            houseHoldIncome = incomeWithDisregard,
            periods = determinePeriodsForTaxYear(ty)
          )
        }
      constructedTaxYears
    }

    def calculateIncomeDisregard(currentIncome : BigDecimal, previousTotalIncome : BigDecimal,periodStart : LocalDate) : BigDecimal = {
      val taxYearConfig = TCConfig.getConfig(periodStart)
      val incomeDisregard = if(currentIncome < previousTotalIncome){ // if current income falls
      val incomeDifferenceComparison: BigDecimal = taxYearConfig.currentIncomeFallDifferenceAmount
        val incomeDifference: BigDecimal = previousTotalIncome - currentIncome

        if (incomeDifference > incomeDifferenceComparison){
          val disregard: BigDecimal = incomeDifference - incomeDifferenceComparison
          previousTotalIncome - disregard
        } else {
          previousTotalIncome
        }
      } else if (currentIncome > previousTotalIncome){ // if current income rises
      val incomeDifferenceComparison: BigDecimal = taxYearConfig.currentIncomeRiseDifferenceAmount
        val incomeDifference: BigDecimal = currentIncome - previousTotalIncome

        if (incomeDifference > incomeDifferenceComparison){
          val disregard: BigDecimal = incomeDifference - incomeDifferenceComparison
          previousTotalIncome + disregard
        } else {
          previousTotalIncome
        }
      } else { // if both incomes are equal
        currentIncome
      }
      incomeDisregard
    }

    //TODO maybe to check for this at the controller level before all element check?
    def isEligibleForTC(listOfTaxYears : List[models.input.tc.TaxYear]) : Boolean = {
      //if the function "exists" finds a TY where a household does not qualify, it will return TRUE
      //TRUE means that a non qualifying family exists, so we need to return FALSE as method is checking if isEligibleForTC
      val eligibleForTC = !listOfTaxYears.exists((ty: TaxYear) => !ty.isCoupleQualifyingForTC)
      eligibleForTC
    }

    override def eligibility(request : models.input.BaseRequest) : Future[Eligibility] = {
      request match {
        case request : models.input.tc.Request =>
          Future {
            Eligibility(
              tc = Some(
                TCEligibilityModel(
                  eligible = isEligibleForTC(request.payload.taxYears),
                  taxYears = constructTaxYearsWithPeriods(request)
                )
              )
            )
          }
        case _ =>
          Logger.warn(s"TCEligibilityService.eligibility - Exception ******")
          throw new IllegalArgumentException(Messages("cc.elig.wrong.type"))
      }
    }

  }
}
