/*
 * Copyright 2016 HM Revenue & Customs
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
import play.api.Logger
import play.api.i18n.Messages
import utils.{TCConfig}

import scala.concurrent.Future

/**
 * Created by adamconder on 24/07/15.
 */
object TCEligibility extends TCEligibility

trait TCEligibility extends CCEligibility {

  val eligibility = new TCEligibilityService

  class TCEligibilityService extends CCEligibilityService {

    import scala.concurrent.ExecutionContext.Implicits.global

    private def determineStartDatesOfPeriodsInTaxYear(taxYear: models.input.tc.TaxYear) : List[LocalDate] = {
      Logger.debug(s"TCEligibilityService.determineStartDatesOfPeriodsInTaxYear - Begin")
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
      Logger.debug(s"TCEligibilityService.determineStartDatesOfPeriodsInTaxYear - End")
      sorted.distinct
    }

    def determineHouseholdEligibilityForPeriod(ty: TaxYear, periodStart: LocalDate) : models.output.tc.HouseholdElements = {
      Logger.debug(s"TCEligibilityService.determineHouseholdEligibilityForPeriod - Begin")
      val householdElements = models.output.tc.HouseholdElements(
        basic = ty.getBasicElement(periodStart),
        hours30 = ty.gets30HoursElement(periodStart),
        childcare = ty.householdGetsChildcareElement(periodStart),
        loneParent = ty.getsLoneParentElement(periodStart),
        secondParent = ty.gets2ndAdultElement(periodStart),
        family = ty.getsFamilyElement(periodStart)
      )
      Logger.debug(s"TCEligibilityService.determineHouseholdEligibilityForPeriod - End")
      householdElements
    }

    def determineClaimantsEligibilityForPeriod(ty : models.input.tc.TaxYear) : List[models.output.tc.OutputClaimant] = {
      Logger.debug(s"TCEligibilityService.determineClaimantsEligibilityForPeriod - Begin")
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
        Logger.debug(s"TCEligibilityService.determineClaimantsEligibilityForPeriod - End")
        outputClaimant
      }
    }

    def determineChildrensEligibilityForPeriod(children: List[Child], periodStart: LocalDate) : List[models.output.tc.OutputChild] = {
      Logger.debug(s"TCEligibilityService.determineChildrensEligibilityForPeriod - Begin")
      for (child <- children) yield {
        val childElement = child.getsChildElement(periodStart)
        val youngAdultElement = child.getsYoungAdultElement(periodStart)
        val disabilityElement = child.getsDisabilityElement(periodStart)
        val severeDisabilityElement = child.getsSevereDisabilityElement(periodStart)
        val childcareElement = child.getsChildcareElement(periodStart)
        val childIsQualifying = childElement || youngAdultElement

       val outputChild =  models.output.tc.OutputChild(
          id = child.id,
          name = child.name,
          childcareCost = child.childcareCost,
          childcareCostPeriod = child.childcareCostPeriod,
          qualifying = childIsQualifying,
          childElements = ChildElements(
            child = childElement,
            youngAdult = youngAdultElement,
            disability = disabilityElement,
            severeDisability = severeDisabilityElement,
            childcare = childcareElement
          ),
          //TODO Implement failures
          failures = List()
        )
        Logger.debug(s"TCEligibilityService.determineChildrensEligibilityForPeriod - End")
        outputChild
      }
    }

   private def determinePeriodsForTaxYear(ty: models.input.tc.TaxYear) : List[models.output.tc.TCPeriod] = {
     Logger.debug(s"TCEligibilityService.determinePeriodsForTaxYear - Begin")
      // get all date ranges of splits for tax year
      val datesOfChanges = determineStartDatesOfPeriodsInTaxYear(ty)
      // multiple periods have been identified
      val periods = for ((date, i) <- datesOfChanges.zipWithIndex) yield {
          val fromAndUntil = fromAndUntilDateForPeriod(date, i, datesOfChanges, ty)

          val claimantsEligibility = determineClaimantsEligibilityForPeriod(ty)
          val childrensEligibility = determineChildrensEligibilityForPeriod(ty.children, periodStart = fromAndUntil._1)
          val householdEligibility = determineHouseholdEligibilityForPeriod(ty, periodStart = fromAndUntil._1)

          models.output.tc.TCPeriod(
            from = fromAndUntil._1,
            until = fromAndUntil._2,
            householdElements = householdEligibility,
            claimants = claimantsEligibility,
            children = childrensEligibility
          )
        }
     Logger.debug(s"TCEligibilityService.determinePeriodsForTaxYear - End")
        periods
    }

    private def constructTaxYearsWithPeriods(request : models.input.tc.Request) : List[models.output.tc.TaxYear] = {
      Logger.debug(s"TCEligibilityService.constructTaxYearsWithPeriods - Begin")
      val taxYears : List[models.input.tc.TaxYear] = request.payload.taxYears

      val constructedTaxYears : List[models.output.tc.TaxYear] = for (ty <- taxYears) yield {
        val incomeWithDisregard = calculateIncomeDisregard(ty.getTotalHouseholdIncome._1, ty.getTotalHouseholdIncome._2, ty.from)
          models.output.tc.TaxYear(
            from = ty.from,
            until = ty.until,
            houseHoldIncome = incomeWithDisregard,
            periods = determinePeriodsForTaxYear(ty)
          )
        }

      Logger.debug(s"TCEligibilityService.constructTaxYearsWithPeriods - End: \n $constructedTaxYears \n for request : \n $request")
      constructedTaxYears
    }

    def calculateIncomeDisregard(currentIncome : BigDecimal, previousTotalIncome : BigDecimal,periodStart : LocalDate) : BigDecimal = {
      Logger.debug(s"TCEligibilityService.calculateIncomeDisregard - Begin")
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
      Logger.debug(s"TCEligibilityService.calculateIncomeDisregard - End")
      incomeDisregard
    }

    //TODO maybe to check for this at the controller level before all element check?
    def isEligibleForTC(listOfTaxYears : List[models.input.tc.TaxYear]) : Boolean = {
      Logger.debug(s"TCEligibilityService.isEligibleForTC - Begin")
      //if the fulnction "exists" finds a TY where a household does not qualify, it will return TRUE
      //TRUE means that a non qualifying family exists, so we need to return FALSE as method is checking if isEligibleForTC
      val eligibleForTC = !listOfTaxYears.exists((ty: TaxYear) => !ty.isCoupleQualifyingForTC)
      Logger.debug(s"TCEligibilityService.isEligibleForTC - End")
      eligibleForTC
    }

    override def eligibility(request : models.input.BaseRequest) : Future[Eligibility] = {
      Logger.debug(s"TCEligibilityService.eligibility")
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
          Logger.warn(s"TCEligibilityService.eligibility - Exception :$request")
          throw new IllegalArgumentException(Messages("cc.elig.wrong.type"))
      }
    }

  }
}
