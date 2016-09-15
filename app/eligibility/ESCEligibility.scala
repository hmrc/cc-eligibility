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

import models.input.BaseRequest
import models.input.esc._
import models.output.OutputAPIModel.Eligibility
import models.output.esc.{ESCEligibilityModel, OutputClaimant}
import org.joda.time.LocalDate
import play.api.Logger
import play.api.i18n.Messages
import utils.{ESCConfig}

import scala.annotation.tailrec
import scala.concurrent.Future

/**
* Created by adamconder on 21/09/15.
*/
object ESCEligibility extends ESCEligibility

trait ESCEligibility extends CCEligibility {

  val eligibility = new ESCEligibilityService

  class ESCEligibilityService extends CCEligibilityService {

    import scala.concurrent.ExecutionContext.Implicits.global

    //TODO investigate if the end date should return 31st Aug instead of 1st Sept to get correct number of months
    private def splitDatesForChildren(taxYear : TaxYear) : List[LocalDate] = {
      Logger.debug(s"ESCEligibilityService.splitDatesForChildren - Begin")
      val dates : List[Option[LocalDate]] = for (child <- taxYear.children) yield {
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
      Logger.debug(s"ESCEligibilityService.splitDatesForChildren - End")
      dates.flatten.distinct.sortBy(x => x.toDate.getTime)
    }

   private def causesSplit(child: Child, taxYear : TaxYear) : Boolean = {
     Logger.debug(s"ESCEligibilityService.causesSplit - Begin")
      val isBeingBorn = child.isBeingBornInTaxYear(taxYear)
      val turns15 = child.isTurning15Before1September(taxYear.from, taxYear.until)
      val turns16 = child.isTurning16Before1September(taxYear.from, taxYear.until)

      val causesSplit = isBeingBorn._1 || (turns15._1 && !child.isDisabled) || (turns16._1 && child.isDisabled)
     Logger.debug(s"ESCEligibilityService.causesSplit - End")
      causesSplit
    }

    def generateSplitDates(taxYear : TaxYear) : List[LocalDate] = {
      Logger.debug(s"ESCEligibilityService.generateSplitDates - Begin")
      val numberOfChildrenEligibleInTaxYearWithNoSplits = taxYear.children.exists(ch => !causesSplit(ch, taxYear) && ch.qualifiesForESC(taxYear.from))

      val splitDateList = numberOfChildrenEligibleInTaxYearWithNoSplits match {
        case true =>
          List()
        case false =>
          //THERE ARE CHILDREN BEING BORN or CAUSE A SPLIT (TURNS 15 or 16)
          val September1 = ESCConfig.september1stForDate(taxYear.from)
          val sortedSplitDates: List[LocalDate] = splitDatesForChildren(taxYear)
          sortedSplitDates match {
            case date if date.length == 1 =>
              sortedSplitDates
            case date if date.length > 1 =>
              sortedSplitDates.head match {
                case firstDateInList : LocalDate if firstDateInList.toDate.equals(September1.toDate) =>
//                  return split date on 1st September and the date when child is being born
                  val childBorn1stSept = taxYear.children.exists(ch => ch.isBeingBornOn1stSeptInTaxYear(taxYear))
                  childBorn1stSept match {
                    case true =>
                      List(sortedSplitDates.head)
                    case false =>
                      sortedSplitDates.slice(0, 2)
                  }
                case firstDateInList : LocalDate =>
//                  return just the date when the child is being born either before or after 1st September.
                  List(sortedSplitDates.head)
              }
            case _ =>
              List()
          }
      }
      Logger.debug(s"ESCEligibilityService.generateSplitDates - End")
      splitDateList
    }

    def determineStartDatesOfPeriodsInTaxYear(taxYear: TaxYear) : List[LocalDate] = {
      Logger.debug(s"ESCEligibilityService.determineStartDatesOfPeriodsInTaxYear - Begin")
      val filtered: List[LocalDate] = generateSplitDates(taxYear)
      val taxYearStart : LocalDate = taxYear.from
      val inserted: List[LocalDate] = filtered.::(taxYearStart)
      Logger.debug(s"ESCEligibilityService.determineStartDatesOfPeriodsInTaxYear - End")
      inserted.distinct
    }

    def hasQualifyingChildForPeriod(children: List[Child], periodStart: LocalDate) : Boolean = {
      Logger.debug(s"ESCEligibilityService.hasQualifyingChildForPeriod - Begin")
      val qualifyingChild = children.exists(child => child.qualifiesForESC(periodStart))
      Logger.debug(s"ESCEligibilityService.hasQualifyingChildForPeriod - End")
      qualifyingChild
    }

    def numberOfQualifyingMonthsForPeriod(qualifying: Boolean, periodStart: LocalDate, periodEnd: LocalDate) : Int = {
      Logger.debug(s"ESCEligibilityService.numberOfQualifyingMonthsForPeriod - Begin")
      qualifying match {
        case true =>
          (periodEnd.getYear - periodStart.getYear) * 12 + (periodEnd.getMonthOfYear - periodStart.getMonthOfYear)
        case _ =>
          0

      }
    }

    def determineClaimantsEligibilityForPeriod(children: List[Child], claimants: List[Claimant], periodStart : LocalDate, periodEnd: LocalDate) : List[models.output.esc.OutputClaimant] = {
      Logger.debug(s"ESCEligibilityService.determineClaimantsEligibilityForPeriod - Begin")
      val outputClaimants = for (claimant <- claimants) yield {
        val hasQualifyingChildren = hasQualifyingChildForPeriod(children, periodStart)
        val claimantQualifying = claimant.isClaimantQualifyingForESC
        val vouchers = claimantQualifying && hasQualifyingChildren

        val months = numberOfQualifyingMonthsForPeriod(vouchers, periodStart, periodEnd)

         models.output.esc.OutputClaimant(
          qualifying = claimantQualifying,
          isPartner = claimant.isPartner,
          eligibleMonthsInPeriod = months,
          elements = models.output.esc.ClaimantElements(
            vouchers = vouchers
          ),
          // TODO Implement failures
          failures = List()
        )
      }
      Logger.debug(s"ESCEligibilityService.determineClaimantsEligibilityForPeriod - End")
      outputClaimants
    }

    def determineChildrensEligibilityForPeriod(children: List[Child], periodStart: LocalDate) : List[models.output.esc.OutputChild] = {
      Logger.debug(s"ESCEligibilityService.determineChildrensEligibilityForPeriod - Begin")
      val outputChildren = for(child <- children) yield {
        val eligible = child.qualifiesForESC(periodStart)
        models.output.esc.OutputChild(
          id = child.id,
          name = child.name,
          qualifying = eligible,
          //TODO Implement failures
          failures = List()
        )
      }
      Logger.debug(s"ESCEligibilityService.determineChildrensEligibilityForPeriod - End")
      outputChildren
    }

    def determinePeriodsForTaxYear(ty: TaxYear) : List[models.output.esc.ESCPeriod] = {
      Logger.debug(s"ESCEligibilityService.determinePeriodsForTaxYear - Begin")
      val datesOfChanges = determineStartDatesOfPeriodsInTaxYear(ty)

      val periods = for((date, i) <- datesOfChanges.zipWithIndex) yield {
        val fromAndUntil = fromAndUntilDateForPeriod(date, i, datesOfChanges, ty)

        // determine child's qualification and claimants qualification
        val children = determineChildrensEligibilityForPeriod(ty.children, fromAndUntil._1)
        val claimants = determineClaimantsEligibilityForPeriod(ty.children, ty.claimants, fromAndUntil._1, fromAndUntil._2)

        models.output.esc.ESCPeriod(
          from = fromAndUntil._1,
          until = fromAndUntil._2,
          claimants = claimants,
          children = children
        )
      }
      Logger.debug(s"ESCEligibilityService.determinePeriodsForTaxYear - End")
      periods
    }

    def constructTaxYearsWithPeriods(request: models.input.esc.Request) : List[models.output.esc.TaxYear] = {
      generateTaxYears(request.payload.taxYears).reverse
    }

    def generateTaxYears(taxYears : List[models.input.esc.TaxYear]) : List[models.output.esc.TaxYear] = {
      Logger.debug(s"ESCEligibilityService.generateTaxYears - Begin")
      @tailrec
      def generateTaxYearsHelper(taxYears : List[models.input.esc.TaxYear], acc : List[models.output.esc.TaxYear], i : Int) : List[models.output.esc.TaxYear] = {
        Logger.debug(s"ESCEligibilityService.generateTaxYearsHelper")
        taxYears match {
          case Nil => acc
          case head :: tail =>
            val periods = determinePeriodsForTaxYear(head)
            //val needToReduce = checkIfEligibleMonthsNeedToReduceForSecondTaxYear(i, acc, periods)

            val ty: models.output.esc.TaxYear = models.output.esc.TaxYear(from = head.from, until = head.until, periods = periods)

            generateTaxYearsHelper(tail, acc.::(ty), i + 1)
        }
      }
      Logger.debug(s"ESCEligibilityService.generateTaxYears - End")
      generateTaxYearsHelper(taxYears, List(), 0)
    }

    override def eligibility(request : BaseRequest) : Future[Eligibility] = {
      Logger.debug(s"ESCEligibilityService.eligibility")
      request match {
        case request : models.input.esc.Request =>
          Future {
            Eligibility(
              esc = Some(
                ESCEligibilityModel(
                  taxYears = constructTaxYearsWithPeriods(request)
                )
              )
            )
          }
        case _ =>
          Logger.warn(s"ESCEligibilityService.eligibility - Exception :$request")
          throw new IllegalArgumentException(Messages("cc.elig.wrong.type"))
      }
    }

  }
}
