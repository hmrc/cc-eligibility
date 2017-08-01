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

package models.mappings

import java.util.{Calendar, Date}

import models.input.esc._
import org.joda.time.LocalDate
import utils.{CCConfig, Periods}

trait ESCMapping {

  val cCConfig: CCConfig

  def convert(household: Household): ESCEligibilityInput = {
    ESCEligibilityInput(createTaxYears(household.parent, household.partner, household.children))
  }

  private def createTaxYears(
                              parent: Claimant,
                              partner: Option[Claimant],
                              children: List[Child]
                            ): List[ESCTaxYear] = {

    val now = cCConfig.StartDate
    val april6thCurrentYear = determineApril6DateFromNow(now)
    val claimantList = createClaimants(parent, partner)
    val childList = createChildren(children)

    List(
      ESCTaxYear(
        from = now,
        until = april6thCurrentYear,
        claimants = claimantList,
        children = childList
      ),
      ESCTaxYear(
        from = april6thCurrentYear,
        until = now.plusYears(1),
        claimants = claimantList,
        children = childList
      )
    )
  }

  private def determineApril6DateFromNow(from: LocalDate): LocalDate = {
    val currentCalendar = Calendar.getInstance()
    currentCalendar.clear()
    currentCalendar.setTime(from.toDate)
    val periodYear = currentCalendar.get(Calendar.YEAR)
    val periodStart = from.toDate
    val january1st = getCalendarMonth(periodYear, Calendar.JANUARY, 1)
    val april6CurrentYear = getCalendarMonth(periodYear, Calendar.APRIL, 6)

    val aprilCalendarNextYear = Calendar.getInstance()
    aprilCalendarNextYear.clear()
    currentCalendar.setTime(april6CurrentYear)
    aprilCalendarNextYear.set(Calendar.YEAR, periodYear + 1)
    aprilCalendarNextYear.set(Calendar.MONTH, Calendar.APRIL)
    aprilCalendarNextYear.set(Calendar.DAY_OF_MONTH, 6)
    val april6NextYear = aprilCalendarNextYear.getTime

    if ((periodStart.compareTo(january1st) == 0 || periodStart.after(january1st)) && periodStart.before(april6CurrentYear)) {
      LocalDate.fromDateFields(april6CurrentYear)
    } else {
      LocalDate.fromDateFields(april6NextYear)
    }
  }

  private def getCalendarMonth(periodYear: Int, calendarMonth: Int, dayOfMonth: Int): Date = {
    val monthCalendar = Calendar.getInstance()
    monthCalendar.clear()
    monthCalendar.set(Calendar.YEAR, periodYear)
    monthCalendar.set(Calendar.MONTH, calendarMonth)
    monthCalendar.set(Calendar.DAY_OF_MONTH, dayOfMonth)
    monthCalendar.getTime
  }

  private def createClaimants(parent: Claimant,
                              partner: Option[Claimant]): List[ESCClaimant] = {

    val newParent = ESCClaimant(
      employerProvidesESC = escVouchersAvailable(parent)
    )

    if (partner.isDefined) {
      List(newParent, ESCClaimant(isPartner = true,
        employerProvidesESC = escVouchersAvailable(partner.get)))
    }
    else {
      List(newParent)
    }
  }

  private def escVouchersAvailable(claimant: Claimant): Boolean = {
    claimant.escVouchers match {
      case Some(YesNoUnsureBothEnum.YES) => true
      case Some(YesNoUnsureBothEnum.NOTSURE) => true
      case _ => false
    }
  }

  private def createChildren(children: List[Child]): List[ESCChild] = {
    for (child <- children) yield {
      ESCChild(
        id = child.id,
        dob = child.dob.get,
        childCareCost = child.childcareCost.flatMap(_.amount).getOrElse(BigDecimal(0)),
        childCareCostPeriod = periodEnumToPeriods(child.childcareCost.flatMap(_.period).getOrElse(PeriodEnum.MONTHLY)),
        disability = ESCDisability(
          disabled = child.disability.exists(d => d.blind || d.disabled),
          severelyDisabled = child.disability.exists(_.severelyDisabled)
        )
      )
    }
  }

  def periodEnumToPeriods(period: PeriodEnum.PeriodEnum): Periods.Period = {
    period match {
      case PeriodEnum.WEEKLY => Periods.Weekly
      case PeriodEnum.FORTNIGHTLY => Periods.Fortnightly
      case PeriodEnum.MONTHLY => Periods.Monthly
      case PeriodEnum.QUARTERLY => Periods.Quarterly
      case PeriodEnum.YEARLY => Periods.Yearly
      case PeriodEnum.INVALID => Periods.INVALID
      case PeriodEnum.DAILY => Periods.INVALID
    }
  }

}

object ESCMapping extends ESCMapping {
  override val cCConfig = CCConfig
}
