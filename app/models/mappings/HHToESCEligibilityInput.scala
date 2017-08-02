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

import config.ApplicationConfig
import models._
import models.input.esc._
import org.joda.time.LocalDate

object HHToESCEligibilityInput {

  def convert(hh:Household):ESCEligibilityInput = {
    ESCEligibilityInput(escTaxYears = createTaxYears(hh))
  }

  private def createTaxYears(hh:Household):List[ESCTaxYear] = {
    val now = ApplicationConfig.startDate
    val april6thCurrentYear = determineApril6DateFromNow(now)
    val claimantList = hhClaimantToESCEligibilityInputClaimant(hh.parent, hh.partner)
    val childList = hhChildToESCEligibilityInputChild(hh.children)

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

  private def hhClaimantToESCEligibilityInputClaimant(hhParent: Claimant, hhPartner: Option[Claimant]): List[ESCClaimant] = {

    val parent: ESCClaimant = ESCClaimant(isPartner = false, employerProvidesESC = convertVouchers(hhParent))

    hhPartner match {
      case Some(hhPartner) => List(parent, ESCClaimant(isPartner = true, employerProvidesESC = convertVouchers(hhParent)))
      case None => List(parent)
    }
  }

  private def hhChildToESCEligibilityInputChild(hhChildren: List[Child]): List[ESCChild] = {
    hhChildren map (child => {
      ESCChild(
        id = child.id,
        dob = child.dob.get,
        childCareCost = child.childcareCost match {
          case Some(childcareCost) => childcareCost.amount.getOrElse(BigDecimal(0.00))
          case None => BigDecimal(0.00)
        }
      ,
        childCareCostPeriod = PeriodEnumToPeriod.convert(child.childcareCost match {
          case Some(childcareCost) => childcareCost.period.get
          case None => PeriodEnum.INVALID
        }),
        disability = ESCDisability(
                                  disabled = child.disability.get.disabled || child.disability.get.blind,
                                  severelyDisabled = child.disability.get.severelyDisabled)
        )
    })
  }


  private def determineApril6DateFromNow(from: LocalDate) : LocalDate = {
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

  private def convertVouchers(hhClaimant: Claimant): Boolean = {
    hhClaimant.escVouchers match {
      case YesNoUnsureBothEnum.BOTH => true
      case YesNoUnsureBothEnum.NO => false
      case YesNoUnsureBothEnum.NOTSURE => true
      case YesNoUnsureBothEnum.YES => true
      case _ => true
    }
  }
}
