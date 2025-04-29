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

package models.input

import java.util.{Calendar, Date}

import com.google.inject.Inject
import java.time.LocalDate
import utils.CCConfig

trait BaseTaxYear {
  def from: LocalDate
  def until: LocalDate
}

abstract class BaseChild @Inject() (ccConfig: Option[CCConfig]) {
  def id: Short
  def dob: LocalDate

  def isBeingBornInTaxYear(taxYear: BaseTaxYear): (Boolean, LocalDate) = {
    val dateOfBirth = ccConfig.get.toDate(dob)

    val requiresSplit =
      dateOfBirth.after(ccConfig.get.toDate(taxYear.from)) && dateOfBirth.before(ccConfig.get.toDate(taxYear.until))
    (requiresSplit, dob)
  }

  private def isSplittingPeriod1stSeptBeforeEndDate(claimDate: LocalDate, years: Int, september1: LocalDate) = {
    val previousSeptember1 = ccConfig.get.previousSeptember1stForDate(claimDate)
    val childBirthday      = childsBirthdayDateForAge(years)
    childBirthday match {
      // if child's 15/16(if disabled) birthday after 1st September of current tax year no split is required
      case birthday if birthday.after(ccConfig.get.toDate(september1)) =>
        false
      // if child's 15/16(if disabled) birthday is after the 1st September of previous tax year,
      // child is eligible until the 1st September of the current year
      case birthday
          if (birthday.after(ccConfig.get.toDate(previousSeptember1))
            || birthday.equals(ccConfig.get.toDate(previousSeptember1))) && ccConfig.get
            .toDate(claimDate)
            .before(ccConfig.get.toDate(september1)) =>
        true
      case _ =>
        false
    }
  }

  // TODO REMOVE THE BIRTHDAY IS AFTER PERIOD START DATE?
  def isSplittingPeriodOn1stSeptemberForYear(
      claimDate: LocalDate,
      endDate: LocalDate,
      years: Int
  ): (Boolean, LocalDate) = {
    val september1 = ccConfig.get.september1stForDate(claimDate)
    val requiresSplit = september1 match {
      // if child's 15/16(if disabled) birthday after end date of current tax year no split is required
      case date if ccConfig.get.toDate(date).after(ccConfig.get.toDate(endDate)) =>
        false
      case _ =>
        isSplittingPeriod1stSeptBeforeEndDate(claimDate, years, september1)
    }
    (requiresSplit, september1)
  }

  def childsBirthdayDateForAge(years: Int): Date = {
    val dobCalendar = Calendar.getInstance()
    dobCalendar.setTime(ccConfig.get.toDate(dob))
    dobCalendar.add(Calendar.YEAR, years)
    val childBirthday = dobCalendar.getTime
    childBirthday
  }

  def age(now: LocalDate): Int = {
    val dobCalendar: Calendar = Calendar.getInstance()
    dobCalendar.setTime(ccConfig.get.toDate(dob))

    val today = Calendar.getInstance()
    today.setTime(ccConfig.get.toDate(now))

    if (dobCalendar.after(today)) {
      -1
    } else {
      var age: Int = today.get(Calendar.YEAR) - dobCalendar.get(Calendar.YEAR)
      if (today.get(Calendar.MONTH) < dobCalendar.get(Calendar.MONTH)) {
        age -= 1
      } else if (
        today.get(Calendar.MONTH) == dobCalendar.get(Calendar.MONTH) && today.get(Calendar.DAY_OF_MONTH) < dobCalendar
          .get(Calendar.DAY_OF_MONTH)
      ) {
        age -= 1
      }
      age
    }
  }

}
