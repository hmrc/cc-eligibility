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

package utils

import java.text.SimpleDateFormat
import java.util.Calendar

import org.joda.time.LocalDate
import play.api.{Logger, Configuration, Play}
import play.api.Play._

/**
 * Created by adamconder on 03/08/15.
 */

trait CCConfig {

  def september1stForDate(date: LocalDate) : LocalDate = {
    val currentYear = determineTaxYearFromNow(date)

    val calendar = Calendar.getInstance()
    calendar.clear()
    calendar.set(Calendar.MONTH, Calendar.SEPTEMBER)
    calendar.set(Calendar.DAY_OF_MONTH, 1)
    calendar.set(Calendar.YEAR, currentYear)
    val september1 = calendar.getTime
    LocalDate.fromDateFields(september1)
  }

  def previousSeptember1stForDate(date: LocalDate) : LocalDate = {
    val currentYear = determineTaxYearFromNow(date)

    val calendar = Calendar.getInstance()
    calendar.clear()
    calendar.set(Calendar.MONTH, Calendar.SEPTEMBER)
    calendar.set(Calendar.DAY_OF_MONTH, 1)
    calendar.set(Calendar.YEAR, currentYear -1)
    val september1 = calendar.getTime
    LocalDate.fromDateFields(september1)
  }

  def september1stFollowingChildBirthday(childBirthday: LocalDate) : LocalDate = {
    // plot the child's birthday (e.g. 16th birthday) on the calendar
    val childBirthdayCalendar = Calendar.getInstance()
    childBirthdayCalendar.clear()
    childBirthdayCalendar.setTime(childBirthday.toDate)

    // determine 1st september for the child's birthday (current year)
    // if their birthday is after september then we have to go to the following year
    val septemberCalendar = Calendar.getInstance()
    septemberCalendar.clear()
    septemberCalendar.setTime(childBirthday.toDate)
    septemberCalendar.set(Calendar.MONTH, Calendar.SEPTEMBER)
    septemberCalendar.set(Calendar.DAY_OF_MONTH, 1)

    // if 16th birthday is after the determined 1st september then we need to add a year to the following september
    if (childBirthdayCalendar.compareTo(septemberCalendar) > 0 || childBirthdayCalendar.compareTo(septemberCalendar) == 0) {
      septemberCalendar.add(Calendar.YEAR, 1)
    }

    val september1 = septemberCalendar.getTime
    LocalDate.fromDateFields(september1)
  }

  def determineTaxYearFromNow(from: LocalDate) : Int = {
    val currentCalendar = Calendar.getInstance()
    currentCalendar.clear()
    currentCalendar.setTime(from.toDate)
    val periodYear = currentCalendar.get(Calendar.YEAR)
    val periodStart = from.toDate

    val januaryCalendar = Calendar.getInstance()
    januaryCalendar.clear()
    januaryCalendar.set(Calendar.YEAR, periodYear)
    januaryCalendar.set(Calendar.MONTH, Calendar.JANUARY)
    januaryCalendar.set(Calendar.DAY_OF_MONTH, 1)
    val january1st = januaryCalendar.getTime

    val aprilCalendar = Calendar.getInstance()
    aprilCalendar.clear()
    aprilCalendar.set(Calendar.YEAR, periodYear)
    aprilCalendar.set(Calendar.MONTH, Calendar.APRIL)
    aprilCalendar.set(Calendar.DAY_OF_MONTH, 5)
    val april5th = aprilCalendar.getTime

    val taxYear = if ((periodStart.compareTo(january1st) == 0 || periodStart.after(january1st)) && (periodStart.before(april5th) || periodStart.compareTo(april5th) == 0)) {
      periodYear-1
    } else {
      periodYear
    }
    taxYear
  }
}

object CCConfig extends CCConfig {

}
