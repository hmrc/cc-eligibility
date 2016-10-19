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

package models.input

import java.util.{Calendar, Date}

import org.joda.time.LocalDate
import utils.{CCConfig}

trait BaseRequest {
  def payload : BasePayload
}

trait BasePayload {
  def taxYears : List[BaseTaxYear]
}

trait BaseTaxYear {
  def from : LocalDate
  def until : LocalDate
  def claimants : List[BaseClaimant]
  def children : List[BaseChild]
}

trait BaseClaimant {
  def liveOrWork : Boolean
  def isPartner : Boolean
}

trait BaseChild {
  def id : Short
  def name : Option[String]
  def dob : LocalDate

  def isBeingBornInTaxYear(taxYear: BaseTaxYear) : (Boolean, LocalDate) = {
    val dateOfBirth = dob.toDate

    val requiresSplit = dateOfBirth.after(taxYear.from.toDate) && dateOfBirth.before(taxYear.until.toDate)
    (requiresSplit, dob)
  }

  def isBeingBornOn1stSeptInTaxYear(taxYear: BaseTaxYear) : (Boolean) = {
    val dateOfBirth = dob.toDate
    val september1 = CCConfig.september1stForDate(taxYear.from)
    val childBorn1stSept = dateOfBirth.equals(september1.toDate)
    (childBorn1stSept)
  }

  // TODO REMOVE THE BIRTHDAY IS AFTER PERIOD START DATE?
  def isSplittingPeriodOn1stSeptemberForYear(claimDate: LocalDate, endDate : LocalDate, years: Int) : Tuple2[Boolean, LocalDate] = {
    val september1 = CCConfig.september1stForDate(claimDate)
    val previousSeptember1 = CCConfig.previousSeptember1stForDate(claimDate)

    val childBirthday = childsBirthdayDateForAge(years)

    val requiresSplit = september1 match {
      //if child's 15/16(if disabled) birthday after end date of current tax year no split is required
      case september1 if september1.toDate.after(endDate.toDate) =>
        false
      case _ =>
        childBirthday match {
          //if child's 15/16(if disabled) birthday after 1st September of current tax year no split is required
          case birthday if birthday.after(september1.toDate) =>
            false
          //if child's 15/16(if disabled) birthday is after the 1st September of previous tax year,
          // child is eligible until the 1st September of the current year
          case birthday if (birthday.after(previousSeptember1.toDate)
            || birthday.equals(previousSeptember1.toDate)) && claimDate.toDate.before(september1.toDate) =>
            true
          case _ =>
            false
        }
    }
    (requiresSplit, LocalDate.fromDateFields(september1.toDate))
  }



  def childsBirthdayDateForAge(years: Int) : Date = {
    val dobCalendar = Calendar.getInstance()
    dobCalendar.setTime(dob.toDate)
    dobCalendar.add(Calendar.YEAR, years)
    val childBirthday = dobCalendar.getTime
    childBirthday
  }

  def age(now : LocalDate) : Int = {
    val dobCalendar : Calendar = Calendar.getInstance()
    dobCalendar.setTime(dob.toDate)

    val today = Calendar.getInstance()
    today.setTime(now.toDate)

    if (dobCalendar.after(today)) {
      -1
    } else {
      var age: Int = today.get(Calendar.YEAR) - dobCalendar.get(Calendar.YEAR)
      if (today.get(Calendar.MONTH) < dobCalendar.get(Calendar.MONTH)) {
        age -= 1
      } else if (today.get(Calendar.MONTH) == dobCalendar.get(Calendar.MONTH) && today.get(Calendar.DAY_OF_MONTH) < dobCalendar.get(Calendar.DAY_OF_MONTH)) {
        age -= 1
      }
      age
    }
  }

}
