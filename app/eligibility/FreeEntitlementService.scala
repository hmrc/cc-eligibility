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

import java.util.Calendar

import config.ConfigConstants
import models.input.freeEntitlement.FreeEntitlementPayload
import models.output.freeEntitlement.FreeEntitlementPageModel
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait FreeEntitlementService {

  def eligibility(request: FreeEntitlementPayload): Future[FreeEntitlementPageModel]

}

object FreeEntitlementService extends FreeEntitlementService {

  override def eligibility(request: FreeEntitlementPayload): Future[FreeEntitlementPageModel] = {

    def hasChildAtAge(ageFilter: Int): Boolean =
      request.childDOBList.exists(dob => age(dob) == ageFilter)

    val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
    val firstSept2017 = LocalDate.parse("2017-09-01", formatter)

    val threeFourYearOldSep2017 = request.childDOBList.exists(x =>
      age(x, currentDate = firstSept2017) == ConfigConstants.freeEntitlementThreeYearOld ||
        age(x, currentDate = firstSept2017) == ConfigConstants.freeEntitlementFourYearOld
    )

    val twoYearOld = hasChildAtAge(ConfigConstants.freeEntitlementTwoYearOld)

    val threeYearOld = hasChildAtAge(ConfigConstants.freeEntitlementThreeYearOld)

    val fourYearOld = hasChildAtAge(ConfigConstants.freeEntitlementFourYearOld)

    val location = request.claimantLocation

    val isFifteenHours: Boolean = {
      location match {
        case "england" => (twoYearOld || threeYearOld || fourYearOld || threeFourYearOldSep2017)
        case "scotland" => (twoYearOld || threeYearOld || fourYearOld)
        case "northern-ireland" => threeYearOld
        case "wales" => (twoYearOld || threeYearOld)
        case _ => false
      }
    }

    Future {
      FreeEntitlementPageModel(
        twoYearOld = twoYearOld,
        threeYearOld = threeYearOld,
        fourYearOld = fourYearOld,
        threeFourYearOldSep2017 = threeFourYearOldSep2017,
        region = location,
        tfcEligibility = getTFCEligibility,
        freeEntitlementRollout = getRollout,
        isFifteenHours = isFifteenHours,
        isThirtyHours = false
      )
    }
  }

  private def getTFCEligibility: Boolean = false

  private def getRollout: Boolean = false

  private def age(dob: LocalDate, currentDate: LocalDate = LocalDate.now()): Int = {
    val dobCalendar: Calendar = Calendar.getInstance()
    dobCalendar.setTime(dob.toDate)

    val today = Calendar.getInstance()
    today.setTime(currentDate.toDate)

    if (dobCalendar.after(today)) {
      -1
    } else {
      val age: Int = today.get(Calendar.YEAR) - dobCalendar.get(Calendar.YEAR)
      if (today.get(Calendar.MONTH) < dobCalendar.get(Calendar.MONTH)) {
        age - 1
      } else if (today.get(Calendar.MONTH) == dobCalendar.get(Calendar.MONTH) &&
        today.get(Calendar.DAY_OF_MONTH) < dobCalendar.get(Calendar.DAY_OF_MONTH)) {
        age - 1
      } else {
        age
      }
    }
  }
}
