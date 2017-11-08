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

import models.input.freeEntitlement.FreeEntitlementEligibilityInput
import models.input.tfc.TFCEligibilityInput
import models.output.freeEntitlement.{FifteenHoursEligibilityModel, ThirtyHoursEligibilityModel}
import org.joda.time.LocalDate
import play.api.Configuration
import uk.gov.hmrc.http.HeaderCarrier
import utils.{ChildHelper, CCConfig}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object FreeEntitlementEligibility extends FreeEntitlementEligibility{
  override val tfcEligibility: TFCEligibility = TFCEligibility
}

trait FreeEntitlementEligibility extends CCConfig with ChildHelper {

  val tfcEligibility: TFCEligibility
  def localDate = StartDate

  private def isChildDOBWithinRollout(dob: LocalDate): Boolean = {

    val futureDate = localDate.plusWeeks(2)
    val freeHoursRollout: Configuration = loadConfigByType("free-hours-rollout")
    val bornOnOrAfter = dateFormat.parse(freeHoursRollout.getString("born-on-after").get)

    dob.isBefore(futureDate) && !bornOnOrAfter.after(dob.toDate)
  }

  private def hasChildAtAge(configField: String, dobs: List[LocalDate], currentDate: LocalDate = localDate): Boolean = {
    val freeHours: Configuration = loadConfigByType("free-hours")
    val ageFilter: List[Int] = freeHours.getString(configField).getOrElse("").split(",").toList.filterNot(_.isEmpty).map(_.toInt)

    dobs.exists(dob => ageFilter.contains(age(dob, currentDate)))
  }

  def thirtyHours(tfcEligibilityInput: TFCEligibilityInput)(implicit req: play.api.mvc.Request[_], hc: HeaderCarrier): Future[ThirtyHoursEligibilityModel] = {

    tfcEligibility.eligibility(tfcEligibilityInput).map { tfcEligibilityResult =>

      val tfcEligibility: Boolean = tfcEligibilityResult.householdEligibility
      val location = tfcEligibilityInput.location

      val hasChild3Or4Years: Boolean = hasChildAtAge(
          configField = s"thirty.${location}",
          dobs = tfcEligibilityInput.children.map(_.dob),
          currentDate = localDate
        )

      val rollOut = tfcEligibilityInput.children.exists( child =>
        isChildDOBWithinRollout(child.dob)
      )

      ThirtyHoursEligibilityModel(
        eligibility = tfcEligibility && hasChild3Or4Years,
        rollout = rollOut
      )
    }
  }

  def fifteenHours(request: FreeEntitlementEligibilityInput): Future[FifteenHoursEligibilityModel] = {
    val location = request.claimantLocation

    Future {
      FifteenHoursEligibilityModel(
        eligibility = hasChildAtAge(
          configField = s"fifteen.${location}",
          dobs = request.childDOBList,
          currentDate = localDate
        )
      )
    }
  }

}
