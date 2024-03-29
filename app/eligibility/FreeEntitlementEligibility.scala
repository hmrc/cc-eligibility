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

package eligibility

import javax.inject.Inject
import models.input.freeEntitlement.FreeEntitlementEligibilityInput
import models.input.tfc.TFCEligibilityInput
import models.output.freeEntitlement.{FifteenHoursEligibilityModel, ThirtyHoursEligibilityModel}
import java.time.LocalDate
import play.api.Configuration
import uk.gov.hmrc.http.HeaderCarrier
import utils.{CCConfig, ChildHelper}

import scala.concurrent.{ExecutionContext, Future}


class FreeEntitlementEligibility @Inject()(tfcEligibility: TFCEligibility,
                                           config: CCConfig)
                                          (implicit ec: ExecutionContext) extends ChildHelper(config) {

  def localDate: LocalDate = config.startDate

  private def isChildDOBWithinRollout(dob: LocalDate): Boolean = {

    val futureDate = localDate.plusWeeks(2)
    val freeHoursRollout: Configuration = config.loadConfigByType("free-hours-rollout")
    val bornOnOrAfter = config.dateFormat.parse(freeHoursRollout.get[String]("born-on-after"))

    dob.isBefore(futureDate) && !bornOnOrAfter.after(config.toDate(dob))
  }

  private def hasChildAtAge(configField: String, dobs: List[LocalDate], currentDate: LocalDate): Boolean = {
    val freeHours: Configuration = config.loadConfigByType("free-hours")
    val ageFilter: List[Int] = freeHours.getOptional[String](configField).getOrElse("").split(",").toList.filterNot(_.isEmpty).map(_.toInt)

    dobs.exists(dob => ageFilter.contains(age(dob, currentDate)))
  }

  def thirtyHours(tfcEligibilityInput: TFCEligibilityInput)(implicit hc: HeaderCarrier): Future[ThirtyHoursEligibilityModel] = {

    tfcEligibility.eligibility(tfcEligibilityInput).map { tfcEligibilityResult =>

      val tfcEligibility: Boolean = tfcEligibilityResult.householdEligibility
      val location = tfcEligibilityInput.location

      val hasChild3Or4Years: Boolean = hasChildAtAge(
          configField = s"thirty.$location",
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
          configField = s"fifteen.$location",
          dobs = request.childDOBList,
          currentDate = localDate
        )
      )
    }
  }

}
