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

import config.ConfigConstants
import models.input.freeEntitlement.FreeEntitlementPayload
import models.input.tfc.{TFC, Request}
import models.output.freeEntitlement.{FifteenHoursEligibilityModel, ThirtyHoursEligibilityModel}
import org.joda.time.LocalDate
import play.api.Configuration
import uk.gov.hmrc.play.http.HeaderCarrier
import utils.{ChildHelper, CCConfig}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object FreeEntitlementService extends FreeEntitlementService

trait FreeEntitlementService extends CCConfig with ChildHelper with TFCEligibility {

  private def isChildDOBWithinRollout(dob: LocalDate): Boolean = {

    val futureDate = LocalDate.now().plusWeeks(2)
    val freeHoursRollout: Configuration = loadConfigByType("free-hours-rollout")
    val bornOnOrAfter = dateFormat.parse(freeHoursRollout.getString("born-on-after").get)

    dob.isBefore(futureDate) && !bornOnOrAfter.after(dob.toDate)
  }

  private def hasCildAtAge(configField: String, dobs: List[LocalDate], currentDate: LocalDate = LocalDate.now): Boolean = {
    val freeHours: Configuration = loadConfigByType("free-hours")
    val ageFilter: List[Int] = freeHours.getString(configField).getOrElse("").split(",").toList.filterNot(_.isEmpty).map(_.toInt)

    dobs.exists(dob => ageFilter.contains(age(dob, currentDate)))
  }

  def thirtyHours(tfcRequest: Request)(implicit req: play.api.mvc.Request[_], hc: HeaderCarrier): Future[ThirtyHoursEligibilityModel] = {

    eligibility.eligibility(tfcRequest).map { tfcEligibilityResult =>

      val tfcPayload: TFC = tfcRequest.payload.tfc
      val tfcEligibility: Boolean = tfcEligibilityResult.tfc.map(_.householdEligibility).getOrElse(false)

      val location = tfcPayload.claimants.head.location

      val hasChild3Or4Sept2017: Boolean = hasCildAtAge(
        configField = s"thirty.${location}",
        dobs = tfcRequest.payload.tfc.children.map(_.dob),
        currentDate = if(LocalDate.now.isBefore(ConfigConstants.firstSept2017)) { // TODO: Use only LocalDate.now after 01.09.2017
          ConfigConstants.firstSept2017
        }
        else {
          LocalDate.now
        }
      )

      val rollout = tfcPayload.children.exists( child =>
        isChildDOBWithinRollout(child.dob)
      )

      ThirtyHoursEligibilityModel(
        eligibility = tfcEligibility && hasChild3Or4Sept2017,
        rollout = rollout
      )
    }
  }

  def fifteenHours(request: FreeEntitlementPayload): Future[FifteenHoursEligibilityModel] = {
    val location = request.claimantLocation
    Future {
      FifteenHoursEligibilityModel(
        eligibility = hasCildAtAge(
          configField = s"fifteen.${location}",
          dobs = request.childDOBList,
          currentDate = LocalDate.now
        )
      )
    }
  }

}