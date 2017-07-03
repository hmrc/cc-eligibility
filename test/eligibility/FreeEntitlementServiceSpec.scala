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

import controllers.FakeCCEligibilityApplication
import models.input.freeEntitlement.FreeEntitlementPayload
import models.input.tfc._

import models.output.tfc.TFCEligibilityOutput
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import play.api.test.FakeRequest
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table
import utils.Periods

import scala.concurrent.Future

class FreeEntitlementServiceSpec extends UnitSpec with FakeCCEligibilityApplication with MockitoSugar {

  val now = LocalDate.now

  // TODO: Replace this with LocalDate.now after 01.09.2017
  val firstSept2017 = LocalDate.parse("2017-09-01", DateTimeFormat.forPattern("yyyy-MM-dd"))

  "determine eligibility corectly for fifteenHours" when {

    val testCases = Table(
      ("Location", "Dates of Birth", "Result"),
      ("england", List(now.minusYears(1)), false),
      ("england", List(now.minusYears(2).plusDays(1)), false),
      ("england", List(now.minusYears(2)), true),
      ("england", List(now.minusYears(3)), true),
      ("england", List(now.minusYears(4)), true),
      ("england", List(now.minusYears(5).plusDays(1)), true),
      ("england", List(now.minusYears(5)), false),
      ("scotland", List(now.minusYears(1)), false),
      ("scotland", List(now.minusYears(2).plusDays(1)), false),
      ("scotland", List(now.minusYears(2)), true),
      ("scotland", List(now.minusYears(3)), true),
      ("scotland", List(now.minusYears(4)), true),
      ("scotland", List(now.minusYears(5).plusDays(1)), true),
      ("scotland", List(now.minusYears(5)), false),
      ("northern-ireland", List(now.minusYears(2)), false),
      ("northern-ireland", List(now.minusYears(3).plusDays(1)), false),
      ("northern-ireland", List(now.minusYears(3)), true),
      ("northern-ireland", List(now.minusYears(3).minusDays(1)), true),
      ("northern-ireland", List(now.minusYears(4)), false),
      ("wales", List(now.minusYears(1)), false),
      ("wales", List(now.minusYears(2).plusDays(1)), false),
      ("wales", List(now.minusYears(2)), true),
      ("wales", List(now.minusYears(3)), true),
      ("wales", List(now.minusYears(4).plusDays(1)), true),
      ("wales", List(now.minusYears(4)), false),
      ("england", List(now.minusYears(1), now.minusYears(5)), false),
      ("england", List(now.minusYears(2), now.minusYears(5)), true),
      ("england", List(now.minusYears(1), now.minusYears(2)), true),
      ("england", List(now.minusYears(2), now.minusYears(3)), true),
      ("invalid-location", List(now.minusYears(3)), false)
    )

    forAll(testCases) { case (location, dobs, isEligible) =>
      s"for ${location} and children dobs = ${dobs} eligibility should be ${isEligible}" in {
        val freeEntitlementService: FreeEntitlementService = new FreeEntitlementService {
          override val tfcEligibility: TFCEligibility = mock[TFCEligibility]
        }

        val data = FreeEntitlementPayload(
          claimantLocation = location,
          childDOBList = dobs
        )
        val result = await(freeEntitlementService.fifteenHours(data))
        result.eligibility shouldBe isEligible
      }
    }

  }

  //Some of these tests will fail after 01/09/2017 but the logic needs fixing anyway

  "determine eligibility corectly for thirtyHours" when {
    val testCases = Table(
      ("Location", "TFC Eligibility", "Dates of Birth", "Eligibility Result", "Eligibility Rollout"),
      ("england", true, List(firstSept2017.minusYears(3).plusDays(1)), false, true),
      ("england", true, List(firstSept2017.minusYears(3)), true, true),
      ("england", true, List(firstSept2017.minusYears(4)), true, true),
      ("england", true, List(firstSept2017.minusYears(5).plusDays(1)), true, false),
      ("england", true, List(firstSept2017.minusYears(5)), false, false),
      ("scotland", true, List(firstSept2017.minusYears(3)), false, true),
      ("northern-ireland", true, List(firstSept2017.minusYears(3)), false, true),
      ("wales", true, List(firstSept2017.minusYears(3)), false, true),
      ("invalid-location", true, List(firstSept2017.minusYears(3)), false, true),
      ("england", false, List(firstSept2017.minusYears(3)), false, true)
    )

    forAll(testCases) { case (location, tfcEligibilityVal, dobs, isEligible, isRollout) =>
      s"location = ${location}, tfcEligibilty = ${tfcEligibilityVal}, dobs = ${dobs} then isEligible should be ${isEligible} and rollout should be ${isRollout}" in {
        implicit val request = FakeRequest()
        implicit val hc = new HeaderCarrier()

        val freeEntitlementService: FreeEntitlementService = new FreeEntitlementService {
          val tfcEligibility = mock[TFCEligibility]
        }

        when(
          freeEntitlementService.tfcEligibility.eligibility(any[models.input.tfc.TFCEligibilityInput])(any[play.api.mvc.Request[_]], any[HeaderCarrier])
        ).thenReturn(
          Future.successful(
                TFCEligibilityOutput(
                  from = now,
                  until = now.plusMonths(3),
                  householdEligibility = tfcEligibilityVal,
                  tfcRollout = false,
                  periods = List.empty
            )
          )
        )

        val tfcRequest = TFCEligibilityInput(
              from = now,
              numberOfPeriods = 3,
              location = location,
              claimants = List(
                TFCClaimant(
                  disability = TFCDisability(),
                  minimumEarnings = TFCMinimumEarnings(),
                  age = None
                )
              ),
              children = for(dob <- dobs) yield TFCChild(
                id = 0,
                childcareCostPeriod = Periods.Monthly,
                dob = dob,
                disability = TFCDisability()
              )
        )

        val result = await(freeEntitlementService.thirtyHours(tfcRequest))
        result.eligibility shouldBe isEligible
        result.rollout shouldBe isRollout
      }
    }
  }
}
