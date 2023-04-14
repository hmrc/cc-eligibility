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

import java.text.SimpleDateFormat

import controllers.FakeCCEligibilityApplication
import models.input.freeEntitlement.FreeEntitlementEligibilityInput
import models.input.tfc._
import models.output.tfc.TFCEligibilityOutput
import org.joda.time.LocalDate
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table
import org.scalatestplus.mockito.MockitoSugar
import play.api.Configuration
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import utils.{CCConfig, Periods}

import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.Future

class FreeEntitlementEligibilitySpec extends AnyWordSpec with FakeCCEligibilityApplication with MockitoSugar {

  val now: LocalDate = LocalDate.now()
  val mockTFCE: TFCEligibility = mock[TFCEligibility]

  "determine eligibility correctly for fifteenHours" when {

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
      s"for $location and children dobs = $dobs eligibility should be $isEligible" in {
        val freeEntitlementService: FreeEntitlementEligibility = new FreeEntitlementEligibility (
          mock[TFCEligibility],
          app.injector.instanceOf[CCConfig]
        )

        val data = FreeEntitlementEligibilityInput(
          claimantLocation = location,
          childDOBList = dobs
        )
        val result = await(freeEntitlementService.fifteenHours(data))
        result.eligibility shouldBe isEligible
      }
    }

  }

  "determine eligibility correctly for thirtyHours" when {
    val testCases = Table(
      ("Location", "TFC Eligibility", "Dates of Birth", "Eligibility Result", "Eligibility Rollout"),
      ("england", true, List(now.minusYears(3).plusDays(1)), false, true),
      ("england", true, List(now.minusYears(3)), true, true),
      ("england", true, List(now.minusYears(4)), true, true),
      ("england", true, List(now.minusYears(5).plusDays(1)), true, true),
      ("england", true, List(now.minusYears(5)), false, true),
      ("scotland", true, List(now.minusYears(3)), false, true),
      ("northern-ireland", true, List(now.minusYears(3)), false, true),
      ("wales", true, List(now.minusYears(3)), false, true),
      ("invalid-location", true, List(now.minusYears(3)), false, true),
      ("england", false, List(now.minusYears(3)), false, true)
    )

    forAll(testCases) { case (location, tfcEligibilityVal, dobs, isEligible, isRollout) =>
      s"location = $location, tfcEligibilty = $tfcEligibilityVal, dobs = $dobs then isEligible should be $isEligible and rollout " +
        s"should be $isRollout" in {
        implicit val hc: HeaderCarrier = HeaderCarrier()
        val mockConfiguration: Configuration = mock[Configuration]

        val freeEntitlementService: FreeEntitlementEligibility = new FreeEntitlementEligibility(
          mockTFCE,
          new CCConfig(mock[ServicesConfig], mockConfiguration)
        ) {
          val freeHoursConfig: Seq[Configuration] = Seq(
            Configuration(
              "rule-date" -> "default",
              "thirty.england" -> "3,4"
            )
          )

          val freeHoursRolloutConfig: Seq[Configuration] = Seq(
            Configuration(
              "rule-date" -> "17-07-2017",
              "born-on-after" -> s"01-04-2013"
            ),
            Configuration(
              "rule-date" -> "default",
              "born-on-after" -> "01-09-2013"
            )
          )

          val configuration: Configuration = Configuration(
            "free-hours-rollout" -> freeHoursRolloutConfig.map(_.entrySet.toMap),
            "free-hours" -> freeHoursConfig.map(_.entrySet.toMap))

          when(mockConfiguration.underlying).thenReturn(configuration.underlying)
        }

        when(mockTFCE.eligibility(any[models.input.tfc.TFCEligibilityInput])(any[HeaderCarrier]))
          .thenReturn(Future.successful(
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
                  maximumEarnings = Some(false),
                  age = None
                )
              ),
              children = for(dob <- dobs) yield TFCChild(
                id = 0,
                childcareCostPeriod = Periods.Monthly,
                dob = dob,
                disability = TFCDisability()
              )(None)
        )

        val result = await(freeEntitlementService.thirtyHours(tfcRequest))
        result.eligibility shouldBe isEligible
        result.rollout shouldBe isRollout
      }
    }
  }

  "determine eligiblity correctly for thirtyHours when it's next year" in {
    implicit val hc: HeaderCarrier = HeaderCarrier()
    val mockConfig = mock[CCConfig]
    val freeEntitlementService: FreeEntitlementEligibility = new FreeEntitlementEligibility(mockTFCE, mockConfig) {
      override def localDate: LocalDate = now.plusYears(1)
    }

    when(mockConfig.loadConfigByType("free-hours")).thenReturn(Configuration(("thirty.england", "3,4")))
    when(mockConfig.loadConfigByType("free-hours-rollout")).thenReturn(Configuration(("born-on-after", "01-04-2013")))
    when(mockConfig.dateFormat).thenReturn(new SimpleDateFormat("dd-MM-yyyy"))
    when(mockTFCE.eligibility(any[models.input.tfc.TFCEligibilityInput])(any[HeaderCarrier]))
      .thenReturn( Future.successful(
        TFCEligibilityOutput(
          from = now,
          until = now.plusMonths(3),
          householdEligibility = true,
          tfcRollout = false,
          periods = List.empty
        )
      )
    )

    val tfcRequest = TFCEligibilityInput(
      from = now,
      numberOfPeriods = 3,
      location = "england",
      claimants = List(
        TFCClaimant(
          disability = TFCDisability(),
          minimumEarnings = TFCMinimumEarnings(),
          age = None
        )
      ),
      children = List(TFCChild(
        id = 0,
        childcareCostPeriod = Periods.Monthly,
        dob = now.minusYears(3),
        disability = TFCDisability()
      )(None)
    ))

    val result = await(freeEntitlementService.thirtyHours(tfcRequest))
    result.eligibility shouldBe true
  }
}
