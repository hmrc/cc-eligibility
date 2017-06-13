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

import models.input.freeEntitlement.FreeEntitlementPayload
import models.output.freeEntitlement.FreeEntitlementPageModel
import org.joda.time.LocalDate
import uk.gov.hmrc.play.test.UnitSpec

class FreeEntitlementServiceSpec extends UnitSpec {

  "FreeEntitlementService" should {
    "return eligibility" in {

      val res = await(FreeEntitlementService.eligibility(FreeEntitlementPayload("England",
        List(LocalDate.now()))))

      res shouldBe FreeEntitlementPageModel(region = "England")
    }

    "return eligibility for 2 3 4 year old child eligibility" in {

      val res = await(FreeEntitlementService.eligibility(FreeEntitlementPayload("England",
        List(LocalDate.now().minusYears(2), LocalDate.now().minusYears(3), LocalDate.now().minusYears(4)))))

      res shouldBe FreeEntitlementPageModel(twoYearOld = true,
        threeYearOld = true, fourYearOld = true, threeFourYearOldSep2017 = true, region = "England")
    }
  }
}
