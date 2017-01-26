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
import eligibility.TestEligibility.TestEligibilityService
import spec.CCSpecConfig
import scala.concurrent.Future

trait TestEligibility extends CCEligibility {
  val eligibility = new TestEligibilityService

  class TestEligibilityService extends CCEligibilityService {
    override def eligibility(request : models.input.BaseRequest) : Future[models.output.OutputAPIModel.Eligibility] = ???
  }
}

object TestEligibility extends TestEligibility

class CCEligibilitySpec  extends CCSpecConfig with FakeCCEligibilityApplication {

  "CCEligibility" should {

    "return an instance of CCEligibilityService" in {
      val eligible = TestEligibility
      eligible.eligibility shouldBe a [TestEligibilityService]
    }
  }

}
