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

package controllers

import akka.stream.Materializer
import org.scalatest.Suite
import play.api.test.FakeApplication
import uk.gov.hmrc.play.test.WithFakeApplication

trait FakeCCEligibilityApplication extends WithFakeApplication {
  this: Suite =>

  val config: Map[String, _] = Map(
    "csrf.sign.tokens" -> false,
    "govuk-tax.Test.services.contact-frontend.host" -> "localhost",
    "govuk-tax.Test.services.contact-frontend.port" -> "9250",
    "tfc-rollout.0.rule-date" -> "default",
    "tfc-rollout.0.all-disabled" -> true,
    "tfc-rollout.0.born-on-after" -> "01-09-2013",
    "free-hours.0.rule-date" -> "default",
    "free-hours.0.fifteen.england" -> "2,3,4",
    "free-hours.0.fifteen.scotland" -> "2,3,4",
    "free-hours.0.fifteen.northern-ireland" -> "3",
    "free-hours.0.fifteen.wales" -> "2,3",
    "free-hours.0.thirty.england" -> "3,4"
  )

  override lazy val fakeApplication = FakeApplication(additionalConfiguration = config)
  implicit lazy val mat: Materializer = fakeApplication.materializer

}
