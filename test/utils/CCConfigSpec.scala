/*
 * Copyright 2021 HM Revenue & Customs
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

package utils

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

import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import org.scalatestplus.play.PlaySpec
import play.api.{Application, Configuration}
import play.api.i18n.{Lang, MessagesApi}
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}

trait CCConfigSpec extends PlaySpec {

  val config: Map[String, _] = Map(
    "csrf.sign.tokens" -> false,
    "govuk-tax.Test.services.contact-frontend.host" -> "localhost",
    "govuk-tax.Test.services.contact-frontend.port" -> "9250",
    "microservice.services.cc-calculator.host" -> "localhost",
    "microservice.services.cc-calculator.port" -> "9372",
    "microservice.services.cc-calculator.url" -> "/cc-calculator/calculate",
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

  lazy val app: Application =
    new GuiceApplicationBuilder()
      .disable[com.kenshoo.play.metrics.PlayModule]
      .disable[com.kenshoo.play.metrics.MetricsController]
      .configure(Configuration("metrics.enabled" -> false))
      .configure(Configuration("metrics.jvm" -> false))
      .configure(config)
      .build()

  def bindModules: Seq[GuiceableModule] = Seq()


  def getMessages(s: String): String = {
    val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
    messagesApi(s)(Lang("en"))
  }

  val formatter: DateTimeFormatter = DateTimeFormat.forPattern("yyyy-MM-dd")
}