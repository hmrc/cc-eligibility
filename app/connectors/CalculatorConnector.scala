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

package connectors

import config.ApplicationConfig

import javax.inject.Inject
import models.input.CalculatorOutput
import models.output.CalculatorInput
import play.api.libs.json.Json
import uk.gov.hmrc.http.{HeaderCarrier, StringContextOps}
import uk.gov.hmrc.http.client.HttpClientV2

import scala.concurrent.{ExecutionContext, Future}

class CalculatorConnector @Inject()(applicationConfig: ApplicationConfig,
                                    http: HttpClientV2)
                                   (implicit ec: ExecutionContext) {

  def getCalculatorResult(calculatorInput: CalculatorInput)(implicit hc: HeaderCarrier): Future[CalculatorOutput] = {
    val url = s"${applicationConfig.calculatorUrl}"
    http.post(url"$url").withBody(Json.toJson(calculatorInput)).execute[CalculatorOutput]
  }
}
