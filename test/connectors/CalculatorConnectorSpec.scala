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
import controllers.FakeCCEligibilityApplication
import models.input.CalculatorOutput
import models.output.CalculatorInput
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.mockito.MockitoSugar
import uk.gov.hmrc.http.client.{HttpClientV2, RequestBuilder}
import scala.concurrent.{ExecutionContext, Future}

class CalculatorConnectorSpec extends AnyWordSpec with MockitoSugar with FakeCCEligibilityApplication with BeforeAndAfterEach {

  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global
  val requestBuilder: RequestBuilder = mock[RequestBuilder]

  "CalculatorConnector" must {

    "get calculator result" in {
      val mockHttp =  mock[HttpClientV2]
      val testCalculatorConnector = new CalculatorConnector(app.injector.instanceOf[ApplicationConfig], mockHttp)

      val testOutput = CalculatorOutput(
        tcAmount = Some(100),
        tfcAmount = Some(200),
        escAmount = Some(300)
      )

      when(mockHttp.post(any)(any)).thenReturn(requestBuilder)
      when(requestBuilder.withBody(any)(any, any, any)).thenReturn(requestBuilder)
      when(requestBuilder.execute[CalculatorOutput](any, any))
        .thenReturn(Future.successful(testOutput))
      val result = testCalculatorConnector.getCalculatorResult(CalculatorInput( tfc = None, esc = None))
      await(result) shouldBe testOutput
    }

  }

}
