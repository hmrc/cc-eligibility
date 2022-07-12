/*
 * Copyright 2022 HM Revenue & Customs
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
import uk.gov.hmrc.play.bootstrap.http.DefaultHttpClient

import scala.concurrent.Future

class CalculatorConnectorSpec extends AnyWordSpec with MockitoSugar with FakeCCEligibilityApplication with BeforeAndAfterEach {

  "CalculatorConnector" must {

    "get calculator result" in {
      val mockHttp =  mock[DefaultHttpClient]
      val testCalculatorConnector = new CalculatorConnector(app.injector.instanceOf[ApplicationConfig], mockHttp)

      val testOutput = CalculatorOutput(
        tcAmount = Some(100),
        tfcAmount = Some(200),
        escAmount = Some(300)
      )

      when(mockHttp.POST[CalculatorInput, CalculatorOutput](anyString, any(), any())(any(), any(), any(), any()))
        .thenReturn(Future.successful(testOutput))

      val result = testCalculatorConnector.getCalculatorResult(CalculatorInput(tc = None, tfc = None, esc = None))
      await(result) shouldBe testOutput
    }

  }

}
