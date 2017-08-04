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

package connectors

import config.WSHttp
import controllers.FakeCCEligibilityApplication
import models.input.CalculatorOutput
import models.output.CalculatorInput
import org.scalatest.BeforeAndAfterEach
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.scalatest.mock.MockitoSugar
import uk.gov.hmrc.play.http.HttpPost
import uk.gov.hmrc.play.http.ws.WSHttp
import uk.gov.hmrc.play.test.UnitSpec
import scala.concurrent.Future

class CalculatorConnectorSpec extends UnitSpec with MockitoSugar with FakeCCEligibilityApplication with BeforeAndAfterEach {

  val sut = new CalculatorConnector {
    override val httpPost: HttpPost = mock[HttpPost]
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(sut.httpPost)
  }

  "check that httpPost is set" in {
    CalculatorConnector.httpPost shouldBe WSHttp
  }

  "get calculator result" in {
    val testOutput = CalculatorOutput(
      tcAmount = Some(100),
      tfcAmount = Some(200),
      escAmount = Some(300)
    )

    when(
      sut.httpPost.POST[CalculatorInput, CalculatorOutput](anyString, any(), any())(any(), any(), any())
    ).thenReturn(
      Future.successful(testOutput)
    )

    val result = await(sut.getCalculatorResult(CalculatorInput(tc = None, tfc = None, esc = None)))
    result shouldBe testOutput
  }

}