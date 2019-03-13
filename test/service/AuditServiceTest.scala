/*
 * Copyright 2019 HM Revenue & Customs
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

package service

import org.mockito.ArgumentMatchers.any
import uk.gov.hmrc.play.audit.http.connector.AuditResult.Success
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.{ForwardedFor, SessionId}
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.DataEvent
import utils.CCConfigSpec

import scala.concurrent.Future

class AuditServiceTest extends CCConfigSpec with MockitoSugar {

  val mockAuditConnector = mock[AuditConnector]
  val testAuditService = new AuditService(mockAuditConnector)

  "AuditService" should {
    implicit val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()
    implicit val hc: HeaderCarrier = HeaderCarrier(
      forwarded = Some(ForwardedFor("test-IP")),
      sessionId = Some(SessionId("sessionid-random"))
    )

    "return a success when calling sendEvent" in {
      when(mockAuditConnector.sendEvent(any())(any(), any()))
        .thenReturn(Future.successful(Success))

      await(testAuditService.sendEvent(
        auditType = "test",
        details = Map(),
        sessionId = Some("id")
      )) shouldBe Success
    }

    "return a success when calling buildEvent" in {

      val dataEvent = DataEvent(
        auditSource =  "cc-eligibility",
        auditType = "test",
        eventId = "autoGenerated",
        tags = hc.headers.toMap,
        detail = Map("testKey" -> "testValue")
      )

     val result = testAuditService.buildEvent(
        auditType = "test",
        details = Map("testKey" -> "testValue"),
        sessionId = Some("id")
      )

      result.copy(eventId = "autoGenerated") shouldBe dataEvent
    }
  }
}
