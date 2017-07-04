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

package service

import config.MicroserviceAuditConnector
import play.api.test.FakeRequest
import spec.CCConfigSpec
import uk.gov.hmrc.play.audit.http.config.AuditingConfig
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.{AuditEvent, DataEvent}
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.http.logging.{ForwardedFor, SessionId}
import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by user on 22/04/16.
 */
class AuditServiceTest extends CCConfigSpec {

  "AuditService" when {

    "use the correct audit connector" in {
      AuditService.auditConnector shouldBe MicroserviceAuditConnector
    }

    "use the correct audit source" in {
      AuditService.auditSource shouldBe "cc-eligibility"
    }

    "auditer should send message" in {

      implicit val request = FakeRequest()

      implicit var hc = new HeaderCarrier(forwarded = Some(ForwardedFor("1.2.3.4,5.6.7.8")),  // test the IP address is in adutit request
        sessionId = Some(SessionId("sessionid-random")))

      val auditConnectorObj = new AuditConnector {

        var lastAuditEvent : Option[DataEvent]  = None

        override def auditingConfig: AuditingConfig = ???
        override def sendEvent(event: AuditEvent)(implicit hc: HeaderCarrier = HeaderCarrier(), ec : ExecutionContext): Future[AuditResult] = {
          lastAuditEvent = Some(event.asInstanceOf[DataEvent])
          Future.successful(AuditResult.Success)
        }
      }

      val auditTest = new AuditService {
        override def auditConnector = auditConnectorObj
        override def auditSource = "cc-eligibility"
      }

      auditTest.sendEvent("testTranType", Map("randomDetails" -> "+=+=+=+=+=+=+=+=+"))(request,hc)


      auditTest.sendEvent("testTranType", Map("randomDetails" -> "+=+=+=+=+=+=+=+=+"))

      val auditEvent : DataEvent = auditConnectorObj.lastAuditEvent.get

      auditEvent should not equal(Nil)

      auditEvent.auditSource should equal("cc-eligibility")
      auditEvent.auditType should equal("testTranType")
      auditEvent.detail("randomDetails") should equal("+=+=+=+=+=+=+=+=+")
      auditEvent.detail("deviceID") should equal("-")

    }

  }

}
