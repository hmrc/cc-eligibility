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

import controllers.FakeCCEligibilityApplication
import org.scalatest.mock.MockitoSugar
import play.api.test.FakeRequest
import spec.CCConfigSpec
import uk.gov.hmrc.play.audit.http.config.AuditingConfig
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.{AuditEvent, DataEvent}
import uk.gov.hmrc.play.http.HeaderCarrier
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by user on 25/04/16.
 */
class AuditEventsTest extends CCConfigSpec with FakeCCEligibilityApplication with MockitoSugar {

  trait ObservableAuditConnector extends AuditConnector {
    var events : ListBuffer[DataEvent] = new ListBuffer[DataEvent]

    def observedEvents : ListBuffer[DataEvent] = events

    def addEvent(auditEvent : DataEvent): Unit = {
      events = events += auditEvent
    }

    override def auditingConfig: AuditingConfig = ???
    override def sendEvent(event: AuditEvent)(implicit hc: HeaderCarrier = HeaderCarrier(), ec : ExecutionContext): Future[AuditResult] = {
      addEvent(event.asInstanceOf[DataEvent])
      Future.successful(AuditResult.Success)
    }
  }

  def createObservableAuditConnector = new ObservableAuditConnector{}

  def createAuditor(observableAuditConnector : ObservableAuditConnector) = {

    val testAuditService = new AuditService {
      override lazy val auditSource = "cc-eligibility"
      override def auditConnector = observableAuditConnector
    }

    new AuditEvents {
      override val auditService : AuditService = testAuditService
    }
  }

  "Audit Events" should {
    implicit val request = FakeRequest()
    implicit var hc = new HeaderCarrier()

    "audit request received for TFC - success " in {
      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditTFCRequest("Data")

      val event =  observableAuditConnector.events.head

      event.auditType should equal("TFCRequest")
      event.detail("TFCRequest") should startWith("Data")

    }

    "audit response processed for TFC - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditTFCResponse("Data")

      val event =  observableAuditConnector.events.head

      event.auditType should equal("TFCResponse")
      event.detail("TFCResponse") should startWith("Data")

    }

    "audit request received for TC - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditTCRequest("Data")

      val event =  observableAuditConnector.events.head

      event.auditType should equal("TCRequest")
      event.detail("TCRequest") should startWith("Data")

    }

    "audit response processed for TC - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditTCResponse("Data")

      val event =  observableAuditConnector.events.head

      event.auditType should equal("TCResponse")
      event.detail("TCResponse") should startWith("Data")

    }

    "audit request received for ESC - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditESCRequest("Data")

      val event =  observableAuditConnector.events.head

      event.auditType should equal("ESCRequest")
      event.detail("ESCRequest") should startWith("Data")

    }

    "audit response processed for ESC - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditESCResponse("Data")

      val event =  observableAuditConnector.events.head

      event.auditType should equal("ESCResponse")
      event.detail("ESCResponse") should startWith("Data")

    }

    "audit request received for FreeEntitlement - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditFreeEntitlementRequest("Data")

      val event =  observableAuditConnector.events.head

      event.auditType should equal("FreeEntitlmentRequest")
      event.detail("FreeEntitlmentRequest") should startWith("Data")

    }

    "audit response processed for FreeEntitlement - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditFreeEntitlementResponse("Data")

      val event =  observableAuditConnector.events.head

      event.auditType should equal("FreeEntitlmentResponse")
      event.detail("FreeEntitlmentResponse") should startWith("Data")

    }
  }


}
