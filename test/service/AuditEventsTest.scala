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

package service

import akka.stream.Materializer
import controllers.FakeCCEligibilityApplication
import org.scalatest.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.mockito.MockitoSugar
import play.api.inject.ApplicationLifecycle
import play.api.test.FakeRequest
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.config.AuditingConfig
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.DataEvent

import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by user on 25/04/16.
 */
class AuditEventsTest extends FakeCCEligibilityApplication with MockitoSugar {

  def createObservableAuditConnector = new ObservableAuditConnector {
    def auditChannel: uk.gov.hmrc.play.audit.http.connector.AuditChannel = ???
    def auditCounter: uk.gov.hmrc.play.audit.http.connector.AuditCounter = ???
  }

  def createAuditor(observableAuditConnector: ObservableAuditConnector): AuditEvents = {

    val testAuditService = new AuditService(observableAuditConnector)

    new AuditEvents(
      testAuditService
    )
  }

  trait ObservableAuditConnector extends AuditConnector {
    var events: ListBuffer[DataEvent] = new ListBuffer[DataEvent]

    def observedEvents: ListBuffer[DataEvent] = events

    override def auditingConfig: AuditingConfig = ???

    override def sendEvent(event: DataEvent)(implicit hc: HeaderCarrier = HeaderCarrier(), ec: ExecutionContext): Future[AuditResult] = {
      addEvent(event.asInstanceOf[DataEvent])
      Future.successful(AuditResult.Success)
    }

    def addEvent(auditEvent: DataEvent): Unit = {
      events = events += auditEvent
    }
  }

  "Audit Events" must {
    implicit val hc = new HeaderCarrier()

    "audit request received for TFC - success " in {
      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditTFCRequest("Data")

      val event = observableAuditConnector.events.head

      event.auditType must equal("TFCRequest")
      event.detail("TFCRequest") should startWith("Data")

    }

    "audit response processed for TFC - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditTFCResponse("Data")

      val event = observableAuditConnector.events.head

      event.auditType must equal("TFCResponse")
      event.detail("TFCResponse") should startWith("Data")

    }

    "audit request received for TC - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditTCRequest("Data")

      val event = observableAuditConnector.events.head

      event.auditType must equal("TCRequest")
      event.detail("TCRequest") should startWith("Data")

    }

    "audit response processed for TC - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditTCResponse("Data")

      val event = observableAuditConnector.events.head

      event.auditType must equal("TCResponse")
      event.detail("TCResponse") should startWith("Data")

    }

    "audit request received for ESC - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditESCRequest("Data")

      val event = observableAuditConnector.events.head

      event.auditType must equal("ESCRequest")
      event.detail("ESCRequest") should startWith("Data")

    }

    "audit response processed for ESC - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditESCResponse("Data")

      val event = observableAuditConnector.events.head

      event.auditType must equal("ESCResponse")
      event.detail("ESCResponse") should startWith("Data")

    }

    "audit request received for FreeEntitlement - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditFreeEntitlementRequest("Data")

      val event = observableAuditConnector.events.head

      event.auditType must equal("FreeEntitlmentRequest")
      event.detail("FreeEntitlmentRequest") should startWith("Data")

    }

    "audit response processed for FreeEntitlement - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditFreeEntitlementResponse("Data")

      val event = observableAuditConnector.events.head

      event.auditType must equal("FreeEntitlmentResponse")
      event.detail("FreeEntitlmentResponse") should startWith("Data")

    }

    "audit request received for household eligibility - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditHouseholdRequest("Data")

      val event = observableAuditConnector.events.head

      event.auditType must equal("HouseholdRequest")
      event.detail("HouseholdRequest") should startWith("Data")

    }

    "audit response processed for Household eligibility - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditHouseholdResponse("Data")

      val event = observableAuditConnector.events.head

      event.auditType must equal("HouseholdResponse")
      event.detail("HouseholdResponse") should startWith("Data")

    }
  }


}
