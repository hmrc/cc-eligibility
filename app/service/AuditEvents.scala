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

import javax.inject.Inject
import play.api.mvc.Request
import uk.gov.hmrc.http.HeaderCarrier

class AuditEvents @Inject()(auditService: AuditService) {

  def auditTFCRequest(data : String) (implicit hc: HeaderCarrier): Unit = {
    auditEvent("TFCRequest", Map("TFCRequest" -> data.toString))
  }

  def auditTFCResponse(data : String) (implicit hc: HeaderCarrier): Unit = {
    auditEvent("TFCResponse", Map("TFCResponse" -> data.toString))
  }

  def auditTCRequest(data : String) (implicit hc: HeaderCarrier): Unit = {
    auditEvent("TCRequest", Map("TCRequest" -> data.toString))
  }

  def auditTCResponse(data : String) (implicit hc: HeaderCarrier): Unit = {
    auditEvent("TCResponse", Map("TCResponse" -> data.toString))
  }

  def auditESCRequest(data : String) (implicit hc: HeaderCarrier): Unit = {
    auditEvent("ESCRequest", Map("ESCRequest" -> data.toString))
  }

  def auditESCResponse(data : String) (implicit hc: HeaderCarrier): Unit = {
    auditEvent("ESCResponse", Map("ESCResponse" -> data.toString))
  }

  def auditMinEarnings(data : Boolean) (implicit hc: HeaderCarrier): Unit = {
    auditEvent("HouseholdMinimumEarnings", Map("failedHouseholdMinimumEarnings" -> data.toString))
  }

  def auditAgeGroup(user: String, data : String) (implicit hc: HeaderCarrier): Unit = {
    auditEvent("HouseholdMinimumEarnings", Map(s"$user-failedAgeGroup" -> data.toString))
  }

  def auditSelfEmploymentStatus(user: String, data : String) (implicit hc: HeaderCarrier): Unit = {
    auditEvent("HouseholdMinimumEarnings", Map(s"$user-selfEmployedUsers" -> data.toString))
  }

  def auditSelfEmployedin1st(user: String, data : Boolean) (implicit hc: HeaderCarrier): Unit = {
    auditEvent("HouseholdMinimumEarnings", Map(s"$user-selfEmployedin1stYear" -> data.toString))
  }

  def auditFreeEntitlementRequest(data : String) (implicit hc: HeaderCarrier): Unit = {
    auditEvent("FreeEntitlmentRequest", Map("FreeEntitlmentRequest" -> data.toString))
  }

  def auditFreeEntitlementResponse(data : String) (implicit hc: HeaderCarrier): Unit = {
    auditEvent("FreeEntitlmentResponse", Map("FreeEntitlmentResponse" -> data.toString))
  }

  def auditHouseholdRequest(data : String) (implicit hc: HeaderCarrier): Unit = {
    auditEvent("HouseholdRequest", Map("HouseholdRequest" -> data.toString))
  }

  def auditHouseholdResponse(data : String) (implicit hc: HeaderCarrier): Unit = {
    auditEvent("HouseholdResponse", Map("HouseholdResponse" -> data.toString))
  }

  private def auditEvent(auditEventType : String, data: Map[String, String]) (implicit hc: HeaderCarrier): Unit = {
    auditService.sendEvent(auditEventType, data)
  }

}
