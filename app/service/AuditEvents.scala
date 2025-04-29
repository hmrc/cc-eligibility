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

package service

import javax.inject.Inject
import uk.gov.hmrc.http.HeaderCarrier

class AuditEvents @Inject() (auditService: AuditService) {

  def auditMinEarnings(data: Boolean)(implicit hc: HeaderCarrier): Unit =
    auditEvent("HouseholdMinimumEarnings", Map("failedHouseholdMinimumEarnings" -> data.toString))

  def auditHouseholdRequest(data: String)(implicit hc: HeaderCarrier): Unit =
    auditEvent("HouseholdRequest", Map("HouseholdRequest" -> data))

  def auditHouseholdResponse(data: String)(implicit hc: HeaderCarrier): Unit =
    auditEvent("HouseholdResponse", Map("HouseholdResponse" -> data))

  private def auditEvent(auditEventType: String, data: Map[String, String])(implicit hc: HeaderCarrier): Unit =
    auditService.sendEvent(auditEventType, data)

}
