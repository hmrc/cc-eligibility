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

import play.api.mvc.Request
import uk.gov.hmrc.play.http.HeaderCarrier


/**
 * Created by user on 25/04/16.
 */
object AuditEvents extends AuditEvents {

  override def auditService: AuditService = AuditService
}

trait AuditEvents {

  def auditService : AuditService

  def auditTFCRequest(data : String) (implicit request: Request[_], hc: HeaderCarrier): Unit = {
    auditEvent("TFCRequest", data)
  }

  def auditTFCResponse(data : String) (implicit request: Request[_], hc: HeaderCarrier): Unit = {
    auditEvent("TFCResponse", data)
  }

  def auditTCRequest(data : String) (implicit request: Request[_], hc: HeaderCarrier): Unit = {
    auditEvent("TCRequest", data)
  }

  def auditTCResponse(data : String) (implicit request: Request[_], hc: HeaderCarrier): Unit = {
    auditEvent("TCResponse", data)
  }

  def auditESCRequest(data : String) (implicit request: Request[_], hc: HeaderCarrier): Unit = {
    auditEvent("ESCRequest", data)
  }

  def auditESCResponse(data : String) (implicit request: Request[_], hc: HeaderCarrier): Unit = {
    auditEvent("ESCResponse", data)
  }

  private def auditEvent(auditEventType : String, data: String) (implicit request: Request[_], hc: HeaderCarrier): Unit = {
    auditService.sendEvent(auditEventType, Map("data" -> data))
  }

}
