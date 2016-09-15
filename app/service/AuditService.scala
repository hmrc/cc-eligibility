/*
 * Copyright 2016 HM Revenue & Customs
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
import play.api.Logger
import play.api.mvc.Request
import uk.gov.hmrc.play.audit.http.config.LoadAuditingConfig
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.{DataEvent, DeviceId}
import uk.gov.hmrc.play.config.{RunMode, AppName}
import uk.gov.hmrc.play.http.HeaderCarrier


object AuditService extends AuditService {
  override lazy val auditSource = "cc-eligibility"
  override lazy val auditConnector = MicroserviceAuditConnector
}

trait AuditService {

  def auditSource : String

  def auditConnector: AuditConnector

  import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._

  def sendEvent(auditType:String, details: Map[String, String], sessionId: Option[String] = None)(implicit request: Request[_], hc: HeaderCarrier) = {
    auditConnector.sendEvent(buildEvent(auditType, details, sessionId))
  }

  def buildEvent(auditType:String, details: Map[String, String], sessionId: Option[String] = None)(implicit request: Request[_], hc: HeaderCarrier) = {
    Logger.debug(s"AuditService.buildEvent")
    val auditEvent = DataEvent(
      auditSource =  auditSource,
      auditType = auditType,
      tags = hc.headers.toMap,
      detail = generateDetails(request, details))
    auditEvent
  }

  private def generateDetails(request: Request[_], details: Map[String, String]): Map[String, String] = {
    Logger.debug(s"AuditService.generateDetails")
    details ++ Map("deviceID" -> DeviceId(request).map(_.id).getOrElse("-"))
  }

}
