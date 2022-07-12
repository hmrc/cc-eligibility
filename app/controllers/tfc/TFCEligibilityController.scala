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

package controllers.tfc

import eligibility.TFCEligibility
import javax.inject.Inject
import models.input.tfc.TFCEligibilityInput
import play.api.Logging
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{Action, ControllerComponents}
import service.AuditEvents
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class TFCEligibilityController @Inject ()(val tfcEligibility: TFCEligibility,
                                          val auditEvent: AuditEvents,
                                          cc: ControllerComponents) extends BackendController(cc) with Logging {

  def eligible : Action[JsValue] = Action.async(cc.parsers.json) {
    implicit request =>
      request.body.validate[TFCEligibilityInput].fold(
        error => {
          logger.warn(s"TFCEligibilityController TFC Validation JsError *****$error")
          Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(play.api.http.Status.BAD_REQUEST, Left(error))))
        },
        result => {
          auditEvent.auditTFCRequest(result.toString)
          tfcEligibility.eligibility(result).map {
            response =>
              auditEvent.auditTFCResponse(Json.toJson(response).toString())
              Ok(Json.toJson(response))
          } recover {
            case e: Exception =>
              logger.warn(s"TFCEligibilityController TFC Eligibility Exception: ${e.getMessage}")
              InternalServerError(utils.JSONFactory.generateErrorJSON(play.api.http.Status.INTERNAL_SERVER_ERROR, Right(e)))
          }
        }
      )
  }
}
