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

package controllers.esc

import eligibility.ESCEligibility
import javax.inject.Inject
import models.input.esc.ESCEligibilityInput
import play.api.Logging
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{Action, ControllerComponents}
import service.AuditEvents
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController
import utils.{CCConfig, ESCConfig}

import scala.concurrent.{ExecutionContext, Future}

class ESCEligibilityController @Inject()(escEligibility: ESCEligibility,
                                         auditEvent: AuditEvents,
                                         cc: ControllerComponents,
                                         eSCConfig: ESCConfig, cCConfig: CCConfig)
                                        (implicit ec: ExecutionContext) extends BackendController(cc) with Logging {

  def eligible : Action[JsValue] = Action.async(cc.parsers.json) {
    implicit request =>
      request.body.validate[ESCEligibilityInput].fold(
        error => {
          logger.warn(s"ESCEligibilityController ESC Validation JsError *****$error")
          Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(play.api.http.Status.BAD_REQUEST, Left(error))))
        },
        result => {
          auditEvent.auditESCRequest(result.toString)
          escEligibility.eligibility(result, eSCConfig, cCConfig).map {
            response =>
              auditEvent.auditESCResponse(Json.toJson(response).toString())
              Ok(Json.toJson(response))
          } recover {
            case e: Exception =>
              logger.warn(s"ESCEligibilityController ESC Eligibility Exception: ${e.getMessage}")
              InternalServerError(utils.JSONFactory.generateErrorJSON(play.api.http.Status.INTERNAL_SERVER_ERROR, Right(e)))
          }
        }
      )
  }
}
