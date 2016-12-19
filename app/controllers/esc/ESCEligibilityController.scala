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

package controllers.esc

import controllers.EligibilityController
import eligibility.ESCEligibility
import models.input.esc.Request
import play.api.Logger
import play.api.libs.json.JsValue
import play.api.mvc.Action
import service.AuditEvents

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ESCEligibilityController extends ESCEligibilityController with ESCEligibility{
  override val auditEvent = AuditEvents
}


trait ESCEligibilityController extends EligibilityController {
  this: ESCEligibility =>

  val auditEvent : AuditEvents

  override def eligible : Action[JsValue] = Action.async(parse.json) {
    implicit request =>
      request.body.validate[Request].fold(
        error => {
          Logger.warn(s"\n\nESC Validation JsError *****\n\n")
          Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(play.api.http.Status.BAD_REQUEST, Left(error))))
        },
        result => {
          auditEvent.auditESCRequest(result.toString)
          eligibility.eligibility(result).map {
            response =>
              auditEvent.auditESCResponse(utils.JSONFactory.generateResultJson(response).toString())
              Ok(utils.JSONFactory.generateResultJson(response))
          } recover {
            case e: Exception =>
              Logger.warn(s"\n\nESC Eligibility Exception: ${e.getMessage}\n\n")
              InternalServerError(utils.JSONFactory.generateErrorJSON(play.api.http.Status.INTERNAL_SERVER_ERROR, Right(e)))
          }
        }
      )
  }
}
