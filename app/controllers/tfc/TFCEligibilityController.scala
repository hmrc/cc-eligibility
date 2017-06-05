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

package controllers.tfc

import controllers.EligibilityController
import eligibility.TFCEligibility
import models.input.tfc.Request
import play.api.Logger
import play.api.libs.json.JsValue
import play.api.mvc.Action
import service.AuditEvents
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object TFCEligibilityController extends TFCEligibilityController with TFCEligibility {
  override val auditEvent = AuditEvents
}

trait TFCEligibilityController extends EligibilityController {

  this : TFCEligibility =>
  val auditEvent : AuditEvents

  override def eligible : Action[JsValue] = Action.async(parse.json) {
    implicit request =>
      request.body.validate[Request].fold(
        error => {
          Logger.warn(s"TFC Validation JsError *****\n")
          Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(play.api.http.Status.BAD_REQUEST, Left(error))))
        },
        result => {
          auditEvent.auditTFCRequest(result.toString)
          eligibility.eligibility(result).map {
            response =>
              //auditEvent.auditTFCResponse(utils.JSONFactory.generateResultJson(response).toString())
              Ok(utils.JSONFactory.generateResultJson(response))
          } recover {
            case e: Exception =>
              Logger.warn(s"Tax Free Childcare Eligibility Exception: ${e.getMessage}\n")
              InternalServerError(utils.JSONFactory.generateErrorJSON(play.api.http.Status.INTERNAL_SERVER_ERROR, Right(e)))
          }
        }
      )
  }
}
