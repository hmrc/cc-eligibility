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

package controllers.tc

import controllers.EligibilityController
import eligibility.TCEligibility
import models.input.tc.Request
import play.api.Logger
import play.api.libs.json.{JsValue, JsError}
import play.api.mvc.Action
import service.AuditEvents

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object TCEligibilityController extends TCEligibilityController with TCEligibility{
  override val auditEvent = AuditEvents
}


trait TCEligibilityController extends EligibilityController {
  this: TCEligibility =>

  val auditEvent : AuditEvents

  override def eligible : Action[JsValue] = Action.async(parse.json) {
    implicit request =>
      request.body.validate[Request].fold(
        error => {
          Logger.warn(s"\n\nTC Validation JsError: ${JsError.toFlatJson(error).toString()}\n\n")
          Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(play.api.http.Status.BAD_REQUEST, Left(error))))
        },
        result => {
          Logger.info(s"\n\nTC Validation passed : ${result.toString}\n\n")
          auditEvent.auditTCRequest(result.toString)
          eligibility.eligibility(result).map {
            response =>
              Logger.info(s"\n\nTC Eligibility Result: ${response.toString}\n\n")
              auditEvent.auditTFCResponse(utils.JSONFactory.generateResultJson(response).toString())
              Ok(utils.JSONFactory.generateResultJson(response))
          } recover {
            case e: Exception =>
              Logger.warn(s"\n\nTax Credits Eligibility Exception: ${e.getMessage}\n\n")
              InternalServerError(utils.JSONFactory.generateErrorJSON(play.api.http.Status.INTERNAL_SERVER_ERROR, Right(e)))
          }
        }
      )
  }
}
