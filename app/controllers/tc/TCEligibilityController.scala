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

package controllers.tc

import controllers.EligibilityController
import eligibility.TCEligibility
import models.input.tc.TCEligibilityInput
import play.api.Logger
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.Action
import service.AuditEvents

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object TCEligibilityController extends TCEligibilityController {
  override val tcEligibility: TCEligibility = TCEligibility
  override val auditEvent = AuditEvents
}

trait TCEligibilityController extends EligibilityController {
  val tcEligibility: TCEligibility

  val auditEvent : AuditEvents

  override def eligible : Action[JsValue] = Action.async(parse.json) {
    implicit request =>
      request.body.validate[TCEligibilityInput].fold(
        error => {
          Logger.warn(s"TC Validation JsError ******")
          Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(play.api.http.Status.BAD_REQUEST, Left(error))))
        },
        result => {
          auditEvent.auditTCRequest(result.toString)
          tcEligibility.eligibility(result).map {
            response =>
              auditEvent.auditTCResponse(Json.toJson(response).toString())
              Ok(Json.toJson(response))
          } recover {
            case e: Exception =>
              Logger.warn(s"Tax Credits Eligibility Exception: ${e.getMessage}")
              InternalServerError(utils.JSONFactory.generateErrorJSON(play.api.http.Status.INTERNAL_SERVER_ERROR, Right(e)))
          }
        }
      )
  }
}
