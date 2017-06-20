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

package controllers.freeEntitlement

import eligibility.FreeEntitlementService
import models.input.freeEntitlement.FreeEntitlementPayload
import models.input.tfc.Request
import play.api.Logger
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.Action
import service.AuditEvents
import uk.gov.hmrc.play.microservice.controller.BaseController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object FreeEntitlementController extends FreeEntitlementController {
  override val auditEvent = AuditEvents

  override val freeHoursService: FreeEntitlementService = FreeEntitlementService
}

trait FreeEntitlementController extends BaseController {

  val auditEvent: AuditEvents

  val freeHoursService: FreeEntitlementService

  def fifteenHours: Action[JsValue] = Action.async(parse.json) {
    implicit request =>
      request.body.validate[FreeEntitlementPayload].fold(
        error => {
          Logger.warn(s"FreeEntitlement Validation JsError *****\n")
          Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(play.api.http.Status.BAD_REQUEST, Left(error))))
        },
        result => {
          auditEvent.auditFreeEntitlementRequest(result.toString)
          freeHoursService.fifteenHours(result).map {
            response =>
              auditEvent.auditFreeEntitlementResponse(Json.toJson(response).toString())
              Ok(Json.toJson(response))
          } recover {
            case e: Exception =>
              Logger.warn(s"FreeEntitlement Eligibility Exception: ${e.getMessage}\n")
              InternalServerError(utils.JSONFactory.generateErrorJSON(play.api.http.Status.INTERNAL_SERVER_ERROR, Right(e)))
          }
        }
      )
  }

  def thirtyHours: Action[JsValue] = Action.async(parse.json) {
    implicit request =>
      request.body.validate[Request].fold(
        error => {
          Logger.warn("Thirty Hours Validation JsError *****")
          Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(play.api.http.Status.BAD_REQUEST, Left(error))))
        },
        result => {
          freeHoursService.thirtyHours(result).map { response =>
            // TODO: Audit event?
            Ok(Json.toJson(response))
          }.recover {
            case e: Exception =>
              Logger.warn(s"Tax Free Childcare Eligibility Exception: ${e.getMessage}")
              InternalServerError(utils.JSONFactory.generateErrorJSON(play.api.http.Status.INTERNAL_SERVER_ERROR, Right(e)))
          }
        }
      )
  }
}
