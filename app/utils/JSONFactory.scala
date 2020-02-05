/*
 * Copyright 2020 HM Revenue & Customs
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

package utils


import play.api.libs.json._

object JSONFactory extends JSONFactory

trait JSONFactory {

  def generateErrorJSON(status: Int, errors: Either[Seq[(JsPath, Seq[JsonValidationError])], Exception]): JsObject = {
    errors match {
      case Left(e) =>
        val errorsSequence = errorBuilder(e)
        Json.obj("status" -> status, "errors" -> errorsSequence)
      case Right(e) =>
        Json.obj("status" -> status, "error" -> s"${e.getMessage}")
    }
  }

  def errorBuilder(errors: Seq[(JsPath, Seq[JsonValidationError])]): JsArray = {
    if(errors.nonEmpty) {
        JsArray(
          errors.map {
            case (path, validationErrors) => Json.obj(
              "path" -> Json.toJson(path.toString()),
              "validationErrors" -> JsArray(validationErrors.map(
                validationError => Json.obj(
                  "message" -> JsString(validationError.message),
                  "args" -> JsArray(validationError.args.map {
                      case x: Int => JsNumber(x)
                      case x => JsString(x.toString)
                    }
                  )
                ))
              )
            )
          }
        )
      } else { JsArray(Seq(JsString("Error while generating JSON response"))) }
  }
}
