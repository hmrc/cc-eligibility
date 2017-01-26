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

package models.output

import models.output.esc.ESCEligibilityModel
import models.output.tc.TCEligibilityModel
import models.output.tfc.TFCEligibilityModel
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Writes}

object OutputAPIModel {

  case class Eligibility(
                          tc: Option[TCEligibilityModel] = None,
                          tfc: Option[TFCEligibilityModel] = None,
                          esc: Option[ESCEligibilityModel] = None
                        )

  object Eligibility {
    implicit val eligibilityWrites : Writes[Eligibility] = (
      (JsPath \ "tc").write[Option[TCEligibilityModel]] and
        (JsPath \ "tfc").write[Option[TFCEligibilityModel]] and
          (JsPath \ "esc").write[Option[ESCEligibilityModel]]
      )(unlift(Eligibility.unapply))
  }

  case class Response(
                      eligibility: Eligibility
                     )

  object Response {

    implicit val responseWrites: Writes[Response] = (JsPath \ "eligibility").write[Eligibility].contramap { (response: Response) => response.eligibility }

  }

}
