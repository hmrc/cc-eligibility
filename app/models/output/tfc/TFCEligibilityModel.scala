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

package models.output.tfc

import play.api.libs.functional.syntax._
import play.api.libs.json.Writes._
import play.api.libs.json.{JsPath, Writes}
import utils.CCFormat
import org.joda.time.LocalDate

case class TFCEligibilityModel(
                                from: LocalDate,
                                until: LocalDate,
                                householdEligibility : Boolean = false,
                                periods: List[TFCPeriod]
                                )

object TFCEligibilityModel extends CCFormat {
  implicit val tfcEligible : Writes[TFCEligibilityModel] = (
    (JsPath \ "from").write[LocalDate](jodaLocalDateWrites(datePattern)) and
      (JsPath \ "until").write[LocalDate](jodaLocalDateWrites(datePattern)) and
        (JsPath \ "householdEligibility").write[Boolean] and
          (JsPath \ "periods").write[List[TFCPeriod]]
    )(unlift(TFCEligibilityModel.unapply))
}

case class TFCPeriod(
                      from: LocalDate,
                      until: LocalDate,
                      periodEligibility: Boolean = false,
                      claimants: List[OutputClaimant],
                      children: List[OutputChild]
                      )

object TFCPeriod extends CCFormat{
  implicit val periodWrites : Writes[TFCPeriod] = (
    (JsPath \ "from").write[LocalDate](jodaLocalDateWrites(datePattern)) and
      (JsPath \ "until").write[LocalDate](jodaLocalDateWrites(datePattern)) and
        (JsPath \ "periodEligibility").write[Boolean] and
          (JsPath \ "claimants").write[List[OutputClaimant]] and
            (JsPath \ "children").write[List[OutputChild]]
    )(unlift(TFCPeriod.unapply))
}

case class OutputClaimant(
                           qualifying: Boolean = false,
                           isPartner: Boolean = false,
                           failures: List[String]
                           )


object OutputClaimant extends CCFormat {
  implicit val claimantWrites: Writes[OutputClaimant] = (
    (JsPath \ "qualifying").write[Boolean] and
      (JsPath \ "isPartner").write[Boolean] and
        (JsPath \ "failures").write[List[String]]
    )(unlift(OutputClaimant.unapply))
}

  case class OutputChild(
                          id: Short,
                          name: Option[String],
                          qualifying: Boolean = false,
                          from: LocalDate,
                          until: LocalDate,
                          failures: List[String]
                          )

  object OutputChild extends CCFormat {
    implicit val childWrites : Writes[OutputChild] = (
      (JsPath \ "id").write[Short] and
        (JsPath \ "name").writeNullable[String] and
          (JsPath \ "qualifying").write[Boolean] and
            (JsPath \ "from").write[LocalDate](jodaLocalDateWrites(datePattern)) and
              (JsPath \ "until").write[LocalDate](jodaLocalDateWrites(datePattern)) and
                (JsPath \ "failures").write[List[String]]
      )(unlift(OutputChild.unapply))
  }
