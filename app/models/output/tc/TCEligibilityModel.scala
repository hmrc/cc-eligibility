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

package models.output.tc

import org.joda.time.LocalDate
import play.api.libs.functional.syntax._
import play.api.libs.json.Writes._
import play.api.libs.json.{JsPath, Writes}
import utils.{CCFormat, Periods}

case class TCEligibilityModel(
                               eligible: Boolean = false,
                               taxYears: List[TaxYear]
                               )

object TCEligibilityModel {
  implicit val tcEligible : Writes[TCEligibilityModel] = (
    (JsPath \ "eligible").write[Boolean] and
      (JsPath \ "taxYears").write[List[TaxYear]]
    )(unlift(TCEligibilityModel.unapply))
}

case class TaxYear(
                    from: LocalDate,
                    until: LocalDate,
                    houseHoldIncome: BigDecimal,
                    periods: List[TCPeriod]
                    )

object TaxYear extends CCFormat{
  implicit val taxYearWrites: Writes[TaxYear] = (
    (JsPath \ "from").write[LocalDate](jodaLocalDateWrites(datePattern)) and
      (JsPath \ "until").write[LocalDate](jodaLocalDateWrites(datePattern)) and
        (JsPath \ "houseHoldIncome").write[BigDecimal] and
          (JsPath \ "periods").write[List[TCPeriod]]
    )(unlift(TaxYear.unapply))
}

case class TCPeriod(
                   from: LocalDate,
                   until: LocalDate,
                   householdElements: HouseholdElements,
                   claimants: List[OutputClaimant],
                   children: List[OutputChild]
                   )

object TCPeriod extends CCFormat{
  implicit val periodWrites : Writes[TCPeriod] = (
    (JsPath \ "from").write[LocalDate](jodaLocalDateWrites(datePattern)) and
      (JsPath \ "until").write[LocalDate](jodaLocalDateWrites(datePattern)) and
        (JsPath \ "householdElements").write[HouseholdElements] and
          (JsPath \ "claimants").write[List[OutputClaimant]] and
            (JsPath \ "children").write[List[OutputChild]]
    )(unlift(TCPeriod.unapply))
}

case class HouseholdElements(
                            basic: Boolean = false,
                            hours30: Boolean = false,
                            childcare: Boolean = false,
                            loneParent: Boolean = false,
                            secondParent: Boolean = false,
                            family: Boolean = false,
                            wtc: Boolean = false,
                            ctc: Boolean = false
                              )

object HouseholdElements {
  implicit val householdWrites : Writes[HouseholdElements] = (
    (JsPath \ "basic").write[Boolean] and
      (JsPath \ "hours30").write[Boolean] and
        (JsPath \ "childcare").write[Boolean] and
          (JsPath \ "loneParent").write[Boolean] and
            (JsPath \ "secondParent").write[Boolean] and
              (JsPath \ "family").write[Boolean] and
                (JsPath \ "wtc").write[Boolean] and
                  (JsPath \ "ctc").write[Boolean]
    )(unlift(HouseholdElements.unapply))
}

case class OutputClaimant(
                         qualifying: Boolean = false,
                         isPartner: Boolean = false,
                         claimantDisability: ClaimantDisability,
                         failures: List[String]
                         )

object OutputClaimant {
  implicit val claimantWrites : Writes[OutputClaimant] = (
      (JsPath \ "qualifying").write[Boolean] and
        (JsPath \ "isPartner").write[Boolean] and
          (JsPath \ "claimantDisability").write[ClaimantDisability] and
            (JsPath \ "failures").write[List[String]]
    )(unlift(OutputClaimant.unapply))

}

case class ClaimantDisability(
                             disability: Boolean,
                             severeDisability: Boolean
                             )

object ClaimantDisability {
  implicit val claimantElementWrites : Writes[ClaimantDisability] = (
    (JsPath \ "disability").write[Boolean] and
      (JsPath \ "severeDisability").write[Boolean]
    )(unlift(ClaimantDisability.unapply))
}

case class OutputChild(
                      id: Short,
                      name: Option[String],
                      childcareCost: BigDecimal = BigDecimal(0.00),
                      childcareCostPeriod: Periods.Period,
                      qualifying: Boolean = false,
                      childElements: ChildElements,
                      failures: List[String]
                      )

object OutputChild {
  implicit val childWrites : Writes[OutputChild] = (
    (JsPath \ "id").write[Short] and
      (JsPath \ "name").writeNullable[String] and
        (JsPath \ "childcareCost").write[BigDecimal] and
          (JsPath \ "childcareCostPeriod").write[Periods.Period] and
            (JsPath \ "qualifying").write[Boolean] and
              (JsPath \ "childElements").write[ChildElements] and
                (JsPath \ "failures").write[List[String]]
    )(unlift(OutputChild.unapply))
}

case class ChildElements(
                          child: Boolean = false,
                          youngAdult: Boolean = false,
                          disability: Boolean = false,
                          severeDisability: Boolean = false,
                          childcare: Boolean = false
                          )

object ChildElements {
  implicit val childElementsWrites : Writes[ChildElements] = (
    (JsPath \ "child").write[Boolean] and
      (JsPath \ "youngAdult").write[Boolean] and
        (JsPath \ "disability").write[Boolean] and
          (JsPath \ "severeDisability").write[Boolean] and
            (JsPath \ "childcare").write[Boolean]
    )(unlift(ChildElements.unapply))
}
