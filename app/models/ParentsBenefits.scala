/*
 * Copyright 2025 HM Revenue & Customs
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

package models

import play.api.libs.json.{Format, JsString, Reads, Writes}

sealed trait ParentsBenefits

object ParentsBenefits {
  case object CarersAllowance extends ParentsBenefits
  case object IncapacityBenefit extends ParentsBenefits
  case object SevereDisablementAllowance extends ParentsBenefits
  case object ContributionBasedEmploymentAndSupportAllowance extends ParentsBenefits
  case object NICreditsForIncapacityOrLimitedCapabilityForWork extends ParentsBenefits
  case object CarersCredit extends ParentsBenefits
  case object NoneOfThese extends ParentsBenefits

  private val allParentsBenefits = List(
    CarersAllowance,
    IncapacityBenefit,
    SevereDisablementAllowance,
    ContributionBasedEmploymentAndSupportAllowance,
    NICreditsForIncapacityOrLimitedCapabilityForWork,
    CarersCredit,
    NoneOfThese
  )

  private val mapping: Map[String, ParentsBenefits] =
    allParentsBenefits.map { benefit =>
      benefit.toString -> benefit
    }.toMap

  private val inverseMapping: Map[ParentsBenefits, String] = mapping.map(_.swap)

  private val reads: Reads[ParentsBenefits] = Reads { json =>
    json.validate[String].map(mapping)
  }

  private val writes: Writes[ParentsBenefits] = Writes { benefits =>
    JsString(inverseMapping(benefits))
  }

  implicit val format: Format[ParentsBenefits] = Format(reads, writes)
}
