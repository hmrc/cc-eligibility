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

package models.mappings

import models.input.freeEntitlement.FreeEntitlementEligibilityInput
import models.{Household, LocationEnum}

trait HHToFree30hoursEligibilityInput {

  def convert(household: Household): FreeEntitlementEligibilityInput = {
    val location = household.location.getOrElse(LocationEnum.ENGLAND)
    val childDOBList = household.children.map(_.dob).flatten

    FreeEntitlementEligibilityInput(location.toString, childDOBList)
  }

}

object HHToFree30hoursEligibilityInput extends HHToFree30hoursEligibilityInput