/*
 * Copyright 2023 HM Revenue & Customs
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

import javax.inject.Inject
import models.input.tfc.TFCChild
import play.api.Configuration

import java.time.LocalDate

class TFCRolloutSchemeConfig @Inject()(config: CCConfig) {

  def futureDate: LocalDate = config.startDate.plusWeeks(2)

  def isChildEligibleForTFCRollout(child: TFCChild, isEligibleForTFC: Boolean): Boolean = {
    val tfcRollout: Configuration = config.loadConfigByType("tfc-rollout")
    val bornOnOrAfter = config.dateFormat.parse(tfcRollout.get[String]("born-on-after"))
    val isAvailableForAllDisabled: Boolean = tfcRollout.getOptional[Boolean]("all-disabled").getOrElse(false)

    isEligibleForTFC && child.dob.isBefore(futureDate) && ((child.isDisabled && isAvailableForAllDisabled) || !bornOnOrAfter.after(config.toDate(child.dob)))
  }

}
