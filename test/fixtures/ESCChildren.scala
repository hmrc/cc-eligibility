/*
 * Copyright 2019 HM Revenue & Customs
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

package fixtures

import models.input.esc.{ESCChild, ESCDisability}
import models.output
import org.joda.time.LocalDate
import utils.Periods

trait ESCChildren {
  
  def buildChild(
                   id: Short = 0,
                   dob: LocalDate = LocalDate.now,
                   childCareCost: BigDecimal = 0,
                   disabled: Boolean = false,
                   severelyDisabled: Boolean = false
                     ) = ESCChild (
    id = id,
    dob = dob,
    childCareCost = childCareCost,
    childCareCostPeriod = Periods.Monthly,
    disability = ESCDisability(
      disabled = disabled,
      severelyDisabled = severelyDisabled
    )
  )

  def buildOutputChild(
                        qualifying: Boolean = false,
                        childCareCost: BigDecimal = 0,
                        childCareCostPeriod: Periods.Period = Periods.Monthly
                        ) = output.esc.ESCChild(
    qualifying = qualifying,
    childCareCost = childCareCost,
    childCareCostPeriod = childCareCostPeriod
  )
  
}
