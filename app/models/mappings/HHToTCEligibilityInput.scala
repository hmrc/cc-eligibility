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

import models._
import models.input.tc._
import org.joda.time.LocalDate
import utils.{CCConfig, HelperManager}

object HHToTCEligibilityInput extends HHToTCEligibilityInput {
  override val cCConfig = CCConfig
}

trait HHToTCEligibilityInput extends PeriodEnumToPeriod with HelperManager {

  val cCConfig: CCConfig

  def convert(household: Household): TCEligibilityInput = {
    TCEligibilityInput(taxYears = createTaxYears(household.hasPartner, household.parent, household.partner, household.children))
  }

  private def createTaxYears(
                              hasPartner: Boolean,
                              parent: Claimant,
                              partner: Option[Claimant],
                              children: List[Child]
                            ): List[TCTaxYear] = {

    val now = cCConfig.StartDate
    val april6thCurrentYear = determineApril6DateFromNow(now)
    val claimantList = hhClaimantToTCEligibilityInputClaimant(hasPartner, parent, partner)
    val childList = hhChildToTEligibilityInputChild(children)

    List(
      TCTaxYear(
        from = now,
        until = april6thCurrentYear,
        claimants = claimantList,
        children = childList
      ),
      TCTaxYear(
        from = april6thCurrentYear,
        until = now.plusYears(1),
        claimants = claimantList,
        children = childList
      )
    )
  }

  private def createClaimant(claimant: Claimant, isPartner: Boolean): TCClaimant = {
    TCClaimant(
      hours = claimant.hours.getOrElse(BigDecimal(0.0)).doubleValue(),
      isPartner = isPartner,
      disability = TCDisability(claimant.benefits.exists(_.disabilityBenefits), claimant.benefits.exists(_.highRateDisabilityBenefits)),
      carersAllowance = claimant.benefits.exists(_.carersAllowance)
    )

  }
  private def hhClaimantToTCEligibilityInputClaimant(hasPartner: Boolean, hhParent: Claimant, hhPartner: Option[Claimant]): List[TCClaimant] = {

    val parent: TCClaimant = createClaimant(hhParent, false)
    if(hasPartner) {
      List(parent, createClaimant(hhPartner.get, true))
    } else {
      List(parent)
    }
  }

  private def hhChildToTEligibilityInputChild(hhChildren: List[Child]): List[TCChild] = {
    hhChildren map (child => {
      TCChild(
        id = child.id,
        childcareCost = child.childcareCost.flatMap(_.amount).getOrElse(BigDecimal(0)),
        childcareCostPeriod = convert(child.childcareCost.flatMap(_.period).getOrElse(PeriodEnum.MONTHLY)),
        dob = child.dob.get,
        disability = TCDisability(
          disabled = child.disability.exists(d => d.blind || d.disabled),
          severelyDisabled = child.disability.exists(_.severelyDisabled)
        ),
        education = Some(TCEducation(child.education.exists(_.inEducation),
          startDate = child.education.flatMap(x => x.startDate).getOrElse(LocalDate.now()))))
    })
  }
}
