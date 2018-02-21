/*
 * Copyright 2018 HM Revenue & Customs
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
import models.input.esc._
import utils.{CCConfig, HelperManager}

object HHToESCEligibilityInput extends HHToESCEligibilityInput {
  override val cCConfig = CCConfig
}

trait HHToESCEligibilityInput extends PeriodEnumToPeriod with HelperManager {

  val cCConfig: CCConfig

  def convert(household: Household): ESCEligibilityInput = {
    ESCEligibilityInput(createTaxYears(household.parent, household.partner, household.children),
      household.location)
  }

  private def createTaxYears(
                              parent: Claimant,
                              partner: Option[Claimant],
                              children: List[Child]
                            ): List[ESCTaxYear] = {

    val now = cCConfig.StartDate
    val april6thCurrentYear = determineApril6DateFromNow(now)
    val claimantList = createClaimants(parent, partner)
    val childList = createChildren(children)

    List(
      ESCTaxYear(
        from = now,
        until = april6thCurrentYear,
        claimants = claimantList,
        children = childList
      ),
      ESCTaxYear(
        from = april6thCurrentYear,
        until = now.plusYears(1),
        claimants = claimantList,
        children = childList
      )
    )
  }

  private def createClaimants(parent: Claimant,
                              partner: Option[Claimant]): List[ESCClaimant] = {

    parent.lastYearlyIncome.map(x => ESCIncome(x.employmentIncome, x.pension, x.taxCode))
    val newParent = ESCClaimant(
      employerProvidesESC = escVouchersAvailable(parent),
      previousIncome = parent.lastYearlyIncome.map(x => ESCIncome(x.employmentIncome, x.pension, x.taxCode)),
      currentIncome = parent.currentYearlyIncome.map(x => ESCIncome(x.employmentIncome, x.pension, x.taxCode))
    )

    partner.fold(List(newParent)) {
      p =>
        List(newParent, ESCClaimant(isPartner = true,
          employerProvidesESC = escVouchersAvailable(partner.get),
          previousIncome = p.lastYearlyIncome.map(x => ESCIncome(x.employmentIncome, x.pension, x.taxCode)),
          currentIncome = p.currentYearlyIncome.map(x => ESCIncome(x.employmentIncome, x.pension, x.taxCode))))
    }

  }

  private def escVouchersAvailable(claimant: Claimant): Boolean = {
    claimant.escVouchers match {
      case Some(YesNoUnsureEnum.YES) => true
      case _ => false
    }
  }

  private def createChildren(children: List[Child]): List[ESCChild] = {
    for (child <- children) yield {
      ESCChild(
        id = child.id,
        dob = child.dob.get,
        childCareCost = child.childcareCost.flatMap(_.amount).getOrElse(BigDecimal(0)),
        childCareCostPeriod = convert(child.childcareCost.flatMap(_.period).getOrElse(PeriodEnum.MONTHLY)),
        disability = ESCDisability(
          disabled = child.disability.exists(d => d.blind || d.disabled),
          severelyDisabled = child.disability.exists(_.severelyDisabled)
        )
      )
    }
  }

}
