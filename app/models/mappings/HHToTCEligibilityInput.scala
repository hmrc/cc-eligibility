/*
 * Copyright 2022 HM Revenue & Customs
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

import javax.inject.Inject
import models._
import models.input.tc._
import org.joda.time.LocalDate
import utils.{CCConfig, HelperManager, TCConfig}

class HHToTCEligibilityInput @Inject()(val cCConfig: CCConfig, tcConfig: TCConfig) extends PeriodEnumToPeriod with HelperManager {

  def convert(household: Household): TCEligibilityInput = {
    TCEligibilityInput(taxYears = createTaxYears(household.parent, household.partner, household.children))
  }

  private def buildIncome(parentIncome: Option[Income], partnerIncome: Option[Income]): TCIncome = {
    val employmentIncome = List(parentIncome.flatMap(_.employmentIncome), partnerIncome.flatMap(_.employmentIncome)).flatten
    val pensionIncome = List(parentIncome.flatMap(_.pension), partnerIncome.flatMap(_.pension)).flatten
    val otherIncome = List(parentIncome.flatMap(_.otherIncome), partnerIncome.flatMap(_.otherIncome)).flatten
    val benefitsIncome = List(parentIncome.flatMap(_.benefits), partnerIncome.flatMap(_.benefits)).flatten

    val parentStatutory = parentIncome.flatMap(_.statutoryIncome.map(si => TCStatutoryIncome(si.statutoryWeeks, si.statutoryAmount)))
    val partnerStatutory = partnerIncome.flatMap(_.statutoryIncome.map(si => TCStatutoryIncome(si.statutoryWeeks, si.statutoryAmount)))

    val statutoryIncome = List(parentStatutory, partnerStatutory).flatten

    TCIncome(
      employment = if(employmentIncome.isEmpty) None else Some(employmentIncome),
      pension = if(pensionIncome.isEmpty) None else Some(pensionIncome),
      other = if(otherIncome.isEmpty) None else Some(otherIncome),
      benefits = if(benefitsIncome.isEmpty) None else Some(benefitsIncome),
      statutory = if(statutoryIncome.isEmpty) None else Some(statutoryIncome)
    )
  }

  private def createTaxYears(
                              parent: Claimant,
                              partner: Option[Claimant],
                              children: List[Child]
                            ): List[TCTaxYear] = {

    val now = cCConfig.startDate
    val april6thCurrentYear = determineApril6DateFromNow(now)
    val claimantList = hhClaimantToTCEligibilityInputClaimant(parent, partner)
    val childList = hhChildToTEligibilityInputChild(children)

    val lastYearIncome: TCIncome = buildIncome(parent.lastYearlyIncome, partner.flatMap(_.lastYearlyIncome))
    val currentYearIncome: TCIncome = buildIncome(parent.currentYearlyIncome, partner.flatMap(_.currentYearlyIncome))

    List(
      TCTaxYear(
        from = now,
        until = april6thCurrentYear,
        previousHouseholdIncome = Some(lastYearIncome),
        currentHouseholdIncome = Some(currentYearIncome),
        claimants = claimantList,
        children = childList
      ),
      TCTaxYear(
        from = april6thCurrentYear,
        until = now.plusYears(1),
        previousHouseholdIncome = Some(currentYearIncome),
        currentHouseholdIncome = Some(currentYearIncome),
        claimants = claimantList,
        children = childList
      )
    )
  }

  private def createClaimant(claimant: Claimant, isPartner: Boolean): TCClaimant = {
    TCClaimant(
      hoursPerWeek = claimant.hours.getOrElse(BigDecimal(0.0)).doubleValue(),
      isPartner = isPartner,
      disability = TCDisability(claimant.benefits.exists(_.disabilityBenefits), claimant.benefits.exists(_.highRateDisabilityBenefits)),
      carersAllowance = claimant.benefits.exists(_.carersAllowance),
      incomeBenefits = claimant.benefits.fold(false)(c=>c.incomeBenefits)
    )(Some(tcConfig))

  }
  private def hhClaimantToTCEligibilityInputClaimant(hhParent: Claimant, hhPartner: Option[Claimant]): List[TCClaimant] = {

    val parent: TCClaimant = createClaimant(hhParent, false)
    if(hhPartner.isDefined) {
      List(parent, createClaimant(hhPartner.get, true))
    } else {
      List(parent)
    }
  }

  private def hhChildToTEligibilityInputChild(hhChildren: List[Child]): List[TCChild] = {

    hhChildren map (child => {
      TCChild(
        child.id,
        child.childcareCost.flatMap(_.amount).getOrElse(BigDecimal(0)),
        convert(child.childcareCost.flatMap(_.period).getOrElse(PeriodEnum.MONTHLY)),
        child.dob.get,
        TCDisability(
          disabled = child.disability.exists(d => d.blind || d.disabled),
          severelyDisabled = child.disability.exists(_.severelyDisabled)
        ),
        Some(
          TCEducation(
            child.education.exists(_.inEducation),
            startDate = child.education.flatMap(x => x.startDate).getOrElse(LocalDate.now())
          )
        )
      ,None, None, Some(true))
    })
  }
}
