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
import utils.CCConfig

object HHToTCEligibilityInput extends HHToTCEligibilityInput {
  override val cCConfig = CCConfig
}

trait HHToTCEligibilityInput {

  val cCConfig: CCConfig

  def convert(household:Household):TCEligibilityInput = {
    TCEligibilityInput(taxYears = createTaxYears(household.hasPartner, household.parent, household.partner, household.children)    )
  }

  private def createTaxYears(
                              hasPartner: Boolean,
                              parent: Claimant,
                              partner: Option[Claimant],
                              children: List[Child]
                            ): List[TCTaxYear] = {

    val now = cCConfig.StartDate
    val april6thCurrentYear = determineApril6DateFromNow(now)
    val claimantList = hhClaimantToTCEligibilityInputClaimant(parent, partner)
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

  //this code is duplicated in HHToESCEligibilityInput and should be refactored out
  private def determineApril6DateFromNow(from: LocalDate): LocalDate = {
    val periodYear = from.getYear
    val january1st = LocalDate.parse(s"${periodYear}-01-01")
    val april6CurrentYear = LocalDate.parse(s"${periodYear}-04-06")

    if ((from.compareTo(january1st) == 0 || (from.isAfter(january1st)) && from.isBefore(april6CurrentYear))) {
      april6CurrentYear
    } else {
      april6CurrentYear.plusYears(1)
    }
  }

  private def hhClaimantToTCEligibilityInputClaimant(hhParent: Claimant, hhPartner: Option[Claimant]): List[TCClaimant] = {

    val parent: TCClaimant = TCClaimant(
                                          hours = hhParent.hours.getOrElse(BigDecimal(0.0)).doubleValue(),
                                          isPartner  = false,
                                          disability = hhBenefitsToTCDisability(hhParent.benefits),
                                          carersAllowance = hhParent.benefits.map(x => x.carersAllowance).getOrElse(false)
    )

    hhPartner match {
      case Some(hhPartner) => List(parent, TCClaimant(
                                                      hours = hhPartner.hours.getOrElse(BigDecimal(0.0)).doubleValue(),
                                                      isPartner  = true,
                                                      disability = hhBenefitsToTCDisability(hhPartner.benefits),
                                                      carersAllowance = hhPartner.benefits.map(x => x.carersAllowance).getOrElse(false)
                                                    )
      )
      case None => List(parent)
    }
  }

  private def hhBenefitsToTCDisability(hhBenefits: Option[Benefits]): TCDisability = {
    if(hhBenefits.isDefined && hhBenefits.get.disabilityBenefits)
      {
        TCDisability(disabled = true, severelyDisabled = true)
      }
    else
      {
        TCDisability(disabled = false, severelyDisabled = false)
      }
  }

  private def hhChildToTEligibilityInputChild(hhChildren: List[Child]): List[TCChild] = {
    hhChildren map (child => {
      TCChild(
        id = child.id,
        childcareCost = child.childcareCost match {
        case Some(childcareCost) => childcareCost.amount.getOrElse(BigDecimal(0.00))
        case None => BigDecimal(0.00)
      },
        childcareCostPeriod = PeriodEnumToPeriod.convert(child.childcareCost match {
        case Some(childcareCost) => childcareCost.period.getOrElse(PeriodEnum.INVALID)
        case None => PeriodEnum.INVALID
      }),
      dob = child.dob.get,
      disability = TCDisability(
        disabled = child.disability.map(x => x.disabled).getOrElse(false) || child.disability.map(x => x.blind).getOrElse(false),
        severelyDisabled = child.disability.map(x => x.severelyDisabled).getOrElse(false)
      ),
        education = Some(TCEducation(child.education.map(x => x.inEducation).getOrElse(false),
          startDate = child.education.flatMap(x => x.startDate).getOrElse(LocalDate.now()))))
    })
  }
}
