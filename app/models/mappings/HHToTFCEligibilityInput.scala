/*
 * Copyright 2020 HM Revenue & Customs
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
import models.input.tfc._
import play.api.Logger
import utils.TFCConfig

class HHToTFCEligibilityInput @Inject()(tFCConfig: TFCConfig) extends PeriodEnumToPeriod {

  def convert(hh: Household): TFCEligibilityInput = {
    TFCEligibilityInput(
      from = tFCConfig.config.startDate,
      numberOfPeriods = tFCConfig.tfcNoOfPeriods,
      location = hh.location.getOrElse(LocationEnum.ENGLAND.toString).toString,
      claimants = hhClaimantToTFCEligibilityInputClaimant(hh.parent, hh.partner),
      children = hhChildToTFCEligibilityInputChild(hh.children)
    )
  }

  private def hhClaimantToTFCEligibilityInputClaimant(hhParent: Claimant, hhPartner: Option[Claimant]): List[TFCClaimant] = {

    val parent: TFCClaimant = createClaimant(hhParent, false)

    if (hhPartner.isDefined) {
      List(parent, createClaimant(hhPartner.get, true))
    } else {
      List(parent)
    }
  }

  private def createClaimant(claimant: Claimant, isPartner: Boolean): TFCClaimant = {
    TFCClaimant(
      previousIncome = hhIncomeToTFCIncome(claimant.lastYearlyIncome),
      currentIncome = hhIncomeToTFCIncome(claimant.currentYearlyIncome),
      hoursPerWeek = claimant.hours.getOrElse(BigDecimal(0.0)).doubleValue(),
      isPartner = isPartner,
      disability = TFCDisability(claimant.benefits.exists(_.disabilityBenefits), claimant.benefits.exists(_.highRateDisabilityBenefits)),
      carersAllowance = claimant.benefits.exists(_.carersAllowance),
      minimumEarnings = hhMinimumEarningsToTFCMinimumEarnings(claimant.minimumEarnings),
      age = claimant.ageRange.map(_.toString),
      employmentStatus = claimant.minimumEarnings.map(_.employmentStatus.toString),
      selfEmployedSelection = claimant.minimumEarnings.flatMap(_.selfEmployedIn12Months),
      maximumEarnings = claimant.maximumEarnings
    )
  }

  private def hhMinimumEarningsToTFCMinimumEarnings(hhMinimumEarnings: Option[MinimumEarnings]): TFCMinimumEarnings = {

    hhMinimumEarnings match {
      case Some(earnings) => {
        if (earnings.amount <= BigDecimal(0.00)) {
          TFCMinimumEarnings(selection = false, amount = BigDecimal(0.00))
        } else {
          TFCMinimumEarnings(amount = earnings.amount)
        }
      }
      case None => TFCMinimumEarnings() //default values will be used
    }
  }

  private def hhIncomeToTFCIncome(hhIncome: Option[Income]): Option[TFCIncome] = {
    hhIncome.map(x => TFCIncome(
      employmentIncome = x.employmentIncome,
      pension = x.pension,
      otherIncome = x.otherIncome)
    )
  }

  private def hhChildToTFCEligibilityInputChild(hhChildren: List[Child]): List[TFCChild] = {
    hhChildren map (child => {
      TFCChild(
        id = child.id,
        childcareCost = child.childcareCost.flatMap(_.amount).getOrElse(BigDecimal(0)),
        childcareCostPeriod = convert(child.childcareCost.flatMap(_.period).getOrElse(PeriodEnum.MONTHLY)),
        dob = child.dob.get,
        disability = TFCDisability(
          disabled = child.disability.exists(d => d.blind || d.disabled),
          severelyDisabled = child.disability.exists(_.severelyDisabled)
        )
      )
    })
  }
}
