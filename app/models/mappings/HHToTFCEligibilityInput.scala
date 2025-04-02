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

package models.mappings

import models.ParentsBenefits._
import models._
import models.input.tfc._
import utils.{CCConfig, TFCConfig}

import javax.inject.Inject

class HHToTFCEligibilityInput @Inject()(tFCConfig: TFCConfig, ccConfig: CCConfig) extends PeriodEnumToPeriod {

  def convert(hh: Household): TFCEligibilityInput =
    TFCEligibilityInput(
      from = tFCConfig.config.startDate,
      numberOfPeriods = tFCConfig.tfcNoOfPeriods,
      location = hh.location.getOrElse(LocationEnum.ENGLAND.toString).toString,
      claimants = hhClaimantToTFCEligibilityInputClaimant(hh.parent, hh.partner),
      children = hhChildToTFCEligibilityInputChild(hh.children)
    )

  private def hhClaimantToTFCEligibilityInputClaimant(hhParent: Claimant, hhPartner: Option[Claimant]): List[TFCClaimant] = {

    val parent: TFCClaimant = createClaimant(hhParent, isPartner = false)

    if (hhPartner.isDefined) {
      List(parent, createClaimant(hhPartner.get, isPartner = true))
    } else {
      List(parent)
    }
  }

  private def createClaimant(claimant: Claimant, isPartner: Boolean): TFCClaimant =
    TFCClaimant(
      currentIncome = hhIncomeToTFCIncome(claimant.currentYearlyIncome),
      isPartner = isPartner,
      disability = TFCDisability(),
      carersAllowance = convertBenefitsToCarersAllowance(claimant.benefits),
      minimumEarnings = hhMinimumEarningsToTFCMinimumEarnings(claimant.minimumEarnings),
      age = claimant.ageRange.map(_.toString),
      employmentStatus = claimant.minimumEarnings.flatMap(_.employmentStatus.map(_.toString)),
      selfEmployedSelection = claimant.minimumEarnings.flatMap(_.selfEmployedIn12Months),
      maximumEarnings = claimant.maximumEarnings
    )

  private def convertBenefitsToCarersAllowance(benefits: Option[Set[ParentsBenefits]]): Boolean = {
    val TfcOrFreeChildcareQualifyingBenefits: Set[ParentsBenefits] = Set(
      CarersAllowance,
      IncapacityBenefit,
      SevereDisablementAllowance,
      ContributionBasedEmploymentAndSupportAllowance
    )

    benefits.exists(_.intersect(TfcOrFreeChildcareQualifyingBenefits).nonEmpty)
  }

  private def hhMinimumEarningsToTFCMinimumEarnings(hhMinimumEarnings: Option[MinimumEarnings]): TFCMinimumEarnings =
    hhMinimumEarnings match {
      case Some(earnings) =>
        if (earnings.amount <= BigDecimal(0.00)) {
          TFCMinimumEarnings(selection = false, amount = BigDecimal(0.00))
        } else {
          TFCMinimumEarnings(amount = earnings.amount)
        }
      case None => TFCMinimumEarnings()
    }

  private def hhIncomeToTFCIncome(hhIncome: Option[Income]): Option[TFCIncome] =
    hhIncome.map(x => TFCIncome(
      employmentIncome = x.employmentIncome,
      pension = x.pension,
      otherIncome = x.otherIncome)
    )

  private def hhChildToTFCEligibilityInputChild(hhChildren: List[Child]): List[TFCChild] =
    hhChildren map (child => {
      TFCChild(
        child.id,
        child.childcareCost.flatMap(_.amount).getOrElse(BigDecimal(0)),
        convert(child.childcareCost.flatMap(_.period).getOrElse(PeriodEnum.MONTHLY)),
        child.dob.get,
        TFCDisability(
          disabled = child.disability.exists(d => d.blind || d.disabled),
          severelyDisabled = child.disability.exists(_.severelyDisabled)
        ), ccConfig, Some(true)
      )
    })

}
