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


import config.ApplicationConfig
import models._
import models.input.tfc._
import org.joda.time.LocalDate

object HHToTFCEligibilityInput {

  def convert(hh:Household):TFCEligibilityInput = {
    val stuff = TFCEligibilityInput(from = LocalDate.now(),
      numberOfPeriods = ApplicationConfig.tfcNoOfPeriods,
      location = hh.location.getOrElse(LocationEnum.ENGLAND.toString).toString,
      claimants = hhClaimantToTFCEligibilityInputClaimant(hh.parent, hh.partner),
      children =  hhChildToTFCEligibilityInputChild(hh.children)
    )
    println(stuff)
    stuff
  }

  private def hhClaimantToTFCEligibilityInputClaimant(hhParent: Claimant, hhPartner: Option[Claimant]): List[TFCClaimant] = {

    val parent: TFCClaimant = TFCClaimant(
                                          previousIncome = hhIncomeToTFCIncome(hhParent.lastYearlyIncome),
                                          currentIncome = hhIncomeToTFCIncome(hhParent.currentYearlyIncome),
                                          hoursPerWeek = hhParent.hours.getOrElse(BigDecimal(0.0)).doubleValue(),
                                          isPartner = false,
                                          disability = hhBenefitsToTFCDisability(hhParent.benefits),
                                          carersAllowance = hhParent.benefits.map(x => x.carersAllowance).getOrElse(false),
                                          minimumEarnings = hhMinimumEarningsToTFCMinimumEarnings(hhParent.minimumEarnings),
                                          age = hhParent.ageRange.map(x => x.toString),
                                          employmentStatus = hhParent.minimumEarnings.map(x => x.employmentStatus.toString),
                                          selfEmployedSelection = hhParent.minimumEarnings.flatMap(x => x.selfEmployedIn12Months)
    )

    hhPartner match {
      case Some(hhPartner) => List(parent, TFCClaimant(previousIncome = hhIncomeToTFCIncome(hhPartner.lastYearlyIncome),
                                                        currentIncome = hhIncomeToTFCIncome(hhPartner.currentYearlyIncome),
                                                        hoursPerWeek = hhPartner.hours.getOrElse(BigDecimal(0.0)).doubleValue(),
                                                        isPartner = true,
                                                        disability = hhBenefitsToTFCDisability(hhPartner.benefits),
                                                        carersAllowance = hhPartner.benefits.map(x => x.carersAllowance).getOrElse(false),
                                                        minimumEarnings = hhMinimumEarningsToTFCMinimumEarnings(hhPartner.minimumEarnings),
                                                        age = hhPartner.ageRange.map(x => x.toString),
                                                        employmentStatus = hhPartner.minimumEarnings.map(x => x.employmentStatus.toString),
                                                        selfEmployedSelection = hhPartner.minimumEarnings.flatMap(x => x.selfEmployedIn12Months)
                                                      )
      )
      case None => List(parent)
    }
  }

  private def hhMinimumEarningsToTFCMinimumEarnings(hhMinimumEarnings: Option[MinimumEarnings]): TFCMinimumEarnings = {
    hhMinimumEarnings match {
      case Some(hhMinimumEarnings) => TFCMinimumEarnings(
        selection = true, //True by default
        amount = hhMinimumEarnings.amount
      )
      case None =>TFCMinimumEarnings(
        selection = true, //True by default
        amount = BigDecimal(0.00))
    }
  }

  private def hhIncomeToTFCIncome(hhIncome: Option[Income]): Option[TFCIncome] = {
    hhIncome.map(x => TFCIncome(
      employmentIncome = x.employmentIncome,
      pension = x.pension,
      otherIncome = x.otherIncome,
      benefits = x.benefits)
    )
  }

  private def hhBenefitsToTFCDisability(hhBenefits: Option[Benefits]): TFCDisability = {
    if(hhBenefits.isDefined && hhBenefits.get.disabilityBenefits)
      {
        TFCDisability(true, true)
      }
    else
      {
        TFCDisability(false, false)
      }

  }
  private def hhChildToTFCEligibilityInputChild(hhChildren: List[Child]): List[TFCChild] = {
    hhChildren map (child => {
      TFCChild(
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
      disability = TFCDisability(
        disabled = child.disability.map(x => x.disabled).getOrElse(false) || child.disability.map(x => x.blind).getOrElse(false),
        severelyDisabled = child.disability.map(x => x.severelyDisabled).getOrElse(false)
      ))
    })
  }}
