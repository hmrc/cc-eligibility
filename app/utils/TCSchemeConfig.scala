/*
 * Copyright 2016 HM Revenue & Customs
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

import java.text.SimpleDateFormat

import org.joda.time.LocalDate
import play.api.{Logger, Play, Configuration}
import play.api.Play._

case class TCTaxYearConfig(
                             childAgeLimit: Int,
                             childAgeLimitDisabled: Int,
                             childAgeLimitEducation : Int,
                             youngAdultAgeLimit : Int,
                             minimumHoursWorked : Double,
                             minimumHoursWorkedIfCouple : Double,
                             hours30Worked : Double,
                             currentIncomeFallDifferenceAmount : Int,
                             currentIncomeRiseDifferenceAmount : Int
                             )


object TCConfig extends CCConfig {

  def getTCConfigDefault(configs :Seq[play.api.Configuration]) : play.api.Configuration = {
    configs.filter(x => {
      x.getString("rule-date").equals(Some("default"))
    }).head
  }

  def getTCConfigExcludingDefault(configs :Seq[play.api.Configuration]) : Seq[play.api.Configuration] = {
    configs.filter(x => {
      !x.getString("rule-date").equals(Some("default"))
    })
  }
  def getSortedTCConfigExcludingDefault(configsExcludingDefault : Seq[play.api.Configuration]) : Seq[Configuration] = {
    configsExcludingDefault.sortBy(c => {
      new SimpleDateFormat("dd-mm-yyyy").parse(c.getString("rule-date").get)
    }).reverse
  }

  def getConfigHelper (currentDate : LocalDate, taxYearConfigs : List[Configuration], acc : Option[Configuration], i : Int) : Option[Configuration] = {
    taxYearConfigs match {
      case Nil => acc
      case head :: tail =>
        val configDate = new SimpleDateFormat("dd-mm-yyyy").parse(head.getString("rule-date").get)

        // exit tail recursive
        if (currentDate.toDate.after(configDate) || currentDate.toDate.compareTo(configDate) == 0) {
          getConfigHelper(currentDate, Nil, Some(head), i)
        } else {
          getConfigHelper(currentDate, tail, acc, i)
        }
    }
  }

  def getTCTaxYearConfig(configuration : play.api.Configuration) : TCTaxYearConfig = {
    TCTaxYearConfig(
      childAgeLimit = configuration.getInt("child-age-limit").getOrElse(15),
      childAgeLimitDisabled = configuration.getInt("child-age-limit-disabled").getOrElse(16),
      childAgeLimitEducation = configuration.getInt("young-adult-education-age-limit").getOrElse(19),
      youngAdultAgeLimit = configuration.getInt("young-adult-age-limit").getOrElse(20),
      minimumHoursWorked = configuration.getDouble("minimum-hours-worked-per-week").getOrElse(16.00),
      minimumHoursWorkedIfCouple = configuration.getDouble("minimum-hours-worked-if-couple-per-week").getOrElse(24.00),
      hours30Worked = configuration.getDouble("hours-30-worked-per-week").getOrElse(30.00),
      currentIncomeFallDifferenceAmount = configuration.getInt("current-income-fall-difference-amount").getOrElse(2500),
      currentIncomeRiseDifferenceAmount = configuration.getInt("current-income-rise-difference-amount").getOrElse(2500)
    )
  }

  def getConfig(currentDate: LocalDate): TCTaxYearConfig = {
    val configs : Seq[play.api.Configuration] = Play.application.configuration.getConfigSeq("tc.rule-change").get
    val configsExcludingDefault = getTCConfigExcludingDefault(configs)
    val defaultConfig = getTCConfigDefault(configs)
    // ensure the latest date is in the head position
    val sorted = getSortedTCConfigExcludingDefault(configsExcludingDefault)

    val result = getConfigHelper(currentDate, sorted.toList, None, 0)

    val config : TCTaxYearConfig = result match {
      case Some(x) =>
        getTCTaxYearConfig(x)
      case _ =>
        getTCTaxYearConfig(defaultConfig)
    }
    config
  }
}
