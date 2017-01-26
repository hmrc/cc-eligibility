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

package utils

import java.text.SimpleDateFormat

import org.joda.time.LocalDate
import play.api.Play._
import play.api.{Configuration, Play}


case class TFCTaxYearConfig(
                             childAgeLimit: Int,
                             childAgeLimitDisabled: Int,
                             minimumHoursWorked: Double,
                             maxIncomePerClaimant: Double
                             )

object TFCConfig extends CCConfig {

  def getTFCConfigDefault(configs :Seq[play.api.Configuration]) : play.api.Configuration = {
    configs.filter(x => {
      x.getString("rule-date").equals(Some("default"))
    }).head
  }

  def getTFCConfigExcludingDefault(configs :Seq[play.api.Configuration]) : Seq[play.api.Configuration] = {
    configs.filter(x => {
      !x.getString("rule-date").equals(Some("default"))
    })
  }
  def getSortedTFCConfigExcludingDefault(configsExcludingDefault : Seq[play.api.Configuration]) : Seq[play.api.Configuration]= {
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

  def getTFCTaxYearConfig(configuration : play.api.Configuration) : TFCTaxYearConfig = {
    TFCTaxYearConfig(
      childAgeLimit = configuration.getInt("child-age-limit").get,
      childAgeLimitDisabled = configuration.getInt("child-age-limit-disabled").get,
      minimumHoursWorked = configuration.getDouble("minimum-hours-worked-per-week").get,
      maxIncomePerClaimant = configuration.getDouble("maximum-income-per-claimant").get
    )
  }

  def getConfig(currentDate: LocalDate): TFCTaxYearConfig = {
    val configs : Seq[play.api.Configuration] = Play.application.configuration.getConfigSeq("tfc.rule-change").get
    val configsExcludingDefault = getTFCConfigExcludingDefault(configs)
    val defaultConfig = getTFCConfigDefault(configs)
    // ensure the latest date is in the head position
    val sorted = getSortedTFCConfigExcludingDefault(configsExcludingDefault)

    val result = getConfigHelper(currentDate, sorted.toList, None, 0)

    val config : TFCTaxYearConfig = result match {
      case Some(x) =>
        getTFCTaxYearConfig(x)
      case _ =>
        getTFCTaxYearConfig(defaultConfig)
    }
    config
  }
}
