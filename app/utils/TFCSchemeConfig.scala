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
import play.api.Configuration
import scala.util.Try

case class TFCTaxYearConfig(
                             childAgeLimit: Int,
                             childAgeLimitDisabled: Int,
                             minimumHoursWorked: Double,
                             maxIncomePerClaimant: Double,
                             personalAllowancePerClaimant: Double,
                             nmwApprentice: Int,
                             nmwUnder18: Int,
                             nmw18To20: Int,
                             nmw21To24: Int,
                             nmw25Over: Int
                             )

trait TFCConfig {
  val minimumEarningsEnabled: Boolean
}

object TFCConfig extends TFCConfig with  CCConfig with LoadConfig {

  override val minimumEarningsEnabled: Boolean = Try(conf.getString("tfc-min-earnings").get.toBoolean).getOrElse(false)

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
      new SimpleDateFormat("dd-MM-yyyy").parse(c.getString("rule-date").get)
    }).reverse
  }

  def getConfigHelper (currentDate : LocalDate, taxYearConfigs : List[Configuration], acc : Option[Configuration], i : Int) : Option[Configuration] = {
    taxYearConfigs match {
      case Nil => acc
      case head :: tail =>
        val configDate = new SimpleDateFormat("dd-MM-yyyy").parse(head.getString("rule-date").get)

        // exit tail recursive
        if (currentDate.toDate.after(configDate) || currentDate.toDate.compareTo(configDate) == 0) {
          getConfigHelper(currentDate, Nil, Some(head), i)
        } else {
          getConfigHelper(currentDate, tail, acc, i)
        }
    }
  }

  def getTFCTaxYearConfig(configuration : play.api.Configuration, location: String) : TFCTaxYearConfig = {
    TFCTaxYearConfig(
      childAgeLimit = configuration.getInt("child-age-limit").get,
      childAgeLimitDisabled = configuration.getInt("child-age-limit-disabled").get,
      minimumHoursWorked = configuration.getDouble("minimum-hours-worked-per-week").get,
      maxIncomePerClaimant = configuration.getDouble("maximum-income-per-claimant").get,
      personalAllowancePerClaimant = configuration.getDouble({location} + ".personal-allowance").
        getOrElse(configuration.getDouble("default.personal-allowance").get),
      nmwApprentice = configuration.getInt("nmw.apprentice").get,
      nmwUnder18 = configuration.getInt("nmw.under-18").get,
      nmw18To20 = configuration.getInt("nmw.18-20").get,
      nmw21To24 = configuration.getInt("nmw.21-24").get,
      nmw25Over = configuration.getInt("nmw.over-25").get
    )
  }

  def getConfig(currentDate: LocalDate, location: String): TFCTaxYearConfig = {
    val configs: Seq[play.api.Configuration] = conf.getConfigSeq("tfc.rule-change").get
    val configsExcludingDefault = getTFCConfigExcludingDefault(configs)
    val defaultConfig = getTFCConfigDefault(configs)
    // ensure the latest date is in the head position
    val sorted = getSortedTFCConfigExcludingDefault(configsExcludingDefault)

    val result = getConfigHelper(currentDate, sorted.toList, None, 0)

    getTFCTaxYearConfig(result.getOrElse(defaultConfig), location)
  }
}
