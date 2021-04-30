/*
 * Copyright 2021 HM Revenue & Customs
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

import javax.inject.{Inject, Singleton}
import org.joda.time.LocalDate
import play.api.Configuration
import scala.collection.JavaConverters.asScalaBufferConverter

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

@Singleton
class TFCConfig @Inject()(val config: CCConfig) {

  def getTFCConfigDefault(configs: Seq[Configuration]): Configuration = {
    configs.filter(_.get[String]("rule-date").contains("default")).head
  }

  def getTFCConfigExcludingDefault(configs: Seq[Configuration]): Seq[Configuration] = {
    configs.filter(!_.get[String]("rule-date").contains("default"))
  }
  def getSortedTFCConfigExcludingDefault(configsExcludingDefault: Seq[Configuration]): Seq[Configuration]= {
    configsExcludingDefault.sortBy(c => {
      new SimpleDateFormat("dd-MM-yyyy").parse(c.get[String]("rule-date"))
    }).reverse
  }

  def getConfigHelper (currentDate: LocalDate, taxYearConfigs: List[Configuration], acc: Option[Configuration], i: Int): Option[Configuration] = {
    taxYearConfigs match {
      case Nil => acc
      case head:: tail =>
        val configDate = new SimpleDateFormat("dd-MM-yyyy").parse(head.get[String]("rule-date"))

        // exit tail recursive
        if (currentDate.toDate.after(configDate) || currentDate.toDate.compareTo(configDate) == 0) {
          getConfigHelper(currentDate, Nil, Some(head), i)
        } else {
          getConfigHelper(currentDate, tail, acc, i)
        }
    }
  }

  def getTFCTaxYearConfig(configuration: Configuration, location: String): TFCTaxYearConfig = {
    TFCTaxYearConfig(
      childAgeLimit = configuration.get[Int]("child-age-limit"),
      childAgeLimitDisabled = configuration.get[Int]("child-age-limit-disabled"),
      minimumHoursWorked = configuration.get[Double]("minimum-hours-worked-per-week"),
      maxIncomePerClaimant = configuration.get[Double]("maximum-income-per-claimant"),
      personalAllowancePerClaimant = configuration.getOptional[Double]({location} + ".personal-allowance").
        getOrElse(configuration.get[Double]("default.personal-allowance")),
      nmwApprentice = configuration.get[Int]("nmw.apprentice"),
      nmwUnder18 = configuration.get[Int]("nmw.under-18"),
      nmw18To20 = configuration.get[Int]("nmw.18-20"),
      nmw21To24 = configuration.get[Int]("nmw.21-24"),
      nmw25Over = configuration.get[Int]("nmw.over-25")
    )
  }

  def tfcNoOfPeriods: Short = config.oldConf.getOptional[Int]("tax.quarters.multiplier").getOrElse(4).toShort

  def getConfig(currentDate: LocalDate, location: String): TFCTaxYearConfig = {
    val configs: Seq[Configuration] = config.oldConf.underlying.getConfigList("tfc.rule-change").asScala.map(Configuration(_))
    val configsExcludingDefault = getTFCConfigExcludingDefault(configs)
    val defaultConfig = getTFCConfigDefault(configs)
    // ensure the latest date is in the head position
    val sorted = getSortedTFCConfigExcludingDefault(configsExcludingDefault)

    val result = getConfigHelper(currentDate, sorted.toList, None, 0)

    getTFCTaxYearConfig(result.getOrElse(defaultConfig), location)
  }
}
