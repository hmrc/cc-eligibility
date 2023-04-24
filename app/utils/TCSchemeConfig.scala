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

package utils

import java.text.SimpleDateFormat

import javax.inject.Inject
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.Configuration
import scala.collection.JavaConverters.asScalaBufferConverter

case class TCTaxYearConfig(
                             childAgeLimit: Int,
                             childAgeLimitDisabled: Int,
                             childAgeLimitEducation: Int,
                             youngAdultAgeLimit: Int,
                             minimumHoursWorked: Double,
                             minimumHoursWorkedIfCouple: Double,
                             hours30Worked: Double,
                             currentIncomeFallDifferenceAmount: Int,
                             currentIncomeRiseDifferenceAmount: Int
                             )

class TCConfig @Inject()(val config: CCConfig) {

  lazy val childElementLimit: Int = config.conf.getInt("tc.child-element-limit")
  lazy val childDate6thApril2017: LocalDate = DateTimeFormat.forPattern("dd-MM-yyyy").parseLocalDate(config.conf.getString("tc.child-element-date-constraint"))

  def getTCConfigDefault(configs: Seq[Configuration]): Configuration = {
    configs.filter(_.get[String]("rule-date").contains("default")).head
  }

  def getTCConfigExcludingDefault(configs: Seq[Configuration]): Seq[Configuration] = {
    configs.filter(!_.get[String]("rule-date").contains("default"))
  }
  def getSortedTCConfigExcludingDefault(configsExcludingDefault: Seq[Configuration]): Seq[Configuration] = {
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

  def getTCTaxYearConfig(configuration: Configuration): TCTaxYearConfig = {
    val defaultCurrentIncomeRiseDifferenceAmount = 2500
    val defaultCurrentIncomeFallDifferenceAmount = 2500
    val defaultHours30Worked = 30.00
    val defaultMinimumHoursWorkedIfCouple = 24.00
    val defaultMinimumHoursWorked = 16.00
    val defaultYoungAdultAgeLimit = 20
    val defaultChildLimitEducation = 19
    val defaultChildAgeLimitDisabled = 16
    val defaultChildAgeLimit = 15

    TCTaxYearConfig(
      childAgeLimit = configuration.getOptional[Int]("child-age-limit").getOrElse(defaultChildAgeLimit),
      childAgeLimitDisabled = configuration.getOptional[Int]("child-age-limit-disabled").getOrElse(defaultChildAgeLimitDisabled),
      childAgeLimitEducation = configuration.getOptional[Int]("young-adult-education-age-limit").
        getOrElse(defaultChildLimitEducation),
      youngAdultAgeLimit = configuration.getOptional[Int]("young-adult-age-limit").
        getOrElse(defaultYoungAdultAgeLimit),
      minimumHoursWorked = configuration.getOptional[Double]("minimum-hours-worked-per-week")
        .getOrElse(defaultMinimumHoursWorked),
      minimumHoursWorkedIfCouple = configuration.getOptional[Double]("minimum-hours-worked-if-couple-per-week")
        .getOrElse(defaultMinimumHoursWorkedIfCouple),
      hours30Worked = configuration.getOptional[Double]("hours-30-worked-per-week").getOrElse(defaultHours30Worked),
      currentIncomeFallDifferenceAmount = configuration.getOptional[Int]("current-income-fall-difference-amount").
        getOrElse(defaultCurrentIncomeFallDifferenceAmount),
      currentIncomeRiseDifferenceAmount = configuration.getOptional[Int]("current-income-rise-difference-amount").
        getOrElse(defaultCurrentIncomeRiseDifferenceAmount)
    )
  }

  def getConfig(currentDate: LocalDate): TCTaxYearConfig = {
    val configs: Seq[Configuration] = config.oldConf.underlying.getConfigList("tc.rule-change").asScala.map(Configuration(_))
    val configsExcludingDefault = getTCConfigExcludingDefault(configs)
    val defaultConfig = getTCConfigDefault(configs)
    // ensure the latest date is in the head position
    val sorted = getSortedTCConfigExcludingDefault(configsExcludingDefault)

    val result = getConfigHelper(currentDate, sorted.toList, None, 0)

    result match {
      case Some(x) =>
        getTCTaxYearConfig(x)
      case _ =>
        getTCTaxYearConfig(defaultConfig)
    }
  }
}
