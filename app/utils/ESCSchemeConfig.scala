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

import javax.inject.Inject
import org.joda.time.LocalDate
import play.api.Configuration

case class ESCTaxYearConfig(
                             childAgeLimit: Int,
                             childAgeLimitDisabled: Int
                             )

class ESCConfig @Inject()(val config: CCConfig) {

  def getESCConfigDefault(configs :Seq[play.api.Configuration]) : play.api.Configuration = {
    configs.filter(_.get[String]("rule-date").contains("default")).head
  }

  def getESCConfigExcludingDefault(configs :Seq[play.api.Configuration]) : Seq[play.api.Configuration] = {
    configs.filter(!_.get[String]("rule-date").contains("default"))
  }

  def getSortedESCConfigExcludingDefault(configsExcludingDefault : Seq[play.api.Configuration]) : Seq[play.api.Configuration] = {
    configsExcludingDefault.sortBy(c => {
      new SimpleDateFormat("dd-MM-yyyy").parse(c.get[String]("rule-date"))
    }).reverse
  }

  def getConfigHelper (currentDate : LocalDate, taxYearConfigs : List[Configuration], acc : Option[Configuration], i : Int) : Option[Configuration] = {
    taxYearConfigs match {
      case Nil => acc
      case head :: tail =>
        val configDate = new SimpleDateFormat("dd-MM-yyyy").parse(head.get[String]("rule-date"))

        // exit tail recursive
        if (currentDate.toDate.after(configDate) || currentDate.toDate.compareTo(configDate) == 0) {
          getConfigHelper(currentDate, Nil, Some(head), i)
        } else {
          getConfigHelper(currentDate, tail, acc, i)
        }
    }
  }

  def getESCTaxYearConfig(configuration : play.api.Configuration) : ESCTaxYearConfig = {
    ESCTaxYearConfig(
      childAgeLimit = configuration.get[Int]("child-age-limit"),
      childAgeLimitDisabled = configuration.get[Int]("child-age-limit-disabled")
    )
  }

  def getConfig(currentDate: LocalDate): ESCTaxYearConfig = {
    val configs : Seq[play.api.Configuration] = config.oldConf.getConfigSeq("esc.rule-change").get
    val configsExcludingDefault = getESCConfigExcludingDefault(configs)
    val defaultConfig = getESCConfigDefault(configs)
    // ensure the latest date is in the head position
    val sorted = getSortedESCConfigExcludingDefault(configsExcludingDefault)

    val result = getConfigHelper(currentDate, sorted.toList, None, 0)

    result match {
      case Some(x) =>
        getESCTaxYearConfig(x)
      case _ =>
        getESCTaxYearConfig(defaultConfig)
    }
  }
}
