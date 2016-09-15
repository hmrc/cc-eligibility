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

case class ESCTaxYearConfig(
                             childAgeLimit: Int,
                             childAgeLimitDisabled: Int
                             )

object ESCConfig extends CCConfig {

  def getESCConfigDefault(configs :Seq[play.api.Configuration]) : play.api.Configuration = {
    Logger.debug(s"ESCConfig.getESCConfigDefault")
    configs.filter(x => {
      x.getString("rule-date").equals(Some("default"))
    }).head
  }

  def getESCConfigExcludingDefault(configs :Seq[play.api.Configuration]) : Seq[play.api.Configuration] = {
    Logger.debug(s"ESCConfig.getESCConfigExcludingDefault")
    configs.filter(x => {
      !x.getString("rule-date").equals(Some("default"))
    })
  }
  def getSortedESCConfigExcludingDefault(configsExcludingDefault : Seq[play.api.Configuration]) = {
    Logger.debug(s"ESCConfig.getSortedESCConfigExcludingDefault")
    configsExcludingDefault.sortBy(c => {
      val predicate = new SimpleDateFormat("dd-mm-yyyy").parse(c.getString("rule-date").get)
      predicate
      //c
    }).reverse
  }

  def getConfigHelper (currentDate : LocalDate, taxYearConfigs : List[Configuration], acc : Option[Configuration], i : Int) : Option[Configuration] = {
    Logger.debug(s"ESCConfig.getConfigHelper")
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

  def getESCTaxYearConfig(configuration : play.api.Configuration) = {
    Logger.debug(s"ESCConfig.getESCTaxYearConfig")
    ESCTaxYearConfig(
      childAgeLimit = configuration.getInt("child-age-limit").get,
      childAgeLimitDisabled = configuration.getInt("child-age-limit-disabled").get
    )
  }

  def getConfig(currentDate: LocalDate): ESCTaxYearConfig = {
    Logger.debug(s"ESCConfig.getConfig")
    val configs : Seq[play.api.Configuration] = Play.application.configuration.getConfigSeq("esc.rule-change").get
    val configsExcludingDefault = getESCConfigExcludingDefault(configs)
    val defaultConfig = getESCConfigDefault(configs)
    // ensure the latest date is in the head position
    val sorted = getSortedESCConfigExcludingDefault(configsExcludingDefault)

    val result = getConfigHelper(currentDate, sorted.toList, None, 0)

    val config : ESCTaxYearConfig = result match {
      case Some(x) =>
        getESCTaxYearConfig(x)
      case _ =>
        getESCTaxYearConfig(defaultConfig)
    }
    config
  }
}
