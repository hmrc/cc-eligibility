/*
 * Copyright 2022 HM Revenue & Customs
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
import java.util.Calendar

import javax.inject.Inject
import org.joda.time.LocalDate
import play.api.Configuration
import scala.collection.JavaConverters.asScalaBufferConverter
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

class CCConfig @Inject()(val conf: ServicesConfig,
                         val oldConf: Configuration) {

  val dateFormat = new SimpleDateFormat("dd-MM-yyyy")

  def startDate: LocalDate = LocalDate.now()


  private def calendar(year: Int, month: Int, day: Int): Calendar = {
    val calendar  = Calendar.getInstance()

    calendar.clear()
    calendar.set(Calendar.MONTH, month)
    calendar.set(Calendar.DAY_OF_MONTH, day)
    calendar.set(Calendar.YEAR, year)

    calendar
  }

  private def birthDayCalendar(date: LocalDate): Calendar = {
    val calendar  = Calendar.getInstance()
    calendar.clear()
    calendar.setTime(date.toDate)

    calendar
  }

  def september1stForDate(date: LocalDate) : LocalDate = {
    val currentYear = determineTaxYearFromNow(date)

     val september1 = calendar(currentYear, Calendar.SEPTEMBER, 1).getTime
    LocalDate.fromDateFields(september1)
  }

  def previousSeptember1stForDate(date: LocalDate) : LocalDate = {
    val currentYear = determineTaxYearFromNow(date)

    val september1 = calendar(currentYear-1, Calendar.SEPTEMBER, 1).getTime
    LocalDate.fromDateFields(september1)
  }

  def september1stFollowingChildBirthday(childBirthday: LocalDate) : LocalDate = {
    // plot the child's birthday (e.g. 16th birthday) on the calendar
    val childBirthdayCalendar = birthDayCalendar(childBirthday)

    // determine 1st september for the child's birthday (current year)
    // if their birthday is after september then we have to go to the following year
    val septemberCalendar = birthDayCalendar(childBirthday)
    septemberCalendar.set(Calendar.MONTH, Calendar.SEPTEMBER)
    septemberCalendar.set(Calendar.DAY_OF_MONTH, 1)

    // if 16th birthday is after the determined 1st september then we need to add a year to the following september
    if (childBirthdayCalendar.compareTo(septemberCalendar) > 0 || childBirthdayCalendar.compareTo(septemberCalendar) == 0) {
      septemberCalendar.add(Calendar.YEAR, 1)
    }

    val september1 = septemberCalendar.getTime
    LocalDate.fromDateFields(september1)
  }

  def determineTaxYearFromNow(from: LocalDate) : Int = {

    val currentCalendar = birthDayCalendar(from)

    val periodYear = currentCalendar.get(Calendar.YEAR)
    val periodStart = from.toDate

    val january1st = calendar(periodYear, Calendar.JANUARY, 1).getTime

    val april5th = calendar(periodYear, Calendar.APRIL, 5).getTime

    val taxYear = if ((periodStart.compareTo(january1st) == 0 || periodStart.after(january1st))
      && (periodStart.before(april5th) || periodStart.compareTo(april5th) == 0)) {
      periodYear-1
    } else {
      periodYear
    }
    taxYear
  }

  def loadConfigByType(configType: String, currentDate: LocalDate = LocalDate.now): Configuration = {
    val configs: Seq[Configuration] = oldConf.underlying.getConfigList(configType).asScala.map(Configuration(_))
    val configExcludingDefault: Seq[Configuration] = getConfigExcludingDefault(configs)
    configExcludingDefault.find(conf => {
      val ruleDate = dateFormat.parse(conf.get[String]("rule-date"))
      currentDate.toDate.compareTo(ruleDate) >= 0
    }).getOrElse(getConfigDefault(configs))
  }

  private def getConfigDefault(configs: Seq[Configuration]): Configuration = {
    configs.filter(_.get[String]("rule-date").contains("default")).head
  }

  private def getConfigExcludingDefault(configs: Seq[Configuration]): Seq[Configuration] = {
    configs.filterNot(_.get[String]("rule-date").contains("default")).sortWith(
      (conf1, conf2) => dateFormat.parse(conf1.get[String]("rule-date")).after(dateFormat.parse(conf2.get[String]("rule-date")))
    )
  }
}
