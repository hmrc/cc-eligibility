/*
 * Copyright 2019 HM Revenue & Customs
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

package controllers

import akka.stream.Materializer
import models.ChildCareCost
import models.input.tfc._
import org.joda.time.LocalDate
import org.mockito.Mockito.when
import org.scalatest.Suite
import org.scalatest.mockito.MockitoSugar
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.{AnyContent, ControllerComponents, DefaultMessagesActionBuilderImpl, PlayBodyParsers}
import play.api.test.Helpers.{stubBodyParser, stubMessagesApi}
import service.AuditEvents
import uk.gov.hmrc.http.HeaderCarrier
import utils.{CCConfigSpec, Periods, TFCConfig}

import scala.concurrent.ExecutionContext.Implicits.global


trait FakeCCEligibilityApplication extends CCConfigSpec with MockitoSugar {
  this: Suite =>

  val config: Map[String, _] = Map(
    "csrf.sign.tokens" -> false,
    "govuk-tax.Test.services.contact-frontend.host" -> "localhost",
    "govuk-tax.Test.services.contact-frontend.port" -> "9250",
    "microservice.services.cc-calculator.host" -> "localhost",
    "microservice.services.cc-calculator.port" -> "9372",
    "microservice.services.cc-calculator.url" -> "/cc-calculator/calculate",
    "tfc-rollout.0.rule-date" -> "default",
    "tfc-rollout.0.all-disabled" -> true,
    "tfc-rollout.0.born-on-after" -> "01-09-2013",
    "free-hours.0.rule-date" -> "default",
    "free-hours.0.fifteen.england" -> "2,3,4",
    "free-hours.0.fifteen.scotland" -> "2,3,4",
    "free-hours.0.fifteen.northern-ireland" -> "3",
    "free-hours.0.fifteen.wales" -> "2,3",
    "free-hours.0.thirty.england" -> "3,4"
  )

  override lazy val app: Application =
    new GuiceApplicationBuilder()
      .disable[com.kenshoo.play.metrics.PlayModule]
      .configure(config)
      .build()

  implicit lazy val mat: Materializer = app.materializer
  implicit val hc: HeaderCarrier = HeaderCarrier()

  val mockCC: ControllerComponents = mock[ControllerComponents]
  val mockParser: PlayBodyParsers = mock[PlayBodyParsers]

  when(mockCC.actionBuilder)
    .thenReturn(new DefaultMessagesActionBuilderImpl(stubBodyParser[AnyContent](), stubMessagesApi()))
  when(mockCC.parsers)
    .thenReturn(mockParser)

  lazy val tfcConfig: TFCConfig = app.injector.instanceOf[TFCConfig]
  lazy val audits: AuditEvents = mock[AuditEvents]

  def testClaimant(
                    previousIncome: Option[TFCIncome] = None,
                    currentIncome: Option[TFCIncome] = None,
                    hoursPerWeek: Double = 0.00,
                    isPartner: Boolean = false,
                    disability: TFCDisability,
                    carersAllowance: Boolean = false,
                    minimumEarnings: TFCMinimumEarnings,
                    age: Option[String],
                    employmentStatus: Option[String] = None,
                    selfEmployedSelection: Option[Boolean] = None,
                    maximumEarnings: Option[Boolean] = None
                  ): TFCClaimant ={
    new TFCClaimant(previousIncome, currentIncome, hoursPerWeek, isPartner, disability,
      carersAllowance, minimumEarnings, age, employmentStatus, selfEmployedSelection, maximumEarnings){
      override lazy val auditEvents: AuditEvents = audits
      override lazy val tFCConfig: TFCConfig = tfcConfig
    }
  }

  def testChild(
               id: Short,
               childCareCost: BigDecimal,
               childcareCostPeriod: Periods.Period = Periods.Monthly,
               dob: LocalDate,
               disability: TFCDisability
              ): TFCChild = {
    new TFCChild(id, childCareCost, childcareCostPeriod, dob, disability) {
      override lazy val tFCConfig: TFCConfig = tfcConfig
    }
  }
}
