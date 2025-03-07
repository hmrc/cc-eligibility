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

package controllers

import models.input.tfc._
import org.apache.pekko.stream.Materializer
import org.apache.pekko.util.ByteString
import org.mockito.Mockito.when
import org.scalatest.Suite
import org.scalatestplus.mockito.MockitoSugar
import play.api.libs.json.{JsValue, Json}
import play.api.mvc._
import play.api.test.Helpers.{stubBodyParser, stubMessagesApi}
import service.AuditEvents
import uk.gov.hmrc.http.HeaderCarrier
import utils._

import java.nio.charset.Charset
import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.{implicitConversions, postfixOps}


trait FakeCCEligibilityApplication extends CCConfigSpec with MockitoSugar {
  this: Suite =>

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
    }
  }

  def testChild(
                 id: Short,
                 childCareCost: BigDecimal,
                 childcareCostPeriod: Periods.Period = Periods.Monthly,
                 dob: LocalDate,
                 disability: TFCDisability,
                 ccConfig: Option[CCConfig] = None
               ): TFCChild = {
    new TFCChild(id, childCareCost, childcareCostPeriod, dob, disability)(ccConfig)
  }

  def jsonBodyOf(result: Result)(implicit mat: Materializer): JsValue = {
    Json.parse(bodyOf(result))
  }

  def jsonBodyOf(resultF: Future[Result])(implicit mat: Materializer): Future[JsValue] = {
    resultF.map(jsonBodyOf)
  }
  def bodyOf(result: Result)(implicit mat: Materializer): String = {
    val bodyBytes: ByteString = await(result.body.consumeData)
    // We use the default charset to preserve the behaviour of a previous
    // version of this code, which used new String(Array[Byte]).
    // If the fact that the previous version used the default charset was an
    // accident then it may be better to decode in UTF-8 or the charset
    // specified by the result's headers.
    bodyBytes.decodeString(Charset.defaultCharset().name)
  }

  def bodyOf(resultF: Future[Result])(implicit mat: Materializer): Future[String] = {
    resultF.map(bodyOf)
  }

  import scala.concurrent.duration._
  import scala.concurrent.{Await, Future}

  implicit val defaultTimeout: FiniteDuration = 5 seconds

  implicit def extractAwait[A](future: Future[A]): A = await[A](future)

  def await[A](future: Future[A])(implicit timeout: Duration): A = Await.result(future, timeout)

  // Convenience to avoid having to wrap andThen() parameters in Future.successful
  implicit def liftFuture[A](v: A): Future[A] = Future.successful(v)

  def status(of: Result): Int = of.header.status

  def status(of: Future[Result])(implicit timeout: Duration): Int = status(Await.result(of, timeout))

}
