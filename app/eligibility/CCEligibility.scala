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

package eligibility

import models.input.tfc.Request
import models.input.{BaseRequest, BaseTaxYear}
import models.output.OutputAPIModel.Eligibility
import org.joda.time.LocalDate
import uk.gov.hmrc.play.http.HeaderCarrier
import scala.concurrent.Future

/**
 * Created by adamconder on 24/07/15.
 */

trait CCEligibilityHelpers {

  def fromAndUntilDateForPeriod[T <: BaseTaxYear](date : LocalDate, i : Int, datesOfChanges : List[LocalDate], ty : T) : (LocalDate, LocalDate) = {
    val from = if (i == 0) { date } else {
      val previousDate = datesOfChanges(i)
      previousDate
    }
    val until = if (i == datesOfChanges.length - 1) { ty.until } else { datesOfChanges(i + 1) }
    (from, until)
  }

}

trait CCEligibility extends CCEligibilityHelpers {
  val eligibility : CCEligibilityService

  trait CCEligibilityService {
    def eligibility(request : BaseRequest) : Future[Eligibility]
  }
}

trait CCTFCEligibility {
  val eligibility : CCTFCEligibilityService

  trait CCTFCEligibilityService {
    def eligibility(request : Request)(implicit req: play.api.mvc.Request[_], hc: HeaderCarrier): Future[Eligibility]
  }
}
