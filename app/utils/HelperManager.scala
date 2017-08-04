package utils

import org.joda.time.LocalDate

trait HelperManager {

  def determineApril6DateFromNow(from: LocalDate): LocalDate = {
    val periodYear = from.getYear
    val january1st = LocalDate.parse(s"${periodYear}-01-01")
    val april6CurrentYear = LocalDate.parse(s"${periodYear}-04-06")

    if ((from.compareTo(january1st) == 0 || (from.isAfter(january1st)) && from.isBefore(april6CurrentYear))) {
      april6CurrentYear
    } else {
      april6CurrentYear.plusYears(1)
    }
  }

}

object HelperManager extends HelperManager
