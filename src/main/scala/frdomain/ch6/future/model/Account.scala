package frdomain.ch6
package future
package model

import java.util.{ Date, Calendar }
import scalaz._
import Scalaz._

object common {
  type Amount = BigDecimal

  def today = Calendar.getInstance.getTime
}

import common._

case class Account(no: String, name: String, dateOfOpen: Option[Date] = today.some, dateOfClose: Option[Date])
