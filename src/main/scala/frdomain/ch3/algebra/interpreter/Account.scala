package frdomain.ch3
package algebra.interpreter

import java.util.{ Date, Calendar }
import util.{ Try, Success, Failure }

object common {
  type Amount = BigDecimal

  def today = Calendar.getInstance.getTime
}

import common._

case class Balance(amount: Amount = 0)

case class Account(no: String, name: String, dateOfOpening: Date = today, dateOfClosing: Option[Date] = None, 
  balance: Balance = Balance(0))
