package frdomain.ch6
package streams

import java.util.{ Date, Calendar }
import util.{ Try, Success, Failure }
import scalaz._
import Scalaz._

object common {
  type Amount = BigDecimal

  def today = Calendar.getInstance.getTime
}

import common._

case class Balance(amount: Amount = 0)

case class Account (no: String, name: String, dateOfOpen: Option[Date], dateOfClose: Option[Date] = None, 
  balance: Balance = Balance()) 
