package frdomain.ch3
package smartconstructor

import java.util.{ Date, Calendar }
import util.{ Try, Success, Failure }

object common {
  type Amount = BigDecimal

  def today = Calendar.getInstance.getTime
}

import common._

case class Balance(amount: Amount = 0)

sealed trait Account {
  def no: String
  def name: String
  def dateOfOpen: Option[Date]
  def dateOfClose: Option[Date]
  def balance: Balance
}

final case class CheckingAccount (no: String, name: String,
  dateOfOpen: Option[Date], dateOfClose: Option[Date] = None, balance: Balance = Balance()) extends Account

final case class SavingsAccount (no: String, name: String, rateOfInterest: Amount, 
  dateOfOpen: Option[Date], dateOfClose: Option[Date] = None, balance: Balance = Balance()) extends Account

object Account {
  def checkingAccount(no: String, name: String, openDate: Option[Date], closeDate: Option[Date], 
    balance: Balance): Try[Account] = { 

    val cd = closeDate.getOrElse(today)
    val od = openDate.getOrElse(today)

    if (cd before od)
      Failure(new Exception(s"Close date [$cd] cannot be earlier than open date [$od]")) 
    else Success(CheckingAccount(no, name, openDate, closeDate, balance))
  }

  def savingsAccount(no: String, name: String, rate: BigDecimal, openDate: Option[Date], 
    closeDate: Option[Date], balance: Balance): Try[Account] = { 

    val cd = closeDate.getOrElse(today)
    val od = openDate.getOrElse(today)

    if (cd before od)
      Failure(new Exception(s"Close date [$cd] cannot be earlier than open date [$od]") )
    else if (rate <= BigDecimal(0)) 
      Failure(new Exception(s"Interest rate $rate must be > 0"))
    else Success(SavingsAccount(no, name, rate, openDate, closeDate, balance))
  }
}

