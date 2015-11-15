package frdomain.ch8
package cqrs
package service

import scalaz._
import Scalaz._

import java.util.{ Date, Calendar }
import lib._

object common {
  type Amount = BigDecimal
  type Error = String

  val today = Calendar.getInstance.getTime
}

import common._

case class Balance(amount: Amount = 0)

case class Account(no: String, name: String, dateOfOpening: Date = today, dateOfClosing: Option[Date] = None, 
  balance: Balance = Balance(0)) extends Aggregate {
  def id = no
  def isClosed = dateOfClosing.isDefined
}

object Account {
  implicit val showAccount: Show[Account] = Show.shows { case a: Account => a.toString }
}



