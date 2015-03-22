package frdomain.ch4
package patterns
package monoids.nomonoid

import java.util.Date

sealed trait TransactionType
case object DR extends TransactionType
case object CR extends TransactionType

sealed trait Currency
case object USD extends Currency
case object JPY extends Currency
case object AUD extends Currency
case object INR extends Currency

object common {
  type Amount = BigDecimal
}

import common._

case class Money(m: Map[Currency, BigDecimal]) {
  def +(that: Money) = {
    val n = that.m.foldLeft(m) { (a, e) =>
      val (ccy, amt) = e
      a.get(ccy).map { amount =>
        a + ((ccy, amt + amount))
      }.getOrElse(a + ((ccy, amt)))
    }
    Money(n)
  }
  def toBaseCurrency: BigDecimal = ???
}

object Money {
  val zeroMoney = Money(Map.empty[Currency, BigDecimal])
}

import Money._
object MoneyOrdering extends Ordering[Money] {
  def compare(a:Money, b:Money) = a.toBaseCurrency compare b.toBaseCurrency
}

import MoneyOrdering._
import scala.math.Ordering

case class Transaction(txid: String, accountNo: String, date: Date, amount: Money, txnType: TransactionType, status: Boolean)

case class Balance(b: Money)

trait Analytics[Transaction, Balance, Money] {
  def maxDebitOnDay(txns: List[Transaction]): Money
  def sumBalances(bs: List[Balance]): Money
}

object Analytics extends Analytics[Transaction, Balance, Money] {
  def maxDebitOnDay(txns: List[Transaction]): Money = {
    txns.filter(_.txnType == DR).foldLeft(zeroMoney) { (a, txn) =>
      if (gt(txn.amount, a)) txn.amount else a
    }
  }

  def sumBalances(bs: List[Balance]): Money = bs.foldLeft(zeroMoney)(_ + _.b)
}
    
