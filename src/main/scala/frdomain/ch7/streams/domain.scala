package frdomain.ch7
package streams

import java.util.Date
import scala.concurrent.duration._
import scala.concurrent.{ Future, ExecutionContext }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.immutable._

import scalaz._
import Scalaz._

import java.util.{ Date, Calendar }

object common {
  type Amount = BigDecimal

  def today = Calendar.getInstance.getTime
}

import common._

case class Account (no: String, name: String, dateOfOpen: Option[Date], dateOfClose: Option[Date] = None, 
  balance: Balance = Balance(0, Debit)) 
import common._

sealed trait TransactionType
case object Debit extends TransactionType
case object Credit extends TransactionType

object TransactionType {
  def apply(s: String) = s.toLowerCase match {
    case "d" => Some(Debit)
    case "c" => Some(Credit)
    case _ => None
  }
}

case class Transaction(id: String, accountNo: String, debitCredit: TransactionType, amount: Amount, date: Date = today)

object Transaction {
  def apply(fields: Array[String]): Option[Transaction] = {
    this(fields(0), fields(1), fields(2), BigDecimal(fields(3)))  // @todo: exception handling
  }

/*
  def apply(fields: Array[String]): Future[Transaction] = {
    this(fields(0), fields(1), fields(2), BigDecimal(fields(3))) match {  // @todo: exception handling
      case Some(t) => validate(t)
      case None => Future.failed(new Exception("Validation failed for transaction"))
    }
  }
*/

  def validate(t: Transaction): Future[Transaction] = Future.successful(t)

  def apply(id: String, accountNo: String, t: String, amount: Amount): Option[Transaction] = TransactionType(t) match {
    case None    => None
    case Some(d) => Some(Transaction(id, accountNo, d, amount, today))
  }

  implicit val TransactionMonoid = new Monoid[Transaction] {
    val zero = Transaction("", "", Debit, 0)
    def append(i: Transaction, j: => Transaction) = {
      val f = if (i.debitCredit == Debit) -i.amount else i.amount
      val s = if (j.debitCredit == Debit) -j.amount else j.amount
      val sum = f + s
      val id = util.Random.nextInt(Integer.MAX_VALUE).toString
      if (sum < 0) Transaction(id, j.accountNo, Debit, -sum) else Transaction(id, j.accountNo, Credit, sum)
    }
  }
}

case class Balance(amount: Amount, debitCredit: TransactionType)

object Balance {
  implicit val BalanceMonoid = new Monoid[Balance] {
    val zero = Balance(0, Debit)
    def append(i: Balance, j: => Balance) = (i.debitCredit, j.debitCredit) match {
      case (Debit, Debit)                         => Balance(i.amount + j.amount, Debit)
      case (Credit, Credit)                       => Balance(i.amount + j.amount, Credit)
      case (Debit, Credit) if i.amount > j.amount => Balance(i.amount - j.amount, Debit)
      case (Debit, Credit)                        => Balance(j.amount - i.amount, Credit)
      case (Credit, Debit) if i.amount > j.amount => Balance(i.amount - j.amount, Credit)
      case (Credit, Debit)                        => Balance(j.amount - i.amount, Debit)
    }
  }
}

object LogSummaryBalance

