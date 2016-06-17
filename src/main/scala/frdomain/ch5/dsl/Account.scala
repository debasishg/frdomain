package frdomain.ch5
package dsl

import cats._
import cats.std.all._
import cats.syntax.show._

import cats.SemigroupK
import cats.data.NonEmptyList

import cats.data.{ Validated, Xor }
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel


import org.joda.time.DateTime

object common {
  type Amount = BigDecimal

  val today = DateTime.now()
}

import common._

private[dsl] case class Balance(amount: Amount = 0, asOf: DateTime = today)

private[dsl] case class Account(no: String, name: String, dateOfOpening: DateTime = today, 
  dateOfClosing: Option[DateTime] = None, balance: Balance = Balance(0))

object Account {
  import cats.std.list._
  import cats.syntax.cartesian._

  implicit val showAccount: Show[Account] = Show.show { case a: Account => a.toString }

  sealed abstract class AccountError
  final case class InvalidAccountNo(msg: String) extends AccountError
  final case class CannotBeAfterToday(msg: String) extends AccountError
  final case class NegativeBalance(msg: String) extends AccountError
  final case class CannotCloseWithBalance(msg: String) extends AccountError

  type AccountValidation[A] = Validated[AccountError, A]
  type AccountValidationNel[A] = Xor[NonEmptyList[AccountError], A]

  def validAccountNo(no: String): AccountValidation[String] =
    if (no.size < 5) Invalid(InvalidAccountNo(s"Account no $no must be > 5 characters"))
    else Valid(no)

  def cannotBeInFuture(date: DateTime): AccountValidation[DateTime] =
    if (date.isAfter(today)) Invalid(CannotBeAfterToday(s"Open date ($date) cannot be after today"))
    else Valid(date)

  def validBalance(a: Account, amount: Amount): AccountValidation[Amount] = {
    val b = a.balance.amount + amount
    if (b < BigDecimal(0)) Invalid(NegativeBalance(s"Negative balance $b reached"))
    else Valid(b)
  }

  def isCloseable(a: Account): AccountValidation[Account] =
    if (a.balance.amount > BigDecimal(0)) Invalid(CannotCloseWithBalance(s"Account still has balance ${a.balance.amount}")) else Valid(a)

  implicit val nelSemigroup: Semigroup[NonEmptyList[AccountError]] =
    SemigroupK[NonEmptyList].algebra[AccountError]
  

  def open(no: String, name: String, openDate: DateTime): Xor[String, Account] =
    ((validAccountNo(no).toValidatedNel |@| cannotBeInFuture(openDate).toValidatedNel) map { (n, od) =>
      Account(n, name, od)
    }).toXor.leftMap(_.unwrap.mkString(","))

  def transact(a: Account, amount: Amount, asOf: DateTime): Xor[String, Account] =
    ((validBalance(a, amount).toValidatedNel |@| cannotBeInFuture(asOf).toValidatedNel) map { (amt, d) => 
      a.copy(balance = Balance(amt, d))
    }).toXor.leftMap(_.unwrap.mkString(","))

  def close(a: Account, closeDate: DateTime): Xor[String, Account] =
    ((isCloseable(a).toValidatedNel |@| cannotBeInFuture(closeDate).toValidatedNel) map { (_, cd) =>
      a.copy(dateOfClosing = Some(closeDate))
    }).toXor.leftMap(_.unwrap.mkString(","))
}


