package frdomain.e2e
package domain

import org.joda.time.DateTime
import scalaz._
import Scalaz._

import Common._

/**
 * Parameterized on a `Monad`. Note how the type system enforces that we have the
 * same `Monad` for `AccountService` and `AccountRepository`.
 */
trait AccountService[M[+_]] {
  implicit def M: Monad[M]

  def openAccount(no: String, name: String, rate: Option[Amount], dateOpened: DateTime = today, ccy: Currency, 
    minBalance: Amount = ZERO): Kleisli[M, AccountRepository[M], Account]

  def closeAccount(no: String, date: DateTime): Kleisli[M, AccountRepository[M], Account]

  def deposit(accountNo: String, amount: Amount, on: DateTime): Kleisli[M, AccountRepository[M], AccountBalance]
  def withdraw(accountNo: String, amount: Amount, on: DateTime): Kleisli[M, AccountRepository[M], AccountBalance]
  def balance(accountNo: String, asOn: DateTime): Kleisli[M, AccountRepository[M], AccountBalance]
}
