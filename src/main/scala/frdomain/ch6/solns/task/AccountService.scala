package frdomain.ch6
package solns
package task

import java.util.Date
import scalaz._
import Scalaz._
import Kleisli._
import scalaz.concurrent.Task

import domain.repository.AccountRepository

/**
 * Exercise 6.2: Future and scalaz.concurrent.Task
 *               Reimplement AccountService using scalaz.concurrent.Task in place of Future.
 */

sealed trait AccountType
case object Checking extends AccountType
case object Savings extends AccountType

trait AccountService[Account, Amount, Balance] {
  type AccountOperation[A] = Kleisli[Task, AccountRepository, A]

  def open(no: String, name: String, rate: Option[BigDecimal], openingDate: Option[Date], 
    accountType: AccountType): AccountOperation[Account]

  def close(no: String, closeDate: Option[Date]): AccountOperation[Account]

  def debit(no: String, amount: Amount): AccountOperation[Account]

  def credit(no: String, amount: Amount): AccountOperation[Account]

  def transfer(from: String, to: String, amount: Amount): AccountOperation[(Account, Account)] = for { 
    a <- debit(from, amount)
    b <- credit(to, amount)
  } yield ((a, b))
}
