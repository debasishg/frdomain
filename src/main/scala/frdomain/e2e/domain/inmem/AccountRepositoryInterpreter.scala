package frdomain.e2e
package domain
package inmem
package repository

import org.joda.time.LocalDate
import scala.collection.concurrent.TrieMap

import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import Task._
import Free._

trait AccountRepositoryInterpreter {
  def apply[A](action: AccountRepo[A]): Task[A]
}
  
/**
 * Basic interpreter that uses a global mutable Map to store the state
 * of computation
 */
case class AccountRepositoryMutableInterpreter() extends AccountRepositoryInterpreter {

  // account repo
  private val arepo = TrieMap.empty[String, Account]

  // balance repo
  private val brepo = TrieMap.empty[(String, LocalDate), Amount]

  def step[A](action: AccountRepoF[AccountRepo[A]]): Task[AccountRepo[A]] = action match {

    case FindAccount(no, onResult) => 
      arepo.get(no)
           .map { a => now(onResult(a)) }
           .getOrElse { fail(new RuntimeException(s"Account no $no not found")) }

    case StoreAccount(account, next) => now { arepo += ((account.no, account)) } map { _ => next }

    case FindBalance(no, asOn, onResult) => (for {
      a <- arepo.get(no)
      m <- brepo.getOrElse((no, asOn.toLocalDate), ZERO).some
    } yield now {
      onResult(AccountBalance(None, no, m, a.currency, asOn.toLocalDate))
    }).getOrElse { fail(new RuntimeException(s"Account no $no not found")) }

    case StoreBalance(b, next) => now { brepo += (((b.accountNo, b.asOn), b.balance)) } map { _ => next }
  }

  /**
   * Turns the AccountRepo script into a `Task` that executes it in a mutable setting
   */
  def apply[A](action: AccountRepo[A]): Task[A] = action.runM(step)
}
