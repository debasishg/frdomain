package frdomain.e2e
package domain

import org.joda.time.DateTime
import scalaz._
import Scalaz._

import Common._

/**
 * Parameterized on a `Monad`. We can have various implementations of a repository
 * depending on which monad we choose. e.g. `InMemoryAccountRepository` chooses disjunction as 
 * the monad. Similarly we can have one that uses a `Future`.
 */
trait AccountRepository[M[+_]] {
  implicit def M: Monad[M]
  
  def query(accountNo: String): M[Account]
  def store(a: Account): M[Account]
  def update(a: Account): M[Account]
  def accountsOpenedOn(date: DateTime): M[List[Account]]
  
  /**
   * Updates the balance for `account` as on the date `asOn` by `amount`. The value
   * of `amount` is treated algebraically - negative amount means debit. If the balance 
   * entry for the date `asOn` does not exist, then a new entry is created for this date.
   * This function assumes the `account` exists - ensure this check is done at the domain 
   * service level.
   */
  def updateBalance(account: Account, amount: Amount, asOn: DateTime): M[AccountBalance]

  /**
   * Gets the balance for this `account`. 
   */
  def balance(account: Account, asOn: DateTime): M[AccountBalance]
}
