package frdomain.e2e
package domain

import org.joda.time._
import scalaz._
import Scalaz._

import Common._

/**
 * Models balance of an account as on a specific date. `balance = None` implies
 * that balance is not available for this date (different from 0 balance).
 */
case class AccountBalance private[domain] (id: Option[Long], accountNo: String, balance: Amount, currency: Currency, asOn: LocalDate) {
  def +(amount: Amount) = this.copy(id = None, balance = this.balance + amount)
}

object AccountBalance {
  def validWithdrawl(current: AccountBalance, withdraw: Amount, ccy: Currency): Error \/ Amount = 
    if (current.currency != ccy) s"Currency of account ${current.currency} does not match with $ccy".left
    else if (current.balance < withdraw) s"Insufficient funds ($current) to withdraw ($withdraw)".left
    else withdraw.right
}
