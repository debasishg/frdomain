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
case class AccountBalance private[domain] (accountNo: String, balance: Option[Money], asOn: LocalDate)

object AccountBalance {
  /**
   * Smart constructor
   */
  def accountBalance(account: Account, balance: Option[Money], asOn: LocalDate = today.toLocalDate): Error \/ AccountBalance = 
    balance.map { b =>
      val m = b.amount
      if ((m.size > 1) || m.isEmpty) s"Balance ($b) needs to have amount of one currency".left
      else if (m.keys.head != account.currency) s"Balance ($b) needs to have the same currency as the account".left
      else AccountBalance(account.no, b.some, asOn).right
    }.getOrElse { AccountBalance(account.no, None, asOn).right }

  def validWithdrawl(current: AccountBalance, withdraw: Amount, ccy: Currency): Error \/ Amount = current.balance match {
    case Some(m) => 
      m.amount.get(ccy).map { a => 
        if (a < withdraw) s"Insufficient funds ($current) to withdraw ($withdraw)".left
        else withdraw.right
      }.getOrElse {
        s"Invalid currency ($ccy) to withdraw from account".left
      }
    case None => s"Insufficient funds ($current) to withdraw ($withdraw)".left
  }
}
