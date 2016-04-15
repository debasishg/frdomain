package frdomain.e2e
package domain

import org.joda.time._
import scalaz._
import Scalaz._

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

sealed trait AccountBalanceAction {
  def run(balance: AccountBalance): ErrorOr[AccountBalance]
}

case class WithdrawAction(withdraw: Amount) extends AccountBalanceAction {
  def run(current: AccountBalance) = AccountBalance.validWithdrawl(current, withdraw, current.currency).map { amount => 
    current.copy(balance = current.balance - amount)
  }
}

case class DepositAction(amount: Amount) extends AccountBalanceAction {
  def run(current: AccountBalance) = current.copy(balance = current.balance + amount).right
}
