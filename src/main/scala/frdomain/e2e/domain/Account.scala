package frdomain.e2e
package domain

import org.joda.time.DateTime
import scalaz._
import Scalaz._

import Common._

/**
 * Base abstraction for `Account`
 */
sealed trait Account {
  def no: String
  def name: String
  def dateOpened: DateTime
  def dateClosed: Option[DateTime]
  def currency: Currency
  def minBalanceRequired: Amount

  def isClosed = dateClosed isDefined
}

/**
 * Make case classes private : we will allow access through smart constructors only
 */
final case class CheckingAccount private[domain] (no: String, name: String,
  dateOpened: DateTime, dateClosed: Option[DateTime] = None, currency: Currency, minBalanceRequired: Amount) extends Account

final case class SavingsAccount private[domain] (no: String, name: String, rateOfInterest: Amount, 
  dateOpened: DateTime, dateClosed: Option[DateTime] = None, currency: Currency, minBalanceRequired: Amount) extends Account

object Account {
  /** 
   * Individual validation methods
   */
  def validNo(no: String) = 
    if (no.size != 10) s"Account number ($no) has to be of length 10".failureNel
    else no.success

  def validOpenDate(date: DateTime) =
    if (date isAfter today) s"Cannot open account in future date ($date)".failureNel
    else date.success

  def validMinBalance(minBalance: Amount) =
    if (minBalance < 0) s"Minimum balance required ($minBalance) must be >= 0".failureNel
    else minBalance.success

  def validRateOfInterest(rate: Amount) =
    if (rate < 0) s"Rate of interest required ($rate) must be >= 0".failureNel
    else if (rate > 3.5) s"Rate of interest required ($rate) must be < 3.5".failureNel
    else rate.success

  /**
   * Smart constructor for checking account. Note we use Validation as an applicative and report
   * all validation failures at a time.
   */
  def checkingAccount(no: String, name: String, openDate: DateTime, ccy: Currency, 
    minBalanceRequired: Amount = ZERO): ValidationNel[Error, Account] = 
    (validNo(no) |@| validOpenDate(openDate) |@| validMinBalance(minBalanceRequired)) { (n, o, m) =>
      CheckingAccount(n, name, o, None, ccy, m)
    }

  def savingsAccount(no: String, name: String, rate: Amount, openDate: DateTime, ccy: Currency, 
    minBalanceRequired: Amount = ZERO): ValidationNel[Error, Account] = 
    (validNo(no) |@| validOpenDate(openDate) |@| validMinBalance(minBalanceRequired) |@| validRateOfInterest(rate)) { (n, o, m, r) =>
      SavingsAccount(n, name, r, o, None, ccy, m)
    }

  def close(account: Account, date: DateTime): ValidationNel[Error, Account] =
    if (account.isClosed) s"Account (${account.no}) is already closed".failureNel
    else if (date isBefore account.dateOpened) s"Account closing date ($date) has to be >= Open date".failureNel
    else account match {
      case CheckingAccount(no, name, dopen, _, ccy, min) => CheckingAccount(no, name, dopen, date.some, ccy, min).success
      case SavingsAccount(no, name, rate, dopen, _, ccy, min) => SavingsAccount(no, name, rate, dopen, date.some, ccy, min).success
    }
}
