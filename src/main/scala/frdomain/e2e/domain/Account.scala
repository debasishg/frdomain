package frdomain.e2e
package domain

import org.joda.time.DateTime
import scalaz._
import Scalaz._

import Common._

/**
 * Make case classes private : we will allow access through smart constructors only
 */
final case class Account (no: String, name: String, rateOfInterest: Option[Amount], 
  dateOpened: DateTime, dateClosed: Option[DateTime] = None, currency: Currency, minBalanceRequired: Amount) {
  def isClosed = dateClosed.isDefined
} 

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

  def validRateOfInterest(rateOfInterest: Option[Amount]) = rateOfInterest.map { rate =>
    if (rate < 0) s"Rate of interest required ($rate) must be >= 0".failureNel
    else if (rate > 3.5) s"Rate of interest required ($rate) must be < 3.5".failureNel
    else rate.some.success
  }.getOrElse { None.success }

  /**
   * Smart constructor for checking account. Note we use Validation as an applicative and report
   * all validation failures at a time.
   */
  def account(no: String, name: String, rate: Option[Amount], openDate: DateTime, ccy: Currency, 
    minBalanceRequired: Amount = ZERO): ValidationNel[Error, Account] = 
    (validNo(no) |@| validOpenDate(openDate) |@| validMinBalance(minBalanceRequired) |@| validRateOfInterest(rate)) { (n, o, m, r) =>
      Account(n, name, r, o, None, ccy, m)
    }

  def close(account: Account, date: DateTime): ErrorOr[Account] =
    if (account.isClosed) s"Account (${account.no}) is already closed".left
    else if (date isBefore account.dateOpened) s"Account closing date ($date) has to be >= Open date".left
    else account.copy(dateClosed = date.some).right
}
