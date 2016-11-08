package frdomain.ch3
package algebra.interpreter

import java.util.{ Date, Calendar }
import util.{ Try, Success, Failure }

import common._
import algebra.AccountService

object AccountService extends AccountService[Account, Amount, Balance] {
  def open(no: String, name: String, openingDate: Option[Date]): Try[Account] = {
    val currentDate = today
    if (no.isEmpty || name.isEmpty) Failure(new Exception(s"Account no or name cannot be blank") )
    else if (openingDate.getOrElse(currentDate) before currentDate) Failure(new Exception(s"Cannot open account in the past"))
    else Success(Account(no, name, openingDate.getOrElse(currentDate)))
  }

  def close(account: Account, closeDate: Option[Date]): Try[Account] = {
    val cd = closeDate.getOrElse(today)
    if (cd before account.dateOfOpening) 
      Failure(new Exception(s"Close date $cd cannot be before opening date ${account.dateOfOpening}")) 
    else Success(account.copy(dateOfClosing = Some(cd)))
  }

  def debit(a: Account, amount: Amount): Try[Account] = {
    if (a.balance.amount < amount) Failure(new Exception("Insufficient balance"))
    else Success(a.copy(balance = Balance(a.balance.amount - amount)))
  }

  def credit(a: Account, amount: Amount): Try[Account] = 
    Success(a.copy(balance = Balance(a.balance.amount + amount)))

  def balance(account: Account): Try[Balance] = Success(account.balance)
}
