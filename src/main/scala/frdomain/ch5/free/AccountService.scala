package frdomain.ch5
package free

import java.util.Date
import scalaz._
import Scalaz._
import common._

trait AccountService[Account, Amount, Balance] {
  def open(no: String, name: String, openingDate: Option[Date]): AccountRepo[Account]
  def close(no: String, closeDate: Option[Date]): AccountRepo[Account]
  def debit(no: String, amount: Amount): AccountRepo[Account]
  def credit(no: String, amount: Amount): AccountRepo[Account]
  def balance(no: String): AccountRepo[Balance]
}

object AccountService extends AccountService[Account, Amount, Balance] with AccountRepository {
  
  def open(no: String, name: String, openingDate: Option[Date]) = for {
    _ <- store(Account(no, name, openingDate.get))
    a <- query(no)
  } yield a

  private val close: Account => Account = { _.copy(dateOfClosing = Some(today)) }

  def close(no: String, closeDate: Option[Date]): AccountRepo[Account] = for {
    _ <- update(no, close)
    a <- query(no)
  } yield a

  private def debitImpl(a: Account, amount: Amount) = {
    if (a.balance.amount < amount) throw new RuntimeException("insufficient fund to debit")
    a.copy(balance = Balance(a.balance.amount - amount))
  }

  def debit(no: String, amount: Amount): AccountRepo[Account] = for {
    _ <- updateBalance(no, amount, debitImpl)
    a <- query(no)
  } yield a

  private def creditImpl(a: Account, amount: Amount) = {
    a.copy(balance = Balance(a.balance.amount + amount))
  }

  def credit(no: String, amount: Amount): AccountRepo[Account] = for {
    _ <- updateBalance(no, amount, creditImpl)
    a <- query(no)
  } yield a

  def balance(no: String): AccountRepo[Balance] = for {
    a <- query(no)
  } yield a.balance
}
