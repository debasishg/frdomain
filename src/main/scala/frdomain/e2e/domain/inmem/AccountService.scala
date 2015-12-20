package frdomain.e2e
package domain
package inmem
package service

import org.joda.time.DateTime
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import Task._

import inmem.repository._

trait AccountService {
  def openAccount(no: String, name: String, rate: Option[Amount], dateOpened: DateTime = today, ccy: Currency, 
    minBalance: Amount = ZERO): Task[Unit]

  def closeAccount(no: String, date: DateTime): Task[Unit]
  def deposit(accountNo: String, amount: Amount, on: DateTime): Task[Unit]
  def withdraw(accountNo: String, amount: Amount, on: DateTime): Task[Unit]
  def balance(accountNo: String, asOn: DateTime): Task[AccountBalance]
}

object AccountService extends AccountService with AccountRepository {
  lazy val repository = AccountRepositoryMutableInterpreter()

  def openAccount(no: String, name: String, rate: Option[Amount], dateOpened: DateTime = today, ccy: Currency, 
    minBalance: Amount = ZERO): Task[Unit] =
    Account.account(no, name, rate, dateOpened, ccy, minBalance) match {
      case Success(a) => repository(storeAccount(a))
      case Failure(errs) => fail { new RuntimeException(errs.list.toList.mkString("/")) }
    }

  def closeAccount(no: String, date: DateTime): Task[Unit] = for {
    a <- repository(findAccount(no))
    c <- fromDisjunction(Account.close(a, date).leftMap(new RuntimeException(_)))
    _ <- repository(storeAccount(c))
  } yield (())

  def deposit(accountNo: String, amount: Amount, on: DateTime): Task[Unit] = 
    repository(updateBalance(accountNo, amount, on))

  def withdraw(accountNo: String, amount: Amount, on: DateTime): Task[Unit] = for {
    b <- repository(findBalance(accountNo, on))
    _ <- fromDisjunction(AccountBalance.validWithdrawl(b, amount, b.currency).leftMap(new RuntimeException(_)))
    _ <- repository(storeBalance(b + (-amount)))
  } yield (())

  def balance(accountNo: String, asOn: DateTime): Task[AccountBalance] = 
    repository(findBalance(accountNo, asOn))
}
