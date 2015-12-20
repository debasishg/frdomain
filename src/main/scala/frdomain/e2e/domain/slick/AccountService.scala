package frdomain.e2e
package domain
package slickdb
package service

import org.joda.time.DateTime
import scalaz._
import Scalaz._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import slickdb.repository._
import slick.driver.H2Driver.api._

trait AccountService {
  def openAccount(no: String, name: String, rate: Option[Amount], dateOpened: DateTime = today, ccy: Currency, 
    minBalance: Amount = ZERO): Future[Option[Account]]

  def closeAccount(no: String, date: DateTime): Future[Option[Account]]
  def deposit(accountNo: String, amount: Amount, on: DateTime): Future[Option[AccountBalance]]
  def withdraw(accountNo: String, amount: Amount, on: DateTime): Future[Option[AccountBalance]]
  def balance(accountNo: String, asOn: DateTime): Future[Option[AccountBalance]]
}

class AccountServiceImpl(db: Database) extends AccountService with AccountRepository {
  lazy val repository = AccountRepositoryRelationalDBInterpreter(db)

  def openAccount(no: String, name: String, rate: Option[Amount], dateOpened: DateTime = today, ccy: Currency, 
    minBalance: Amount = ZERO): Future[Option[Account]] =
    Account.account(no, name, rate, dateOpened, ccy, minBalance) match {
      case Success(a) => repository(storeAccount(a))
      case Failure(errs) => Future.failed { new RuntimeException(errs.list.toList.mkString("/")) }
    }

  def closeAccount(no: String, date: DateTime): Future[Option[Account]] = 
    repository(update(no, Some(CloseAction(date))))

  def deposit(accountNo: String, amount: Amount, on: DateTime): Future[Option[AccountBalance]] = 
    repository(updateBalance(accountNo, on.toLocalDate, Some(DepositAction(amount))))

  def withdraw(accountNo: String, amount: Amount, on: DateTime): Future[Option[AccountBalance]] = 
    repository(updateBalance(accountNo, on.toLocalDate, Some(WithdrawAction(amount))))

  def balance(accountNo: String, asOn: DateTime): Future[Option[AccountBalance]] = 
    repository(findBalance(accountNo, asOn.toLocalDate))
}

