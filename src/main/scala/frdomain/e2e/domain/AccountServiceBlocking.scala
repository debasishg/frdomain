package frdomain.e2e
package domain

import org.joda.time.DateTime
import scalaz._
import Scalaz._
import Kleisli._

import Common._

class AccountServiceBlocking extends AccountService[ErrorOr] {
  val M: Monad[ErrorOr] = Monad[ErrorOr]

  def openAccount(no: String, name: String, rate: Option[Amount], dateOpened: DateTime = today, ccy: Currency, 
    minBalance: Amount = ZERO): Kleisli[ErrorOr, AccountRepository[ErrorOr], Account] = 
    kleisliU { repo: AccountRepository[ErrorOr] =>
      Account.account(no, name, rate, dateOpened, ccy, minBalance) match {
        case Success(a) => repo.store(a)
        case Failure(errs) => errs.list.toList.mkString("/").left
      }
    }

  def closeAccount(no: String, date: DateTime): Kleisli[ErrorOr, AccountRepository[ErrorOr], Account] =
    kleisliU { repo: AccountRepository[ErrorOr] =>
      for {
        a <- repo.query(no)
        c <- a.map { Account.close(_, date) } getOrElse { s"Account with no $no does not exist".left }
        s <- repo.store(c)
      } yield s
    }

  def deposit(accountNo: String, amount: Amount, on: DateTime): Kleisli[ErrorOr, AccountRepository[ErrorOr], AccountBalance] =
    kleisliU { repo: AccountRepository[ErrorOr] =>
      for {
        a <- repo.query(accountNo)
        b <- a.map { repo.updateBalance(_, amount, on) } getOrElse { s"Account with no $accountNo does not exist".left }
      } yield b
    }

  def withdraw(accountNo: String, amount: Amount, on: DateTime): Kleisli[ErrorOr, AccountRepository[ErrorOr], AccountBalance] =
    kleisliU { repo: AccountRepository[ErrorOr] =>
      for {
        a <- repo.query(accountNo)
        b <- a.map { repo.balance(_, on) } getOrElse { s"Account with no $accountNo does not exist".left }
        _ <- AccountBalance.validWithdrawl(b, amount, b.currency)
        l <- repo.updateBalance(a.get, -amount, on)
      } yield l
    }

  def balance(accountNo: String, asOn: DateTime): Kleisli[ErrorOr, AccountRepository[ErrorOr], AccountBalance] =
    kleisliU { repo: AccountRepository[ErrorOr] =>
      for {
        a <- repo.query(accountNo)
        b <- a.map { repo.balance(_, asOn) } getOrElse { s"Account with no $accountNo does not exist".left }
      } yield b
    }
}

object AccountServiceBlocking extends AccountServiceBlocking

