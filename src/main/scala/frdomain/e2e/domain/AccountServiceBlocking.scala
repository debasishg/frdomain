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

  def closeAccount(a: Account, date: DateTime): Kleisli[ErrorOr, AccountRepository[ErrorOr], Account] =
    kleisliU { repo: AccountRepository[ErrorOr] =>
      for {
        a <- repo.query(a.no)
        b <- Account.close(a, date)
      } yield b
    }

  def deposit(accountNo: String, amount: Amount, on: DateTime): Kleisli[ErrorOr, AccountRepository[ErrorOr], AccountBalance] =
    kleisliU { repo: AccountRepository[ErrorOr] =>
      for {
        a <- repo.query(accountNo)
        b <- repo.updateBalance(a, amount, on)
      } yield b
    }

  def withdraw(accountNo: String, amount: Amount, on: DateTime): Kleisli[ErrorOr, AccountRepository[ErrorOr], AccountBalance] =
    kleisliU { repo: AccountRepository[ErrorOr] =>
      for {
        a <- repo.query(accountNo)
        b <- repo.balance(a, on)
        _ <- AccountBalance.validWithdrawl(b, amount, a.currency)
        l <- repo.updateBalance(a, -amount, on)
      } yield l
    }

  def balance(accountNo: String, asOn: DateTime): Kleisli[ErrorOr, AccountRepository[ErrorOr], AccountBalance] =
    kleisliU { repo: AccountRepository[ErrorOr] =>
      for {
        a <- repo.query(accountNo)
        b <- repo.balance(a, asOn)
      } yield b
    }
}

object AccountServiceBlocking extends AccountServiceBlocking

