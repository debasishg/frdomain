package frdomain.e2e
package domain

import org.joda.time.DateTime
import scalaz._
import Scalaz._
import Kleisli._

import Common._

/**
 * Parameterized on a `Monad`. Note how the type system enforces that we have the
 * same `Monad` for `AccountService` and `AccountRepository`.
 */
trait AccountService[M[+_]] {
  implicit def M: Monad[M]

  def openCheckingAccount(no: String, name: String, dateOpened: DateTime = today, ccy: Currency, 
    minBalance: Amount = ZERO): Kleisli[M, AccountRepository[M], Account]

  def openSavingsAccount(no: String, name: String, rate: Amount, dateOpened: DateTime = today, ccy: Currency, 
    minBalance: Amount = ZERO): Kleisli[M, AccountRepository[M], Account]

  def close(a: Account, date: DateTime): Kleisli[M, AccountRepository[M], Account]

  def deposit(accountNo: String, amount: Amount, on: DateTime): Kleisli[M, AccountRepository[M], AccountBalance]
  def withdraw(accountNo: String, amount: Amount, on: DateTime): Kleisli[M, AccountRepository[M], AccountBalance]
  def balance(accountNo: String, asOn: DateTime): Kleisli[M, AccountRepository[M], AccountBalance]
}

class AccountServiceDefaultImpl extends AccountService[ErrorOr] {
  val M: Monad[ErrorOr] = Monad[ErrorOr]

  def openCheckingAccount(no: String, name: String, dateOpened: DateTime = today, ccy: Currency, 
    minBalance: Amount = ZERO): Kleisli[ErrorOr, AccountRepository[ErrorOr], Account] = 
    Kleisli { repo: AccountRepository[ErrorOr] =>
      Account.checkingAccount(no, name, dateOpened, ccy, minBalance) match {
        case Success(a) => repo.store(a)
        case Failure(errs) => errs.list.toList.mkString("/").left
      }
    }

  def openSavingsAccount(no: String, name: String, rate: Amount, dateOpened: DateTime = today, ccy: Currency, 
    minBalance: Amount = ZERO): Kleisli[ErrorOr, AccountRepository[ErrorOr], Account] = 
    Kleisli { repo: AccountRepository[ErrorOr] =>
      Account.savingsAccount(no, name, rate, dateOpened, ccy, minBalance) match {
        case Success(a) => repo.store(a)
        case Failure(errs) => errs.list.toList.mkString("/").left
      }
    }

  def close(a: Account, date: DateTime): Kleisli[ErrorOr, AccountRepository[ErrorOr], Account] =
    Kleisli { repo: AccountRepository[ErrorOr] =>
      repo.query(a.no) match {
        case \/-(a) =>
          Account.close(a, date) match {
            case Success(a) => repo.store(a)
            case Failure(errs) => errs.list.toList.mkString("/").left
          }
        case -\/(err) => err.left
      }
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
