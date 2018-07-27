package frdomain.ch6
package domain
package service
package interpreter

import java.util.{ Date, Calendar }

import cats._
import cats.data._
import cats.instances.all._
import cats.effect.IO

import model.{ Account, Balance }
import common._
import repository.AccountRepository

class AccountServiceInterpreter extends AccountService[Account, Amount, Balance] {

  def open(no: String, 
           name: String, 
           rate: Option[BigDecimal],
           openingDate: Option[Date],
           accountType: AccountType) = Kleisli[Valid, AccountRepository, Account] { (repo: AccountRepository) =>

    EitherT {
      repo.query(no).flatMap {

        case Right(Some(a)) => IO(Left(AlreadyExistingAccount(a.no)))

        case Right(None)    => accountType match {

          case Checking => createOrUpdate(repo, Account.checkingAccount(no, name, openingDate, None, Balance())) 

          case Savings  => rate map { r =>
            createOrUpdate(repo, Account.savingsAccount(no, name, r, openingDate, None, Balance()))
          } getOrElse {
            IO(Left(RateMissingForSavingsAccount))
          }
        }

        case Left(x)        => IO(Left(MiscellaneousDomainExceptions(x)))
      }
    }
  }

  private def createOrUpdate(repo: AccountRepository, 
    errorOrAccount: ErrorOr[Account]): IO[Either[AccountServiceException, Account]] = errorOrAccount match {

    case Left(errs) => IO(Left(MiscellaneousDomainExceptions(errs)) )
    case Right(a) => repo.store(a).map {
      case Right(acc) => Right(acc)
      case Left(errs) => Left(MiscellaneousDomainExceptions(errs))
    }
  }

  def close(no: String, closeDate: Option[Date]) = Kleisli[Valid, AccountRepository, Account] { (repo: AccountRepository) =>
    EitherT {
      repo.query(no).flatMap {

        case Right(None) => IO(Left(NonExistingAccount(no)))

        case Right(Some(a))    => 
          val cd = closeDate.getOrElse(today)
          createOrUpdate(repo, Account.close(a, cd))

        case Left(x)        => IO(Left(MiscellaneousDomainExceptions(x)))
      }
    }
  }

  def debit(no: String, amount: Amount) = up(no, amount, D)
  def credit(no: String, amount: Amount) = up(no, amount, C)

  private trait DC
  private case object D extends DC
  private case object C extends DC

  private def up(no: String, amount: Amount, dc: DC) = Kleisli[Valid, AccountRepository, Account] { (repo: AccountRepository) =>
    EitherT {
      repo.query(no).flatMap {

        case Right(None) => IO(Left(NonExistingAccount(no)))

        case Right(Some(a))    => dc match {
          case D => createOrUpdate(repo, Account.updateBalance(a, -amount))
          case C => createOrUpdate(repo, Account.updateBalance(a, amount))
        }

        case Left(x)        => IO(Left(MiscellaneousDomainExceptions(x)))
      }
    }
  }

  def balance(no: String) = Kleisli[Valid, AccountRepository, Balance] { (repo: AccountRepository) =>
    EitherT {
      repo.balance(no).map { 
        case Left(errs) => Left(MiscellaneousDomainExceptions(errs))
        case Right(b) => Right(b)
      }
    }
  }
}

object AccountService extends AccountServiceInterpreter
