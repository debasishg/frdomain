package frdomain.ch6
package domain
package service
package interpreter

import java.util.{Date, Calendar}

import scalaz._
import Scalaz._
import \/._
import Kleisli._

import model.{Account, Balance}
import model.common._
import repository.AccountRepository

import scala.concurrent._
import ExecutionContext.Implicits.global

class AccountServiceInterpreter extends AccountService[Account, Amount, Balance] {

  def open(no: String,
           name: String,
           rate: Option[BigDecimal],
           openingDate: Option[Date],
           accountType: AccountType) = kleisli[Valid, AccountRepository, Account] { (repo: AccountRepository) =>
    repo.query(no) flatMap {
      case Some(_) => Valid(NonEmptyList(s"Already existing account with no $no").left[Account])
      case None => accountType match {
        case Checking => Valid(Account.checkingAccount(no, name, openingDate, None, Balance())).flatMap(repo.store)
        case Savings => rate map { r =>
          Valid(Account.savingsAccount(no, name, r, openingDate, None, Balance())).flatMap(repo.store)
        } getOrElse {
          Valid(NonEmptyList(s"Rate needs to be given for savings account").left[Account])
        }
      }
    }
  }

  def close(no: String, closeDate: Option[Date]) = kleisli[Valid, AccountRepository, Account] {
    (repo: AccountRepository) =>
      repo.query(no).flatMap {
        case None => Valid(NonEmptyList(s"Account $no does not exist").left[Account])
        case Some(a) =>
          val cd = closeDate.getOrElse(today)
          Valid(Account.close(a, cd)).flatMap(repo.store)
      }
  }

  def debit(no: String, amount: Amount) = up(no, amount, D)
  def credit(no: String, amount: Amount) = up(no, amount, C)

  private trait DC
  private case object D extends DC
  private case object C extends DC

  private def up(no: String, amount: Amount, dc: DC): AccountOperation[Account] = kleisli[Valid, AccountRepository, Account] {
    (repo: AccountRepository) =>
      repo.query(no) flatMap {
        case None => Valid(NonEmptyList(s"Account $no does not exist").left[Account])
        case Some(a) => dc match {
          case D => Valid(Account.updateBalance(a, -amount)).flatMap(repo.store)
          case C => Valid(Account.updateBalance(a, amount)).flatMap(repo.store)
        }
      }
  }

  def balance(no: String) =
    kleisli[Valid, AccountRepository, Balance] {
      (repo: AccountRepository) => repo.balance(no)
    }
}

object AccountService extends AccountServiceInterpreter
