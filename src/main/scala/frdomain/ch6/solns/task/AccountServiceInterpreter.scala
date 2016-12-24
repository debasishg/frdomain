package frdomain.ch6
package solns
package task

import java.util.Date
import scalaz._
import Scalaz._
import Kleisli._
import scalaz.concurrent.Task

import domain.model.{ Account, Balance }
import domain.model.common._
import domain.repository.AccountRepository

class AccountServiceInterpreter extends AccountService[Account, Amount, Balance] {

  def open(no: String, 
           name: String, 
           rate: Option[BigDecimal],
           openingDate: Option[Date],
           accountType: AccountType) = kleisli[Task, AccountRepository, Account] { (repo: AccountRepository) =>

    Task {
      repo.query(no) match {
        case \/-(Some(a)) =>  throw new Exception(s"Already existing account with no $no")
        case \/-(None)    => accountType match {
          case Checking => 
            Account.checkingAccount(no, name, openingDate, None, Balance()).flatMap(repo.store) match {
              case -\/(err) => throw new Exception(err.list.toList.mkString(","))
              case \/-(a) => a
            }
          case Savings  => rate map { r =>
            Account.savingsAccount(no, name, r, openingDate, None, Balance()).flatMap(repo.store) match {
              case -\/(err) => throw new Exception(err.list.toList.mkString(","))
              case \/-(a) => a
            }
          } getOrElse {
            throw new Exception(s"Rate needs to be given for savings account")
          }
        }
        case -\/(err) => throw new Exception(err.list.toList.mkString(","))
      }
    }
  }

  def close(no: String, closeDate: Option[Date]) = kleisli[Task, AccountRepository, Account] { (repo: AccountRepository) =>
    Task {
      repo.query(no) match {
        case \/-(None) => throw new Exception(s"Account $no does not exist")
        case \/-(Some(a)) => 
          val cd = closeDate.getOrElse(today)
          Account.close(a, cd).flatMap(repo.store) match { 
            case -\/(err) => throw new Exception(err.list.toList.mkString(","))
            case \/-(a) => a
          }
        case -\/(err) => throw new Exception(err.list.toList.mkString(","))
      }
    }
  }

  def debit(no: String, amount: Amount) = up(no, amount, D)
  def credit(no: String, amount: Amount) = up(no, amount, C)

  private trait DC
  private case object D extends DC
  private case object C extends DC

  private def up(no: String, amount: Amount, dc: DC): AccountOperation[Account] = kleisli[Task, AccountRepository, Account] { (repo: AccountRepository) =>
    Task {
      repo.query(no) match {
        case \/-(None) => throw new Exception(s"Account $no does not exist")
        case \/-(Some(a)) => dc match {
          case D => Account.updateBalance(a, -amount).flatMap(repo.store) match { 
            case -\/(err) => throw new Exception(err.list.toList.mkString(","))
            case \/-(a) => a
          }
          case C => Account.updateBalance(a, amount).flatMap(repo.store) match { 
            case -\/(err) => throw new Exception(err.list.toList.mkString(","))
            case \/-(a) => a
          }
        }
        case -\/(err) => throw new Exception(err.list.toList.mkString(","))
      }
    }
  }
}

