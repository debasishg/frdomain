package frdomain.e2e
package domain
package slickdb
package repository

import org.joda.time._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import slick.driver.H2Driver.api._
import slick.jdbc.meta._
import com.github.tototoshi.slick.PostgresJodaSupport._

import scalaz._
import Scalaz._

import Money._

import Accounts._
import Balances._
  
/**
 * Basic interpreter that uses a global mutable Map to store the state
 * of computation
 */
case class AccountRepositoryRelationalDBInterpreter(db: Database) {

  def step[A](action: AccountRepoF[AccountRepo[A]]): Future[AccountRepo[A]] = action match {

    case FindAccount(no, onResult) => 
      db.run(accounts.filter(_.no === no)
                     .result
                     .headOption).map { onResult }

    case StoreAccount(account, pre, onResult) => pre match {
      case Some(p) => 
        p.run(account) match {
          case \/-(a) => db.run(DBIO.seq(accounts += a)).map { _ => onResult(a.some) }  // todo : handle updates
          case -\/(err) => Future.failed(new RuntimeException(err)).map { onResult }
        }
      case None =>
        db.run(DBIO.seq(accounts += account)).map { _ => onResult(account.some) }
    }

    case FindBalance(no, asOn, onResult) => 
      val a = for {
        _ <- accounts.filter(_.no === no).result.headOption
        b <- balances.filter(b => b.accountNo === no && b.asOn === asOn).result.headOption
      } yield (b)

      db.run(a).map { onResult }

    case StoreBalance(curr, pre, onResult) => pre match {
      case Some(p) => 
        p.run(curr) match {
          case \/-(b) => db.run(DBIO.seq(balances += b)).map { _ => onResult(b.some) }  // todo : handle updates
          case -\/(err) => Future.failed(new RuntimeException(err)).map { onResult }
        }
      case None =>
        db.run(DBIO.seq(balances += curr)).map { _ => onResult(curr.some) }
    }
  }

  /**
   * Turns the AccountRepo script into a `Task` that executes it in a mutable setting
   */
  def apply[A](action: AccountRepo[A]): Future[A] = action.runM(step)
}
