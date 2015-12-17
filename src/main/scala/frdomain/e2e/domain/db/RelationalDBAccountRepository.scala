package frdomain.e2e
package domain
package db

import org.joda.time._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import slick.driver.H2Driver.api._
import slick.jdbc.meta._
import com.github.tototoshi.slick.PostgresJodaSupport._

import scalaz._
import Scalaz._

import Common._
import Money._

import Accounts._
import Balances._

/**
 * In memory implementation of `AccountRepository` based on `Disjunction` monad.
 */
class RelationalDBAccountRepository extends AccountRepository[Future] {

  val M: Monad[Future] = Monad[Future]

  private val db = Database.forConfig("h2mem1")

  def query(accountNo: String): Future[Option[Account]] = db.run(accounts.filter(_.no === accountNo).result.headOption)

  def store(a: Account): Future[Account] = ???

  def update(a: Account): Future[Account] = ???

  def accountsOpenedOn(date: DateTime): Future[List[Account]] = ???

  def updateBalance(account: Account, amount: Amount, asOn: DateTime): Future[AccountBalance] = ???

  def balance(account: Account, asOn: DateTime): Future[AccountBalance] = ???
}

object RelationalDBAccountRepository extends RelationalDBAccountRepository

