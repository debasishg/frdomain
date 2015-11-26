package frdomain.ch8
package crud

import java.sql.Timestamp
import slick.driver.H2Driver.api._
import slick.jdbc.meta._
import slick.backend.DatabasePublisher

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import Accounts._
import Balances._

trait Queries {
  final val timeout: Duration = 2 seconds

  def getBalance(db: Database, accountNo: String, fromDate: Timestamp, toDate: Timestamp) = {
    val action = for {
      a <- accounts.filter(_.no === accountNo)
      b <- balances if a.id === b.account && b.asOnDate >= fromDate && b.asOnDate <= toDate
    } yield b
    Await.result(db.run(action.result.asTry), timeout)
  }

  type BalanceRecord = (String, Timestamp, Timestamp, Option[BigDecimal])
  def getTotalBalanceByAccount(db: Database, fromDate: Timestamp, toDate: Timestamp): DatabasePublisher[BalanceRecord] = {
    val action = (for {
      a <- accounts
      b <- balances if a.id === b.account && b.asOnDate >= fromDate && b.asOnDate <= toDate
    } yield (a.no, b)).groupBy(_._1).map { case (no, bs) => 
      (no, fromDate, toDate, bs.map(_._2.amount).sum) 
    }
    db.stream(action.result)
  }
}
