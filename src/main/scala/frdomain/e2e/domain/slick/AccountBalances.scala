package frdomain.e2e
package domain
package slickdb
package repository

import slick.driver.H2Driver.api._
import com.github.tototoshi.slick.PostgresJodaSupport._
import org.joda.time._

import Balances._
import Accounts._

import Custom._

class Balances(tag: Tag) extends Table[AccountBalance](tag, "balances") {
  def id = column[Option[Long]]("id", O.PrimaryKey, O.AutoInc)
  def accountNo = column[String]("account")
  def balance = column[BigDecimal]("balance")
  def currency = column[Currency]("currency")
  def asOn = column[LocalDate]("as_on")

  def * = (id, accountNo, balance, currency, asOn) <> ((AccountBalance.apply _).tupled, AccountBalance.unapply)
  def accountfk = foreignKey("ACCOUNT_FK", accountNo, accounts)(_.no, onUpdate=ForeignKeyAction.Cascade, onDelete=ForeignKeyAction.Cascade)
}

object Balances {
  val balances = TableQuery[Balances]
}

