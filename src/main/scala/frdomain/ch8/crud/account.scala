package frdomain.ch8
package crud

import java.sql.Timestamp
import slick.driver.H2Driver.api._

case class Account(id: Option[Long], 
  no: String, 
  name: String, 
  address: String, 
  dateOfOpening: Timestamp,
  dateOfClosing: Option[Timestamp]
)

case class Balance(id: Option[Long],
  account: Long,
  asOnDate: Timestamp,
  amount: BigDecimal
)

import Accounts._
import Balances._

class Accounts(tag: Tag) extends Table[Account](tag, "accounts") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def no = column[String]("no")
  def name = column[String]("name")
  def address = column[String]("address")
  def dateOfOpening = column[Timestamp]("date_of_opening")
  def dateOfClosing = column[Option[Timestamp]]("date_of_closing")

  def * = (id.?, no, name, address, dateOfOpening, dateOfClosing) <> (Account.tupled, Account.unapply)
  def noIdx = index("idx_no", no, unique = true)
}

object Accounts {
  val accounts = TableQuery[Accounts]
}

class Balances(tag: Tag) extends Table[Balance](tag, "balances") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def account = column[Long]("account")
  def asOnDate = column[Timestamp]("as_on_date")
  def amount = column[BigDecimal]("amount")

  def * = (id.?, account, asOnDate, amount) <> (Balance.tupled, Balance.unapply)
  def accountfk = foreignKey("ACCOUNT_FK", account, accounts)(_.id, onUpdate=ForeignKeyAction.Cascade, onDelete=ForeignKeyAction.Cascade)
}

object Balances {
  val balances = TableQuery[Balances]
}
