package frdomain.e2e
package domain
package repository

import slick.driver.H2Driver.api._
import com.github.tototoshi.slick.PostgresJodaSupport._
import org.joda.time.DateTime

import Accounts._

object Custom {
  implicit val ccyColumnType = MappedColumnType.base[Currency, String](
    { c => 
        c match {
          case USD => "USD"
          case INR => "INR"
          case AUD => "AUD"
          case JPY => "JPY"
          case GBP => "GBP"
          case EUR => "EUR"
        }
     },
     { s =>
       s match {
          case "USD" => USD
          case "INR" => INR
          case "AUD" => AUD
          case "JPY" => JPY
          case "GBP" => GBP
          case "EUR" => EUR
       }
     }
  )
}

import Custom._

class Accounts(tag: Tag) extends Table[Account](tag, "accounts") {
  def no = column[String]("no", O.PrimaryKey)
  def name = column[String]("name")
  def rateOfInterest = column[Option[BigDecimal]]("rate_of_interest")
  def dateOpened = column[DateTime]("date_opened")
  def dateClosed = column[Option[DateTime]]("date_closed")
  def currency = column[Currency]("currency")
  def minBalanceRequired = column[BigDecimal]("min_balance_required")

  def * = (no, name, rateOfInterest, dateOpened, dateClosed, currency, minBalanceRequired) <> ((Account.apply _).tupled, Account.unapply)
}

object Accounts {
  val accounts = TableQuery[Accounts]
}
