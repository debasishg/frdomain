package frdomain.ch6
package domain
package repository

import java.util.Date

import frdomain.ch6.domain.model.{Account, Balance}
import frdomain.ch6.domain.service.Valid

import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.NonEmptyList
import scalaz.Scalaz._

trait AccountRepository {
  def query(no: String): Valid[Option[Account]]
  def store(a: Account): Valid[Account]
  def query(openedOn: Date): Valid[Seq[Account]]
  def all: Valid[Seq[Account]]

  def balance(no: String): Valid[Balance] = query(no).flatMap { maybeAcc =>
    Valid {
      maybeAcc match {
        case Some(acc) => acc.balance.right
        case None => NonEmptyList(s"No account exists with no $no").left
      }
    }
  }
}
