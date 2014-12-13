package frdomain.ch3
package repository

import java.util.Date
import util.{ Try, Success, Failure }

trait AccountRepository extends Repository[Account, String] {
  def query(no: String): Try[Option[Account]] = ???
  def store(a: Account): Try[Account] = ???
  def balance(no: String): Try[Balance] = query(no) match {
    case Success(Some(a)) => Success(a.balance)
    case Success(None) => Failure(new Exception(s"No account exists with no $no"))
    case Failure(ex) => Failure(ex)
  }
  def query(openedOn: Date): Try[Seq[Account]] = ???
}

