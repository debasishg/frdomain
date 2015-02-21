package frdomain.ch5
package typeclass

import java.util.{ Date, Calendar }
import scala.util.{ Try, Success }

trait Show[T] {
  def shows(t: T): Try[String]
}

object common {
  type Amount = BigDecimal

  val today = Calendar.getInstance.getTime
}

import common._

case class Balance(amount: Amount = 0)

case class Account(no: String, name: String, dateOfOpening: Date = today, dateOfClosing: Option[Date] = None, 
  balance: Balance = Balance(0)) extends Show[Account] {
  def shows(a: Account) = Success(a.toString)
}

case class Account1(no: String, name: String, dateOfOpening: Date = today, dateOfClosing: Option[Date] = None, 
  balance: Balance = Balance(0))

trait ShowProtocol {
  implicit val showAccount: Show[Account1]
}

trait DomainShowProtocol extends ShowProtocol {
  implicit val showAccount: Show[Account1] = new Show[Account1] { def shows(a: Account1) = Success(a.toString) }
}

object DomainShowProtocol extends DomainShowProtocol

object Reporting {
  def report[T: Show](as: Seq[T]) = as.map(implicitly[Show[T]].shows(_))
}

object App {
  import DomainShowProtocol._
  import Reporting._

  val as = Seq(
    Account1("a-1", "name-1"),
    Account1("a-2", "name-2"),
    Account1("a-3", "name-3"),
    Account1("a-4", "name-4")
  )
}
