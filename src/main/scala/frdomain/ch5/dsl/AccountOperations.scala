package frdomain.ch5
package dsl

import org.joda.time.DateTime
import freek._

trait AccountOperations {
  import common._
  import Account._
  import cats.{~>, Id}
  import cats.data.{ NonEmptyList, Xor }

  sealed trait AccountOps[A]
  case class Open(no: String, name: String, openDate: DateTime) extends AccountOps[AccountValidationNel[Account]]
  case class Transact(a: Account, amount: Amount, asOf: DateTime) extends AccountOps[AccountValidationNel[Account]]
  case class Close(a: Account, closeDate: DateTime) extends AccountOps[AccountValidationNel[Account]]

  def open(no: String, name: String, openDate: DateTime) = Open(no, name, openDate)
  def transact(a: Account, amount: Amount, asOf: DateTime) = Transact(a, amount, asOf)
  def close(a: Account, closeDate: DateTime) = Close(a, closeDate)

  val accountOperationsInterpreter = new (AccountOps ~> cats.Id) {
    def apply[A](dsl: AccountOps[A]) = dsl match {
      case Open(no, name, openDate) => Account.open(no, name, openDate)
      case Transact(a, amount, asOf) => Account.transact(a, amount, asOf)
      case Close(a, closeDate) => Account.close(a, closeDate)
    }
  }
}
