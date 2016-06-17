package frdomain.ch5
package dsl

import common._
import cats.data.Xor
import cats.{~>, Id}
import freek._

trait AccountRepository {
  sealed trait AccountRepo[A]
  
  case class Query(no: String) extends AccountRepo[Xor[String, Account]]
  case class Store(account: Account) extends AccountRepo[Xor[String, Account]]
  case class Delete(no: String) extends AccountRepo[Xor[String, Unit]]

  def query(no: String) = Query(no)
  def store(account: Account) = Store(account)
  def delete(no: String) = Delete(no)

/*
  type PR = AccountRepo :|: FXNil
  type P = Xor[String, ?] :&: Bulb

  def update(no: String, f: Account => Account) = for {
    a <- query(no).freek[PR].onionT[P]
    s <- store(f(a)).freek[PR].onionT[P]
  } yield s

  def updateBalance(no: String, amount: Amount, f: (Account, Amount) => Account) = for {
    a <- query(no).freek[PR].onionT[P]
    _ <- store(f(a, amount)).freek[PR].onionT[P]
  } yield ()
*/

  val accountRepositoryInterpreter = new (AccountRepo ~> cats.Id) {
    val table = collection.mutable.Map.empty[String, Account]

    def apply[A](dsl: AccountRepo[A]) = dsl match {
      case Query(no) => table.get(no) match {
        case Some(a) => Xor.right(a)
        case None => Xor.left(s"Account no $no not found")
      }
      case Store(a) => 
        table += ((a.no, a))
        Xor.right(a)
      case Delete(no) => 
        table -= no
        Xor.right(())
    }
  }
}
