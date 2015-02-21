package frdomain.ch5
package free

import scalaz._
import Scalaz._
import Free._

import common._

sealed trait AccountRepoF[+A]
  
case class Query[+A](no: String, onResult: Account => A) extends AccountRepoF[A]
case class Store[+A](account: Account, next: A) extends AccountRepoF[A]
case class Delete[+A](no: String, next: A) extends AccountRepoF[A]

object AccountRepoF {
  implicit val functor: Functor[AccountRepoF] = new Functor[AccountRepoF] {
    def map[A,B](action: AccountRepoF[A])(f: A => B): AccountRepoF[B] = action match {
      case Store(account, next) => Store(account, f(next))
      case Query(no, onResult) => Query(no, onResult andThen f)
      case Delete(no, next) => Delete(no, f(next))
    }
  }
}

trait AccountRepository {
  def store(account: Account): AccountRepo[Unit] = 
    liftF(Store(account, ()))
  
  def query(no: String): AccountRepo[Account] = 
    liftF(Query(no, identity))
  
  def delete(no: String): AccountRepo[Unit] = 
    liftF(Delete(no, ()))

  def update(no: String, f: Account => Account): AccountRepo[Unit] = for {
    a <- query(no)
    _ <- store(f(a))
  } yield ()

  def updateBalance(no: String, amount: Amount, f: (Account, Amount) => Account) = for {
    a <- query(no)
    _ <- store(f(a, amount))
  } yield ()
}
object AccountRepository extends AccountRepository

