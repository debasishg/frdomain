package frdomain.ch5
package free

import scalaz.Free

import common._

sealed trait AccountRepoF[A]
  
case class Query(no: String) extends AccountRepoF[Account]
case class Store(account: Account) extends AccountRepoF[Unit]
case class Delete(no: String) extends AccountRepoF[Unit]

trait AccountRepository {
  def store(account: Account): AccountRepo[Unit] = 
    Free.liftF(Store(account))
  
  def query(no: String): AccountRepo[Account] =
    Free.liftF(Query(no))
  
  def delete(no: String): AccountRepo[Unit] = 
    Free.liftF(Delete(no))

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

