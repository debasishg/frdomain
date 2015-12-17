package frdomain.e2e
package domain
package repository

import org.joda.time.DateTime
import scalaz._
import Scalaz._
import Free._

sealed trait AccountRepoF[+A]
  
case class FindAccount[+A](no: String, onResult: Account => A) extends AccountRepoF[A]
case class StoreAccount[+A](account: Account, next: A) extends AccountRepoF[A]
case class StoreBalance[+A](balance: AccountBalance, next: A) extends AccountRepoF[A]
case class FindBalance[+A](no: String, asOn: DateTime, onResult: AccountBalance => A) extends AccountRepoF[A]

object AccountRepoF {
  implicit val functor: Functor[AccountRepoF] = new Functor[AccountRepoF] {
    def map[A,B](action: AccountRepoF[A])(f: A => B): AccountRepoF[B] = action match {
      case StoreAccount(account, next) => StoreAccount(account, f(next))
      case StoreBalance(balance, next) => StoreBalance(balance, f(next))
      case FindAccount(no, onResult) => FindAccount(no, onResult andThen f)
      case FindBalance(no, asOn: DateTime, onResult) => FindBalance(no, asOn, onResult andThen f)
    }
  }
}

trait AccountRepository {

  def storeAccount(account: Account): AccountRepo[Unit] = 
    liftF(StoreAccount(account, ()))
  
  def findAccount(no: String): AccountRepo[Account] = 
    liftF(FindAccount(no, identity))
  
  def findBalance(no: String, asOn: DateTime): AccountRepo[AccountBalance] = 
    liftF(FindBalance(no, asOn, identity))
  
  def storeBalance(balance: AccountBalance): AccountRepo[Unit] = 
    liftF(StoreBalance(balance, ()))
  
  def update(no: String, f: Account => Account): AccountRepo[Unit] = for {
    a <- findAccount(no)
    _ <- storeAccount(f(a))
  } yield ()

  def updateBalance(no: String, amount: Amount, on: DateTime) = for {
    b <- findBalance(no, on)
    _ <- storeBalance(b + amount)
  } yield (())
}

object AccountRepository extends AccountRepository
