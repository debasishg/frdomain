package frdomain.e2e
package domain
package slickdb
package repository

import org.joda.time.{ DateTime, LocalDate }
import scalaz._
import Scalaz._
import OptionT._
import Free._

sealed trait AccountRepoF[+A]
  
case class FindAccount[+A](no: String, onResult: Option[Account] => A) extends AccountRepoF[A]
case class StoreAccount[+A](account: Account, prehook: Option[AccountAction], next: Option[Account] => A) extends AccountRepoF[A]

case class StoreBalance[+A](balance: AccountBalance, prehook: Option[AccountBalanceAction], 
  next: Option[AccountBalance] => A) extends AccountRepoF[A]

case class FindBalance[+A](no: String, asOn: LocalDate, onResult: Option[AccountBalance] => A) extends AccountRepoF[A]

object AccountRepoF {
  implicit val functor: Functor[AccountRepoF] = new Functor[AccountRepoF] {
    def map[A,B](action: AccountRepoF[A])(f: A => B): AccountRepoF[B] = action match {
      case StoreAccount(account, pre, onResult) => StoreAccount(account, pre, onResult andThen f)
      case StoreBalance(balance, pre, onResult) => StoreBalance(balance, pre, onResult andThen f)
      case FindAccount(no, onResult) => FindAccount(no, onResult andThen f)
      case FindBalance(no, asOn, onResult) => FindBalance(no, asOn, onResult andThen f)
    }
  }
}

trait AccountRepository {

  def storeAccount(account: Account, prehook: Option[AccountAction] = None): AccountRepo[Option[Account]] = 
    liftF(StoreAccount(account, prehook, identity))
  
  def findAccount(no: String): AccountRepo[Option[Account]] = 
    liftF(FindAccount(no, identity))
  
  def findBalance(no: String, asOn: LocalDate): AccountRepo[Option[AccountBalance]] = 
    liftF(FindBalance(no, asOn, identity))
  
  def storeBalance(balance: AccountBalance, prehook: Option[AccountBalanceAction]): AccountRepo[Option[AccountBalance]] = 
    liftF(StoreBalance(balance, prehook, identity))
  
  def update(no: String, f: Option[AccountAction]): AccountRepo[Option[Account]] = (for {
    a <- optionT(findAccount(no))
    s <- optionT(storeAccount(a, f))
  } yield s).run

  def updateBalance(no: String, on: LocalDate, f: Option[AccountBalanceAction]) = (for {
    b <- optionT(findBalance(no, on))
    s <- optionT(storeBalance(b, f))
  } yield s).run
}

object AccountRepository extends AccountRepository

