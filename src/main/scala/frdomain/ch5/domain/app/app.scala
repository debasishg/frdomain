package frdomain.ch5
package domain
package app

import scalaz._
import Scalaz._
import Kleisli._

import service.interpreter.{ AccountService, InterestPostingService, ReportingService }
import repository.interpreter.AccountRepositoryInMemory
import service.{ Checking, Savings }
import model.common._
import model.Account

object App {

  import AccountService._
  import InterestPostingService._
  import ReportingService._

  def postTransactions(a: Account, cr: Amount, db: Amount) = 
    for { 
      _ <- credit(a.no, cr)
      d <- debit(a.no, db)
    } yield d

  def composite(no: String, name: String, cr: Amount, db: Amount) = (for {
    a <- open(no, name, BigDecimal(0.4).some, None, Savings)
    t <- postTransactions(a, cr, db)
  } yield t) >=> computeInterest >=> computeTax

  val x = composite("a-123", "debasish ghosh", 10000, 2000)(AccountRepositoryInMemory)

  val opens = 
    for {
      _ <- open("a1234", "a1name", None, None, Checking)
      _ <- open("a2345", "a2name", None, None, Checking)
      _ <- open("a3456", "a3name", BigDecimal(5.8).some, None, Savings)
      _ <- open("a4567", "a4name", None, None, Checking)
      _ <- open("a5678", "a5name", BigDecimal(2.3).some, None, Savings)
    } yield (())

  val credits = 
    for {
      _ <- credit("a1234", 1000)
      _ <- credit("a2345", 2000)
      _ <- credit("a3456", 3000)
      _ <- credit("a4567", 4000)
    } yield (())

  val c = for {
    _ <- opens
    _ <- credits
    a <- balanceByAccount
  } yield a

  val y = c(AccountRepositoryInMemory)
}
