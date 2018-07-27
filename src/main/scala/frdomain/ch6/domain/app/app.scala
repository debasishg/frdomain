package frdomain.ch6
package domain
package app

import cats._
import cats.data._
import cats.implicits._
import cats.instances.all._

import service.interpreter.{ AccountService, InterestPostingService, ReportingService }
import repository.interpreter.AccountRepositoryInMemory
import service.{ Checking, Savings }
import common._
import model.Account

object App {

  import AccountService._
  import InterestPostingService._
  import ReportingService._

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

  // y.value.unsafeRunSync
  //
  // scala> res0.unsafeRunAsync {
  //      |   case Left(s) => println(s)
  //      |   case Right(s) => println(s)
  //      | }
  // Right(Stream((a2345,2000), ?))
}
