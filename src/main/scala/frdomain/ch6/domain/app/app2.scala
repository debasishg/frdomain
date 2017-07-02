package frdomain.ch6
package domain
package app

import scalaz._
import Scalaz._
import repository.interpreter.AccountRepositoryInMemory
import model.common._
import model.{Account, Balance}
import Account._
import frdomain.ch6.domain.service.Valid

object App2 {

  import AccountRepositoryInMemory._
  import scala.concurrent.ExecutionContext.Implicits.global

  val account = checkingAccount("a-123", "debasish ghosh", today.some, None, Balance()).toOption.get
  val c = for {
    b <- Valid(updateBalance(account, 10000))
    c <- store(b)
    d <- balance(c.no)
  } yield d
}
