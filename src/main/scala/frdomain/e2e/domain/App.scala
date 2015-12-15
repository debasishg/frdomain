package frdomain.e2e
package domain

import org.joda.time.DateTime
import scalaz._
import Scalaz._
import Kleisli._

import Common._

object App {
  import AccountServiceBlocking._

  val setup1 =
  for {
    c <- openAccount("c-12345678", "dg", None, ccy = USD)
    _ <- deposit(c.no, 1000, today) 
    _ <- deposit(c.no, 5000, today) 
    _ <- withdraw(c.no, 1500, today) 
    b <- balance(c.no, today)
  } yield b

  val b1 = setup1.run(InMemoryAccountRepository)

  // res1: frdomain.e2e.domain.Common.ErrorOr[frdomain.e2e.domain.AccountBalance] = \/-(AccountBalance(c-12345678,Some(Money(Map(USD -> 4500))),2015-12-14))
  
  // invalid account number & minimum balance
  val setup2 =
  for {
    c <- openAccount("c-123456", "dg", None, ccy = USD, minBalance = -1000)
    _ <- deposit(c.no, 1000, today) 
    _ <- deposit(c.no, 5000, today) 
    _ <- withdraw(c.no, 1500, today) 
    b <- balance(c.no, today)
  } yield b

  val b2 = setup2.run(InMemoryAccountRepository)

  // res0: frdomain.e2e.domain.Common.ErrorOr[frdomain.e2e.domain.AccountBalance] = -\/(Account number (c-123456) has to be of length 10/Minimum balance required (-1000) must be >= 0)
  
  // insufficient funds to withdraw
  val setup3 =
  for {
    c <- openAccount("c-87654321", "dg", None, ccy = USD)
    _ <- deposit(c.no, 1000, today) 
    _ <- deposit(c.no, 5000, today) 
    _ <- withdraw(c.no, 7500, today) 
    b <- balance(c.no, today)
  } yield b

  val b3 = setup3.run(InMemoryAccountRepository)

  // res0: frdomain.e2e.domain.Common.ErrorOr[frdomain.e2e.domain.AccountBalance] = -\/(Insufficient funds (AccountBalance(c-87654321,Some(Money(Map(USD -> 6000))),2015-12-14)) to withdraw (7500))
}
