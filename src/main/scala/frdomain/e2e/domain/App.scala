package frdomain.e2e
package domain

import org.joda.time.DateTime
import scalaz._
import Scalaz._

import service._

object App {
  import AccountService._

  val setup1 =
  for {
    c <- openAccount("c-12345678", "dg", None, ccy = USD)
    no = "c-12345678"
    _ <- deposit(no, 1000, today) 
    _ <- deposit(no, 5000, today) 
    _ <- withdraw(no, 1500, today) 
    b <- balance(no, today)
  } yield b

  // val b1 = setup1.unsafePerformSync

  // res1: frdomain.e2e.domain.Common.ErrorOr[frdomain.e2e.domain.AccountBalance] = \/-(AccountBalance(c-12345678,Some(Money(Map(USD -> 4500))),2015-12-14))
  
  // invalid account number & minimum balance
  val setup2 =
  for {
    c <- openAccount("c-123456", "dg", None, ccy = USD, minBalance = -1000)
    no = "c-123456"
    _ <- deposit(no, 1000, today) 
    _ <- deposit(no, 5000, today) 
    _ <- withdraw(no, 1500, today) 
    b <- balance(no, today)
  } yield b

  // val b2 = setup2.unsafePerformSync

  // res0: frdomain.e2e.domain.Common.ErrorOr[frdomain.e2e.domain.AccountBalance] = -\/(Account number (c-123456) has to be of length 10/Minimum balance required (-1000) must be >= 0)
  
  // insufficient funds to withdraw
  val setup3 =
  for {
    c <- openAccount("c-87654321", "dg", None, ccy = USD)
    no = "c-87654321"
    _ <- deposit(no, 1000, today) 
    _ <- deposit(no, 5000, today) 
    _ <- withdraw(no, 7500, today) 
    b <- balance(no, today)
  } yield b

  // val b3 = setup3.unsafePerformSync

  // res0: frdomain.e2e.domain.Common.ErrorOr[frdomain.e2e.domain.AccountBalance] = -\/(Insufficient funds (AccountBalance(c-87654321,Some(Money(Map(USD -> 6000))),2015-12-14)) to withdraw (7500))

  // non existent account number
  val setup4 =
  for {
    _ <- deposit("a-1234", 1000, today) 
    _ <- deposit("a-1234", 5000, today) 
    _ <- withdraw("a-1234", 7500, today) 
    b <- balance("a-1234", today)
  } yield b

  // val b4 = setup4.unsafePerformSync

  // res1: frdomain.e2e.domain.Common.ErrorOr[frdomain.e2e.domain.AccountBalance] = -\/(Account with no a-1234 does not exist)
}
