package frdomain.ch5
package domain
package service
package interpreter

import scalaz._
import Scalaz._
import Kleisli._

import model.{ Account, Balance }
import model.common._

class InterestPostingServiceInterpreter extends InterestPostingService[Account, Amount] {
  def computeInterest = kleisli[Valid, Account, Amount] { (account: Account) =>
    if (account.dateOfClose isDefined) NonEmptyList(s"Account ${account.no} is closed").left
    else Account.rate(account).map { r =>
      val a = account.balance.amount
      a + a * r
    }.getOrElse(BigDecimal(0)).right
  }

  def computeTax = kleisli[Valid, Amount, Amount] { amount: Amount =>
    (amount * 0.1).right
  }
}

object InterestPostingService extends InterestPostingServiceInterpreter

