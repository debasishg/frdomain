package frdomain.ch4
package patterns

import java.util.{ Date, Calendar }
import util.{ Try, Success, Failure }

sealed trait Currency
case object USD extends Currency
case object JPY extends Currency
case object AUD extends Currency
case object INR extends Currency

object common {
  type Amount = BigDecimal
  val today = Calendar.getInstance.getTime
}

import common._
import Monoid._

case class Money(m: Map[Currency, Amount]) {
  def toBaseCurrency: Amount = ???
}

case class Balance(amount: Money = zeroMoney)

sealed trait Account {
  def no: String
  def name: String
  def dateOfOpen: Option[Date]
  def dateOfClose: Option[Date]
  def balance: Balance
}

final case class CheckingAccount (no: String, name: String,
  dateOfOpen: Option[Date], dateOfClose: Option[Date] = None, balance: Balance = Balance()) extends Account

final case class SavingsAccount (no: String, name: String, rateOfInterest: Amount, 
  dateOfOpen: Option[Date], dateOfClose: Option[Date] = None, balance: Balance = Balance()) extends Account

object Account {

  /**
   * Uses Applicative instance of ValidationNEL which accumulates errors using Semigroup
   */
  object FailSlowApplicative {
    import scalaz._
    import syntax.apply._, syntax.std.option._, syntax.validation._

    private def validateAccountNo(no: String) = 
      if (no.isEmpty || no.size < 5) s"Account No has to be at least 5 characters long: found $no".failureNel[String] 
      else no.successNel[String]
  
    private def validateOpenCloseDate(od: Date, cd: Option[Date]) = cd.map { c => 
      if (c before od) s"Close date [$c] cannot be earlier than open date [$od]".failureNel[(Option[Date], Option[Date])]
      else (od.some, cd).successNel[String]
    }.getOrElse { (od.some, cd).successNel[String] }
  
    private def validateRate(rate: BigDecimal) =
      if (rate <= BigDecimal(0)) s"Interest rate $rate must be > 0".failureNel[BigDecimal]
      else rate.successNel[String]
  
    def checkingAccount(no: String, name: String, openDate: Option[Date], closeDate: Option[Date], 
      balance: Balance): Validation[NonEmptyList[String], Account] = { 
  
      val od = openDate.getOrElse(today)
  
      (
        validateAccountNo(no) |@| 
        validateOpenCloseDate(openDate.getOrElse(today), closeDate)
      ) { (n, d) =>
        CheckingAccount(n, name, d._1, d._2, balance)
      }
    }
  
    def savingsAccount(no: String, name: String, rate: BigDecimal, openDate: Option[Date], 
      closeDate: Option[Date], balance: Balance): Validation[NonEmptyList[String], Account] = { 
  
      val od = openDate.getOrElse(today)
  
      (
        validateAccountNo(no) |@| 
        validateOpenCloseDate(openDate.getOrElse(today), closeDate) |@|
        validateRate(rate)
      ) { (n, d, r) =>
        SavingsAccount(n, name, r, d._1, d._2, balance)
      }
    }
  }

  /**
   * Uses default Applicative instance of Either which does not accumulate errors. But true to applicative
   * nature it will invoke validations for all the items. So not truly fail fast - fail slow but non-accumulating.
   */
  object FailFastApplicative {
    import scalaz._
    import syntax.apply._, syntax.std.option._, std.either._

    private def validateAccountNo(no: String): Either[String, String] =
      if (no.isEmpty || no.size < 5) Left(s"Account No has to be at least 5 characters long: found $no")
      else Right(no)
  
    private def validateOpenCloseDate(od: Date, cd: Date): Either[String, (Date, Date)] = 
      if (cd before od) Left(s"Close date [$cd] cannot be earlier than open date [$od]")
      else Right((od, cd))
  
    private def validateRate(rate: BigDecimal): Either[String, BigDecimal] =
      if (rate <= BigDecimal(0)) Left(s"Interest rate $rate must be > 0")
      else Right(rate)
  
    def checkingAccount(no: String, name: String, openDate: Option[Date], closeDate: Option[Date], 
      balance: Balance): Either[String, Account] = { 
  
      val cd = closeDate.getOrElse(today)
      val od = openDate.getOrElse(today)
  
      (validateAccountNo(no) |@| validateOpenCloseDate(od, cd)) { (n, d) =>
        CheckingAccount(n, name, d._1.some, d._2.some, balance)
      }
    }
  
    def savingsAccount(no: String, name: String, rate: BigDecimal, openDate: Option[Date], 
      closeDate: Option[Date], balance: Balance): Either[String, Account] = { 
  
      val cd = closeDate.getOrElse(today)
      val od = openDate.getOrElse(today)
  
      (validateAccountNo(no) |@| validateOpenCloseDate(od, cd) |@| validateRate(rate)) { (n, d, r) =>
        SavingsAccount(n, name, r, d._1.some, d._2.some, balance)
      }
    }
  }

  /**
   * Uses monad instance of Either which does not accumulate errors. True fail-fast.
   */
  object FailFastMonad {
    import scalaz._
    import syntax.std.option._

    private def validateAccountNo(no: String): Either[String, String] =
      if (no.isEmpty || no.size < 5) Left(s"Account No has to be at least 5 characters long: found $no")
      else Right(no)
  
    private def validateOpenCloseDate(od: Date, cd: Date): Either[String, (Date, Date)] =
      if (cd before od) Left(s"Close date [$cd] cannot be earlier than open date [$od]")
      else Right((od, cd))
  
    private def validateRate(rate: BigDecimal): Either[String, BigDecimal] =
      if (rate <= BigDecimal(0)) Left(s"Interest rate $rate must be > 0")
      else Right(rate)
  
    def checkingAccount(no: String, name: String, openDate: Option[Date], closeDate: Option[Date], 
      balance: Balance): Either[String, Account] = { 
  
      val cd = closeDate.getOrElse(today)
      val od = openDate.getOrElse(today)
  
      for {
        n <- validateAccountNo(no).right
        d <- validateOpenCloseDate(od, cd).right
      } yield CheckingAccount(n, name, d._1.some, d._2.some, balance)
    }
  
    def savingsAccount(no: String, name: String, rate: BigDecimal, openDate: Option[Date], 
      closeDate: Option[Date], balance: Balance): Either[String, Account] = { 
  
      val cd = closeDate.getOrElse(today)
      val od = openDate.getOrElse(today)
  
      for {
        n <- validateAccountNo(no).right
        d <- validateOpenCloseDate(od, cd).right
        r <- validateRate(rate).right
      } yield SavingsAccount(n, name, r, d._1.some, d._2.some, balance)
    }
  }
}

import scalaz._
import Scalaz._
import frdomain.ch3.repository._

object AccountNumberGeneration {
  final class Generator(rep: AccountRepository) {
    val no: String = scala.util.Random.nextString(10)
    def exists: Boolean = rep.query(no) match {
      case Success(Some(a)) => true
      case _ => false
    }
  }

  def generate(start: Generator, r: AccountRepository): Generator = {
    val StateGen = StateT.stateMonad[Generator]
    import StateGen._ // includes whileM_, gets, put
    whileM_(gets(_.exists), modify(_ => new Generator(r))).exec(start)
  }
}


