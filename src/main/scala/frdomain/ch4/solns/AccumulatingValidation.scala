package frdomain.ch4
package solns

import java.util.{ Date, Calendar }
import util.{ Try, Success, Failure }
import scalaz._
import Scalaz._

/**
 *
 * Exercise 4.2: Accumulating Validation Errors (Applicatively)
 */
object AccumulatingValidation {
  // utilities
  object common {
    type Amount = BigDecimal
  
    def today = Calendar.getInstance.getTime
  }
  
  import common._
  
  case class Balance(amount: Amount = 0)
  
  // contract for Account
  sealed trait Account {
    def no: String
    def name: String
    def dateOfOpen: Option[Date]
    def dateOfClose: Option[Date]
    def balance: Balance
  }
  
  // checking account
  final case class CheckingAccount (no: String, name: String,
    dateOfOpen: Option[Date], dateOfClose: Option[Date] = None, balance: Balance = Balance()) extends Account
  
  // savings account
  final case class SavingsAccount (no: String, name: String, rateOfInterest: Amount, 
    dateOfOpen: Option[Date], dateOfClose: Option[Date] = None, balance: Balance = Balance()) extends Account
  
  object Account {
  
    // validation functions : each returns a ValidationNel[String, _]
    private def validateAccountNo(no: String): ValidationNel[String, String] = 
      if (no.isEmpty || no.size < 10) 
        s"Account No has to be at least 10 characters long: found $no".failureNel[String] 
      else no.successNel[String]
  
    private def validateOpenCloseDate(od: Date, cd: Option[Date]): ValidationNel[String, (Option[Date], Option[Date])] = cd.map { c => 
      if (c before od) 
        s"Close date [$c] cannot be earlier than open date [$od]".failureNel[(Option[Date], Option[Date])]
      else (od.some, cd).successNel[String]
    }.getOrElse { (od.some, cd).successNel[String] }
  
    private def validateRate(rate: BigDecimal): ValidationNel[String, BigDecimal] =
      if (rate <= BigDecimal(0)) s"Interest rate $rate must be > 0".failureNel[BigDecimal] 
      else rate.successNel[String]
  
    // the validations are invoked applicatively using the applicative builder
    // Validation[E, A] in scalaz defines an Applicative by virtue of which the errors
    // on the left side are accumulated through a SemiGroup defined on E
    def savingsAccount(no: String, name: String, rate: BigDecimal, openDate: Option[Date], 
      closeDate: Option[Date], balance: Balance): ValidationNel[String, Account] = { 
  
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
}  

/**
Welcome to Scala 2.12.0 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_91).
Type in expressions for evaluation. Or try :help.

scala> import frdomain.ch4.solns._
import frdomain.ch4.solns._

scala> import AccumulatingValidation._
import AccumulatingValidation._

scala> import Account._
import Account._

scala> savingsAccount("1234", "john", BigDecimal(0.1), None, None, Balance())
res0: scalaz.ValidationNel[String,frdomain.ch4.solns.AccumulatingValidation.Account] = Failure(NonEmpty[Account No has to be at least 10 characters long: found 1234])

scala> savingsAccount("1234", "john", BigDecimal(-0.1), None, None, Balance())
res1: scalaz.ValidationNel[String,frdomain.ch4.solns.AccumulatingValidation.Account] = Failure(NonEmpty[Account No has to be at least 10 characters long: found 1234,Interest rate -0.1 must be > 0])

scala> savingsAccount("123456789000", "john", BigDecimal(0.1), None, None, Balance())
res2: scalaz.ValidationNel[String,frdomain.ch4.solns.AccumulatingValidation.Account] = Success(SavingsAccount(123456789000,john,0.1,Some(Sun Dec 25 20:35:35 IST 2016),None,Balance(0)))
**/
